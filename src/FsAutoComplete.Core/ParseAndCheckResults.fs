namespace FsAutoComplete

open System
open System.IO
open System.Text
open FSharp.Compiler.SourceCodeServices
open Utils
open FSharp.Compiler.Range
open FSharp.Compiler
open FSharp.Compiler.Text
open ProjectSystem
open FsAutoComplete.Logging
open FsAutoComplete.Utils
open FSharp.UMX
open FSharp.Compiler.SyntaxTree
open FsToolkit.ErrorHandling

[<RequireQualifiedAccess>]
type FindDeclarationResult =
    | ExternalDeclaration of Decompiler.ExternalContentPosition
    | Range of FSharp.Compiler.Range.range
    /// The declaration refers to a file.
    | File of string

module private Helpers =
  let isStaticArgTip (pos: pos) (text: ISourceText) =
    let parenLine, parenCol = pos.Line, pos.Column
    if parenLine >= text.Length
    then Error "line outside text"
    else
      let lineText = text.GetLineString parenLine
      Ok(parenCol < lineText.Length && lineText.[parenCol] = '<')

  let oneColBefore (pos: pos) =
    mkPos (pos.Line) (pos.Column - 1)

  let oneColAfter (pos: pos) =
    mkPos (pos.Line) (pos.Column + 1)


type ParseAndCheckResults
    (
        parseResults: FSharpParseFileResults,
        checkResults: FSharpCheckFileResults,
        entityCache: EntityCache
    ) =

  let logger = LogProvider.getLoggerByName "ParseAndCheckResults"

  member __.TryGetMethodOverrides (lines: ISourceText) (caretLinePos: pos) (triggeringChar: char option) (caretLineColumn: int) (caretPosition: int) = asyncResult {
    /// ensures that a pos is 'capped' to the end of the document in the worst case
    let posToLinePosition (pos: pos) =
      let posTup = pos.Line, pos.Column
      let (lastLine, lastColumn) as lastPosInDocument = lines.GetLastCharacterPosition()
      if lastPosInDocument > posTup then pos else mkPos lastLine lastColumn

    // Get the parameter locations
    let paramLocations = parseResults.FindNoteworthyParamInfoLocations caretLinePos
    match paramLocations with
    | None ->
      return! ResultOrString.Error "Could not find parameter locations"
    | Some paramLocations ->

      let names = paramLocations.LongId
      let lidEnd = paramLocations.LongIdEndLocation
      let! methodGroup = checkResults.GetMethods(lidEnd.Line, lidEnd.Column, "", Some names)
      let methods = methodGroup.Methods
      do! Result.guard (methods.Length > 0 && not (methodGroup.MethodName.EndsWith "> )")) "no matching methods in method group"

      let! isStaticArgTip = Helpers.isStaticArgTip paramLocations.OpenParenLocation lines

      let filteredMethods =
        [|
            for m in methods do
                if (isStaticArgTip && m.StaticParameters.Length > 0) ||
                    (not isStaticArgTip && m.HasParameters) then   // need to distinguish TP<...>(...)  angle brackets tip from parens tip
                    m
        |]

      do! Result.guard (filteredMethods.Length > 0) "no methods found"

      let startPos = posToLinePosition paramLocations.LongIdStartLocation
      let endPos =
        let lastPos = Array.last paramLocations.TupleEndLocations |> posToLinePosition
        if paramLocations.IsThereACloseParen then Helpers.oneColBefore lastPos else lastPos

      let startOfArgs = paramLocations.OpenParenLocation |> posToLinePosition |> Helpers.oneColAfter
      let tupleEnds =
        [|
          startOfArgs
          for i in 0..paramLocations.TupleEndLocations.Length-2 do
            posToLinePosition paramLocations.TupleEndLocations.[i]
          endPos
        |]

      match triggeringChar with
      | Some ('<' | '(' | ',') when not (tupleEnds |> Array.exists (fun pos -> pos.Column = caretLineColumn)) ->
        return None
      | _ ->
        // Compute the argument index by working out where the caret is between the various commas.
        let argumentIndex =
          let computedTextSpans =
              tupleEnds
              |> Array.pairwise
              |> Array.map (fun (startPos, endPos) -> mkRange "foo" startPos endPos )

          match (computedTextSpans |> Array.tryFindIndex (fun t -> rangeContainsPos t caretPosition)) with
          | None ->
              // Because 'TextSpan.Contains' only succeeds if 'TextSpan.Start <= caretPosition < TextSpan.End' is true,
              // we need to check if the caret is at the very last position in the TextSpan.
              //
              // We default to 0, which is the first argument, if the caret position was nowhere to be found.
              if computedTextSpans.[computedTextSpans.Length-1].End = caretPosition then
                  computedTextSpans.Length-1
              else 0
          | Some n -> n

        let argumentCount =
          match paramLocations.TupleEndLocations.Length with
          | 1 when caretLinePos.Character = startOfArgs.Character -> 0  // count "WriteLine(" as zero arguments
          | n -> n

        // Compute the current argument name if it is named.
        let argumentName =
          if argumentIndex < paramLocations.NamedParamNames.Length then
            paramLocations.NamedParamNames.[argumentIndex]
          else
            None

        let results = ()
        return Some results
  }

  member __.TryFindDeclaration (pos: pos) (lineStr: LineStr) = async {
    // try find identifier first
    let! identResult = __.TryFindIdentifierDeclaration pos lineStr
    match identResult with
    | Ok r -> return Ok r
    | Error identErr ->
    // then #load directive
    let! loadResult = __.TryFindLoadDirectiveSource pos lineStr
    match loadResult with
    | Ok r -> return Ok r
    | Error _ -> return Error identErr
  }

  member __.TryFindLoadDirectiveSource (pos: pos) (lineStr: LineStr) = async {
    let tryGetFullPath fileName =
      try
        // use the parsed file name directory as base path
        let basePath = Path.GetDirectoryName(__.FileName)
        Some (Path.Combine(basePath, fileName))
      with
      | :? ArgumentException -> None
      | :? PathTooLongException -> None
      | :? NotSupportedException -> None

    let result =
      InteractiveDirectives.tryParseLoad lineStr pos.Column
      |> Option.bind tryGetFullPath

    match result with
    | Some file -> return Ok (FindDeclarationResult.File file)
    | None -> return Error "load directive not recognized"
  }

  member __.TryFindIdentifierDeclaration (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return ResultOrString.Error "Could not find ident at this location"
    | Some(col, identIsland) ->
      let identIsland = Array.toList identIsland
      let! declarations = checkResults.GetDeclarationLocation(pos.Line, col, lineStr, identIsland, preferFlag = false)

      let decompile assembly externalSym =
        match Decompiler.tryFindExternalDeclaration checkResults (assembly, externalSym) with
        | Ok extDec -> ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration extDec)
        | Error(Decompiler.FindExternalDeclarationError.ReferenceHasNoFileName assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' missing file name" assy.SimpleName)
        | Error(Decompiler.FindExternalDeclarationError.ReferenceNotFound assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' not found" assy)
        | Error(Decompiler.FindExternalDeclarationError.DecompileError (Decompiler.Exception(symbol, file, exn))) ->
          Error (sprintf "Error while decompiling symbol '%A' in file '%s': %s\n%s" symbol file exn.Message exn.StackTrace)

      /// these are all None because you can't easily get the source file from the external symbol information here.
      let tryGetSourceRangeForSymbol (sym: ExternalSymbol): (string<NormalizedRepoPathSegment> * int * int) option =
        match sym with
        | ExternalSymbol.Type name -> None
        | ExternalSymbol.Constructor(typeName, args) -> None
        | ExternalSymbol.Method(typeName, name, paramSyms, genericArity) -> None
        | ExternalSymbol.Field(typeName, name) -> None
        | ExternalSymbol.Event(typeName, name) -> None
        | ExternalSymbol.Property(typeName, name) -> None

      // attempts to manually discover symbol use and externalsymbol information for a range that doesn't exist in a local file
      // bugfix/workaround for FCS returning invalid declfound for f# members.
      let tryRecoverExternalSymbolForNonexistentDecl (rangeInNonexistentFile: range): Async<ResultOrString<string<LocalPath> * string<NormalizedRepoPathSegment>>> = async {
        match Lexer.findLongIdents(pos.Column - 1, lineStr) with
        | None -> return ResultOrString.Error (sprintf "Range for nonexistent file found, no ident found: %s" rangeInNonexistentFile.FileName)
        | Some (col, identIsland) ->
          let identIsland = Array.toList identIsland
          let! symbolUse = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)
          match symbolUse with
          | None -> return ResultOrString.Error (sprintf "Range for nonexistent file found, no symboluse found: %s" rangeInNonexistentFile.FileName)
          | Some sym ->
            match sym.Symbol.Assembly.FileName with
            | Some fullFilePath ->
              return Ok (UMX.tag<LocalPath> fullFilePath, UMX.tag<NormalizedRepoPathSegment> rangeInNonexistentFile.FileName)
            | None ->
              return ResultOrString.Error (sprintf "Assembly '%s' declaring symbol '%s' has no location on disk" sym.Symbol.Assembly.QualifiedName sym.Symbol.DisplayName)
      }

      match declarations with
      | FSharpFindDeclResult.DeclNotFound reason ->
        let elaboration =
          match reason with
          | FSharpFindDeclFailureReason.NoSourceCode -> "No source code was found for the declaration"
          | FSharpFindDeclFailureReason.ProvidedMember m -> sprintf "Go-to-declaration is not available for Type Provider-provided member %s" m
          | FSharpFindDeclFailureReason.ProvidedType t -> sprintf "Go-to-declaration is not available from Type Provider-provided type %s" t
          | FSharpFindDeclFailureReason.Unknown r -> r
        return ResultOrString.Error (sprintf "Could not find declaration. %s" elaboration)
      | FSharpFindDeclResult.DeclFound range when range.FileName.EndsWith(Range.rangeStartup.FileName) -> return ResultOrString.Error "Could not find declaration"
      | FSharpFindDeclResult.DeclFound range when System.IO.File.Exists range.FileName ->
        let rangeStr = range.ToString()
        logger.info (Log.setMessage "Got a declresult of {range} that supposedly exists" >> Log.addContextDestructured "range" rangeStr)
        return Ok (FindDeclarationResult.Range range)
      | FSharpFindDeclResult.DeclFound rangeInNonexistentFile ->
        let range = rangeInNonexistentFile.ToString()
        logger.warn (Log.setMessage "Got a declresult of {range} that doesn't exist" >> Log.addContextDestructured "range" range)
        match! tryRecoverExternalSymbolForNonexistentDecl rangeInNonexistentFile with
        | Ok (assemblyFile, sourceFile) ->
          match! Sourcelink.tryFetchSourcelinkFile assemblyFile sourceFile with
          | Ok localFilePath ->
            return ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration { File = UMX.untag localFilePath; Line = rangeInNonexistentFile.StartLine; Column = rangeInNonexistentFile.StartColumn })
          | Error reason ->
            return ResultOrString.Error (sprintf "%A" reason)
        | Error e -> return Error e
      | FSharpFindDeclResult.ExternalDecl (assembly, externalSym) ->
        // not enough info on external symbols to get a range-like thing :(
        match tryGetSourceRangeForSymbol externalSym with
        | Some (sourceFile, line, column) ->
          match! Sourcelink.tryFetchSourcelinkFile (UMX.tag<LocalPath> assembly) sourceFile with
          | Ok localFilePath ->
            return ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration { File = UMX.untag localFilePath; Line = line; Column = column })
          | Error reason ->
            logger.info (Log.setMessage "no sourcelink info for {assembly}, decompiling instead" >> Log.addContextDestructured "assembly" assembly)
            return decompile assembly externalSym
        | None ->
          return decompile assembly externalSym
    }

  member __.TryFindTypeDeclaration (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None ->
      return Error "Cannot find ident at this location"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)
      match symbol with
      | None ->
        return Error "Cannot find symbol at this location"
      | Some sym ->

        let tryGetTypeDef (t: FSharpType option) =
          t |> Option.bind (fun t -> if t.HasTypeDefinition then Some t.TypeDefinition else None)

        let rec tryGetSource (ty: FSharpEntity option) = async {
            match ty |> Option.map (fun ty -> ty, ty.DeclarationLocation) with
            | Some (_, loc) when File.Exists loc.FileName ->
                return Ok (FindDeclarationResult.Range loc)
            | Some (ty, loc) ->
                match ty.Assembly.FileName with
                | Some dllFile ->
                    let dllFile = UMX.tag<LocalPath> dllFile
                    let sourceFile = UMX.tag<NormalizedRepoPathSegment> loc.FileName
                    let! source = Sourcelink.tryFetchSourcelinkFile dllFile sourceFile
                    match source with
                    | Ok localFilePath ->
                        return Ok (FindDeclarationResult.ExternalDeclaration { File = UMX.untag localFilePath; Line = loc.StartLine; Column = loc.StartColumn })
                    | Error _ ->
                        return! tryDecompile ty
                | None ->
                    return! tryDecompile ty
            | None ->
                return Error "No type information for the symbol at this location"
          }
        and tryDecompile (ty: FSharpEntity) = async {
            match ty.TryFullName with
            | Some fullName ->
                let externalSym = ExternalSymbol.Type fullName
                // from TryFindIdentifierDeclaration
                let decompile assembly externalSym =
                  match Decompiler.tryFindExternalDeclaration checkResults (assembly, externalSym) with
                  | Ok extDec -> ResultOrString.Ok (FindDeclarationResult.ExternalDeclaration extDec)
                  | Error(Decompiler.FindExternalDeclarationError.ReferenceHasNoFileName assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' missing file name" assy.SimpleName)
                  | Error(Decompiler.FindExternalDeclarationError.ReferenceNotFound assy) -> ResultOrString.Error (sprintf "External declaration assembly '%s' not found" assy)
                  | Error(Decompiler.FindExternalDeclarationError.DecompileError (Decompiler.Exception(symbol, file, exn))) ->
                      Error (sprintf "Error while decompiling symbol '%A' in file '%s': %s\n%s" symbol file exn.Message exn.StackTrace)

                return decompile ty.Assembly.SimpleName externalSym
            | None ->
                // might be abbreviated type (like string)
                return!
                  if ty.IsFSharpAbbreviation then Some ty.AbbreviatedType else None
                  |> tryGetTypeDef
                  |> tryGetSource
          }

        let ty =
          match sym with
          | SymbolUse.Field f -> Some f.FieldType |> tryGetTypeDef
          | SymbolUse.Constructor c -> c.DeclaringEntity
          | SymbolUse.Property p when p.IsPropertyGetterMethod ->
              Some p.ReturnParameter.Type |> tryGetTypeDef
          | SymbolUse.Val v -> v.FullTypeSafe |> tryGetTypeDef
          | SymbolUse.Entity (e, _) -> Some e
          | SymbolUse.UnionCase c -> Some c.ReturnType |> tryGetTypeDef
          | SymbolUse.Parameter p -> Some p.Type |> tryGetTypeDef
          | _ -> None

        return! tryGetSource ty
  }

  member __.TryGetToolTip (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return ResultOrString.Error "Cannot find ident for tooltip"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) ->
          match identIsland with
          | [ident] ->
             match KeywordList.keywordTooltips.TryGetValue ident with
             | true, tip ->
                return Ok tip
             | _ ->
                return ResultOrString.Error "No tooltip information"
          | _ ->
            return ResultOrString.Error "No tooltip information"
      | _ ->
        return Ok(tip)
  }

  member __.TryGetToolTipEnhanced (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return Error "Cannot find ident for tooltip"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)

      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) && symbol.IsNone ->
          match identIsland with
          | [ident] ->
             match KeywordList.keywordTooltips.TryGetValue ident with
             | true, tip ->
                return Ok (tip, ident, "", None)
             | _ ->
                return Error "No tooltip information"
          | _ ->
            return Error "No tooltip information"
      | _ ->
        match symbol with
        | None ->
          return Error "No tooltip information"
        | Some symbol ->

          match SignatureFormatter.getTooltipDetailsFromSymbolUse symbol with
          | None ->
            return Error "No tooltip information"
          | Some (signature, footer) ->
              let typeDoc = getTypeIfConstructor symbol.Symbol |> Option.map (fun n -> n.XmlDocSig)
              return Ok (tip, signature, footer, typeDoc)
  }

  member __.TryGetFormattedDocumentation (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return Error "Cannot find ident"
    | Some(col,identIsland) ->
      let identIsland = Array.toList identIsland
      // TODO: Display other tooltip types, for example for strings or comments where appropriate
      let! tip = checkResults.GetToolTipText(pos.Line, col, lineStr, identIsland, FSharpTokenTag.Identifier)
      let! symbol = checkResults.GetSymbolUseAtLocation(pos.Line, col, lineStr, identIsland)

      match tip with
      | FSharpToolTipText(elems) when elems |> List.forall ((=) FSharpToolTipElement.None) && symbol.IsNone ->
          match identIsland with
          | [ident] ->
             match KeywordList.keywordTooltips.TryGetValue ident with
             | true, tip ->
                return Ok (Some tip, None, (ident, (DocumentationFormatter.emptyTypeTip)), "", "")
             | _ ->
                return Error "No tooltip information"
          | _ ->
            return Error "No documentation information"
      | _ ->
      match symbol with
      | None ->
        return Error "No documentation information"
      | Some symbol ->
        match DocumentationFormatter.getTooltipDetailsFromSymbolUse symbol with
        | None ->
          return Error "No documentation information"
        | Some (signature, footer, cn) ->
            match symbol with
            | SymbolUse.TypeAbbreviation symbol ->
              return Ok (None, Some (symbol.GetAbbriviatedParent().XmlDocSig, symbol.GetAbbriviatedParent().Assembly.FileName |> Option.defaultValue ""), signature, footer, cn)
            | _ ->
              return Ok (Some tip, None, signature, footer, cn)
  }

  member x.TryGetFormattedDocumentationForSymbol (xmlSig: string) (assembly: string) = async {
    let entities = x.GetAllEntities false
    let ent =
      entities |> List.tryFind (fun e ->
        let check = (e.Symbol.XmlDocSig = xmlSig && e.Symbol.Assembly.SimpleName = assembly)
        if not check then
          match e.Symbol with
          | FSharpEntity (_, abrvEnt, _) ->
            abrvEnt.XmlDocSig = xmlSig && abrvEnt.Assembly.SimpleName = assembly
          | _ -> false
        else
          true
      )
    let ent =
      match ent with
      | Some ent -> Some ent
      | None ->
        entities |> List.tryFind (fun e ->
          let check = (e.Symbol.XmlDocSig = xmlSig)
          if not check then
            match e.Symbol with
            | FSharpEntity (_, abrvEnt, _) ->
              abrvEnt.XmlDocSig = xmlSig
            | _ -> false
          else
            true
        )

    let symbol =
      match ent with
      | Some ent -> Some ent.Symbol
      | None ->
        entities |> List.tryPick (fun e ->
          match e.Symbol with
          | FSharpEntity (ent, _, _) ->
            match ent.MembersFunctionsAndValues |> Seq.tryFind (fun f -> f.XmlDocSig = xmlSig) with
            | Some e -> Some (e :> FSharpSymbol)
            | None ->
              match  ent.FSharpFields |> Seq.tryFind (fun f -> f.XmlDocSig = xmlSig) with
              | Some e -> Some (e :> FSharpSymbol)
              | None -> None
          | _ ->
            None
        )

    match symbol with
    | None -> return Error "No matching symbol information"
    | Some symbol ->
      match DocumentationFormatter.getTooltipDetailsFromSymbol symbol with
      | None ->
        return Error "No tooltip information"
      | Some (signature, footer, cn) ->
          return Ok (symbol.XmlDocSig, symbol.Assembly.FileName |> Option.defaultValue "", symbol.XmlDoc |> Seq.toList , signature, footer, cn)
  }

  member __.TryGetSymbolUse (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None ->
      return ResultOrString.Error "No ident at this location"
    | Some(colu, identIsland) ->

    let identIsland = Array.toList identIsland
    let! symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
    match symboluse with
    | None ->
      return ResultOrString.Error "No symbol information found"
    | Some symboluse ->

      let! symboluses = checkResults.GetUsesOfSymbolInFile symboluse.Symbol
      return Ok (symboluse, symboluses)
  }

  member __.TryGetSignatureData (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None ->
      return ResultOrString.Error "No ident at this location"
    | Some(colu, identIsland) ->

      let identIsland = Array.toList identIsland
      let! symboluse = checkResults.GetSymbolUseAtLocation(pos.Line, colu, lineStr, identIsland)
      match symboluse with
      | None ->
        return ResultOrString.Error "No symbol information found"
      | Some symboluse ->
        let fsym = symboluse.Symbol
        match fsym with
        | :? FSharpMemberOrFunctionOrValue as symbol ->
          let typ = symbol.ReturnParameter.Type.Format symboluse.DisplayContext
          if symbol.IsPropertyGetterMethod then
              return Ok(typ, [], [])
          else
            let parms =
              symbol.CurriedParameterGroups
              |> Seq.map (Seq.map (fun p -> p.DisplayName, p.Type.Format symboluse.DisplayContext) >> Seq.toList )
              |> Seq.toList
            let generics =
              symbol.GenericParameters
              |> Seq.map (fun generic ->
                  generic.Name
              )
              |> Seq.toList
            // Abstract members and abstract member overrides with one () parameter seem have a list with an empty list
            // as parameters.
            match parms with
            | [ [] ] when symbol.IsMember && (not symbol.IsPropertyGetterMethod) ->
              return Ok(typ, [ [ ("unit", "unit") ] ], [])
            | _ ->
              return Ok(typ, parms, generics)
        | _ ->
          return ResultOrString.Error "Not a member, function or value"
  }

  member __.TryGetF1Help (pos: pos) (lineStr: LineStr) = async {
    match Lexer.findLongIdents(pos.Column - 1, lineStr) with
    | None -> return ResultOrString.Error "No ident at this location"
    | Some(colu, identIsland) ->

      let identIsland = Array.toList identIsland
      let! help = checkResults.GetF1Keyword(pos.Line, colu, lineStr, identIsland)
      match help with
      | None -> return ResultOrString.Error "No symbol information found"
      | Some hlp -> return Ok hlp
  }

  member __.TryGetCompletions (pos: pos) (lineStr: LineStr) filter (getAllSymbols : unit -> AssemblySymbol list) = async {
    try
      let longName = FSharp.Compiler.QuickParse.GetPartialLongNameEx(lineStr, pos.Column - 2)
      let residue = longName.PartialIdent
      logger.info (Log.setMessage "TryGetCompletions - long name: {longName}" >> Log.addContextDestructured "longName" longName)

      let getAllSymbols() =
        getAllSymbols()
        |> List.filter (fun entity -> entity.FullName.Contains "." && not (PrettyNaming.IsOperatorName entity.Symbol.DisplayName))

      let token = Lexer.getSymbol pos.Line (pos.Column - 1) lineStr SymbolLookupKind.Simple [||]
      logger.info (Log.setMessage "TryGetCompletions - token: {token}" >> Log.addContextDestructured "token" token)
      let isEmpty = longName.QualifyingIdents.IsEmpty && String.IsNullOrWhiteSpace longName.PartialIdent && longName.LastDotPos.IsNone

      match token with
      | Some k when k.Kind = Other && not isEmpty -> return None
      | Some k when k.Kind = Operator  -> return None
      | Some k when k.Kind = Keyword  -> return None
      | _ ->

      let! results = checkResults.GetDeclarationListInfo(Some parseResults, pos.Line, lineStr, longName, getAllSymbols)

      let getKindPriority = function
        | CompletionItemKind.CustomOperation -> -1
        | CompletionItemKind.Property -> 0
        | CompletionItemKind.Field -> 1
        | CompletionItemKind.Method (isExtension = false) -> 2
        | CompletionItemKind.Event -> 3
        | CompletionItemKind.Argument -> 4
        | CompletionItemKind.Other -> 5
        | CompletionItemKind.Method (isExtension = true) -> 6

      let decls =
        match filter with
        | Some "StartsWith" ->
          results.Items
          |> Array.filter (fun d -> d.Name.StartsWith(residue, StringComparison.InvariantCultureIgnoreCase))
        | Some "Contains" ->
          results.Items
          |> Array.filter (fun d -> d.Name.IndexOf(residue, StringComparison.InvariantCultureIgnoreCase) >= 0)
        | _ -> results.Items

      let sortedDecls =
          decls
          |> Array.sortWith (fun x y ->
              let mutable n = (not x.IsResolved).CompareTo(not y.IsResolved)
              if n <> 0 then n else
                  n <- (getKindPriority x.Kind).CompareTo(getKindPriority y.Kind)
                  if n <> 0 then n else
                      n <- (not x.IsOwnMember).CompareTo(not y.IsOwnMember)
                      if n <> 0 then n else
                          n <- StringComparer.OrdinalIgnoreCase.Compare(x.Name, y.Name)
                          if n <> 0 then n else
                            x.MinorPriority.CompareTo(y.MinorPriority))

      let shouldKeywords = sortedDecls.Length > 0 && not results.IsForType && not results.IsError && List.isEmpty longName.QualifyingIdents
      return Some (sortedDecls, residue, shouldKeywords)
    with :? TimeoutException -> return None
  }

  member __.GetAllEntities (publicOnly: bool) : AssemblySymbol list =
      try
        let res = [
          yield! AssemblyContentProvider.getAssemblySignatureContent AssemblyContentType.Full checkResults.PartialAssemblySignature
          let ctx = checkResults.ProjectContext
          let assembliesByFileName =
            ctx.GetReferencedAssemblies()
            |> List.groupBy (fun asm -> asm.FileName)
            |> List.rev // if mscorlib.dll is the first then FSC raises exception when we try to
                        // get Content.Entities from it.

          for fileName, signatures in assembliesByFileName do
            let contentType = if publicOnly then Public else Full
            let content = AssemblyContentProvider.getAssemblyContent entityCache.Locking contentType fileName signatures
            yield! content
        ]
        res
      with
      | _ -> []

  member __.GetAllSymbolUsesInFile () = checkResults.GetAllUsesOfAllSymbolsInFile()

  member __.GetSemanticClassification = checkResults.GetSemanticClassification None
  member __.GetAST = parseResults.ParseTree
  member __.GetCheckResults = checkResults
  member __.GetParseResults = parseResults
  member __.FileName: string = parseResults.FileName
