/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions

module FsAutoComplete.FCSPatches

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FsAutoComplete.UntypedAstUtils
open System.Collections.Generic
open System.Runtime.CompilerServices
open FSharp.Compiler.PrettyNaming
open FSharp.Compiler
open FSharp.Compiler.AbstractIL.Internal.Library

[<Extension>]
type DictionaryExtensions() =

    [<Extension>]
    static member inline BagAdd(dic: Dictionary<'key, 'value list>, key: 'key, value: 'value) =
        match dic.TryGetValue key with
        | true, values -> dic.[key] <- value :: values
        | _ -> dic.[key] <- [value]

    [<Extension>]
    static member inline BagExistsValueForKey(dic: Dictionary<'key, 'value list>, key: 'key, f: 'value -> bool) =
        match dic.TryGetValue key with
        | true, values -> values |> List.exists f
        | _ -> false

module internal SynExprAppLocationsImpl =
    let rec private searchSynArgExpr traverseSynExpr expr ranges =
        match expr with
        | SynExpr.Const(SynConst.Unit, _) ->
            None, None

        | SynExpr.Paren(SynExpr.Tuple (_, exprs, _commas, _tupRange), _, _, _parenRange) ->
            let rec loop (exprs: SynExpr list) ranges =
                match exprs with
                | [] -> ranges
                | h::t ->
                    loop t (h.Range :: ranges)

            let res = loop exprs ranges
            Some (res), None

        | SynExpr.Paren(SynExpr.Paren(_, _, _, _) as synExpr, _, _, _parenRange) ->
            let r, _cacheOpt = searchSynArgExpr traverseSynExpr synExpr ranges
            r, None

        | SynExpr.Paren(SynExpr.App (_, _isInfix, _, _, _range), _, _, parenRange) ->
            Some (parenRange :: ranges), None

        | e ->
            let inner = traverseSynExpr e
            match inner with
            | None ->
                Some (e.Range :: ranges), Some inner
            | _ -> None, Some inner

    let getAllCurriedArgsAtPosition pos parseTree =
        AstTraversal.Traverse(pos, parseTree, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
                match expr with
                | SynExpr.App (_exprAtomicFlag, _isInfix, funcExpr, argExpr, range) when posEq pos range.Start ->
                    let isInfixFuncExpr =
                        match funcExpr with
                        | SynExpr.App (_, isInfix, _, _, _) -> isInfix
                        | _ -> false

                    if isInfixFuncExpr then
                        traverseSynExpr funcExpr
                    else
                        let workingRanges =
                            match traverseSynExpr funcExpr with
                            | Some ranges -> ranges
                            | None -> []

                        let xResult, cacheOpt = searchSynArgExpr traverseSynExpr argExpr workingRanges
                        match xResult, cacheOpt with
                        | Some ranges, _ -> Some ranges
                        | None, Some cache -> cache
                        | _ -> traverseSynExpr argExpr
                | _ -> defaultTraverse expr })
        |> Option.map List.rev

type FSharpParseFileResults with

  member scope.IsPositionContainedInACurriedParameter pos =
    match scope.ParseTree with
    | Some input ->
        let result =
            AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
                member __.VisitExpr(_path, traverseSynExpr, defaultTraverse, expr) =
                    defaultTraverse(expr)

                override __.VisitBinding (_, binding) =
                    match binding with
                    | SynBinding.Binding(_, _, _, _, _, _, valData, _, _, _, ((ContainsPos pos) as range), _) ->
                        let info = valData.SynValInfo.CurriedArgInfos
                        let mutable found = false
                        for group in info do
                            for arg in group do
                                match arg.Ident with
                                | Some (IdentContainsPos pos) ->
                                    found <- true
                                | _ -> ()
                        if found then Some range else None
                    | _ ->
                        None
            })
        result.IsSome
    | _ -> false

  member scope.TryRangeOfParenEnclosingOpEqualsGreaterUsage opGreaterEqualPos =
    /// reused pattern to find applications of => (a symptom of improper C# style lambdas)
    let (|InfixAppOfOpEqualsGreater|_|) =
      function | SynExpr.App(ExprAtomicFlag.NonAtomic, false, SynExpr.App(ExprAtomicFlag.NonAtomic, true, Ident "op_EqualsGreater", actualParamListExpr, _), actualLambdaBodyExpr, _) -> Some (actualParamListExpr, actualLambdaBodyExpr)
               | _ -> None

    match scope.ParseTree with
    | None -> None
    | Some input ->
      let visitor = {
        new AstTraversal.AstVisitorBase<_>() with
          member _.VisitExpr(_, _, defaultTraverse, expr) =
            match expr with
            | SynExpr.Paren((InfixAppOfOpEqualsGreater(lambdaArgs, lambdaBody) as app), _, _, _) ->
              Some (app.Range, lambdaArgs.Range, lambdaBody.Range)
            | _ -> defaultTraverse expr
          member _.VisitBinding(defaultTraverse, binding) =
            match binding with
            | SynBinding.Binding (_, SynBindingKind.NormalBinding, _, _, _, _, _, _, _, (InfixAppOfOpEqualsGreater(lambdaArgs, lambdaBody) as app), _, _) ->
              Some(app.Range, lambdaArgs.Range, lambdaBody.Range)
            | _ -> defaultTraverse binding
        }
      AstTraversal.Traverse(opGreaterEqualPos, input, visitor)

  member scope.TryRangeOfRefCellDereferenceContainingPos expressionPos =
    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(expressionPos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.App(_, false, SynExpr.Ident funcIdent, expr, _) ->
                    if funcIdent.idText = "op_Dereference" && rangeContainsPos expr.Range expressionPos then
                        Some funcIdent.idRange
                    else
                        None
                | _ -> defaultTraverse expr })
    | None -> None

  member scope.TryRangeOfRecordExpressionContainingPos pos =
    match scope.ParseTree with
    | Some input ->
        AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
            member _.VisitExpr(_, _, defaultTraverse, expr) =
                match expr with
                | SynExpr.Record(_, _, _, range) when rangeContainsPos range pos ->
                    Some range
                | _ -> defaultTraverse expr })
    | None ->
        None

  member scope.TryRangeOfExprInYieldOrReturn pos =
      match scope.ParseTree with
      | Some parseTree ->
          AstTraversal.Traverse(pos, parseTree, { new AstTraversal.AstVisitorBase<_>() with
              member __.VisitExpr(_path, _, defaultTraverse, expr) =
                  match expr with
                  | SynExpr.YieldOrReturn(_, expr, range)
                  | SynExpr.YieldOrReturnFrom(_, expr, range) when rangeContainsPos range pos ->
                      Some expr.Range
                  | _ -> defaultTraverse expr })
      | None -> None

  /// Attempts to find an Ident of a pipeline containing the given position, and the number of args already applied in that pipeline.
  /// For example, '[1..10] |> List.map ' would give back the ident of '|>' and 1, because it applied 1 arg (the list) to 'List.map'.
  member scope.TryIdentOfPipelineContainingPosAndNumArgsApplied pos =
      match scope.ParseTree with
      | Some input ->
          AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                  match expr with
                  | SynExpr.App (_, _, SynExpr.App(_, true, SynExpr.Ident ident, _, _), argExpr, _) when rangeContainsPos argExpr.Range pos ->
                      if ident.idText = "op_PipeRight" then
                          Some (ident, 1)
                      elif ident.idText = "op_PipeRight2" then
                          Some (ident, 2)
                      elif ident.idText = "op_PipeRight3" then
                          Some (ident, 3)
                      else
                          None
                  | _ -> defaultTraverse expr
          })
      | None -> None

  /// Determines if the given position is inside a function or method application.
  member scope.IsPosContainedInApplication pos =
      match scope.ParseTree with
      | Some input ->
          let result =
              AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
                  member _.VisitExpr(_, _, defaultTraverse, expr) =
                      match expr with
                      | SynExpr.App (_, _, _, _, range) when rangeContainsPos range pos ->
                          Some range
                      | _ -> defaultTraverse expr
              })
          result.IsSome
      | None -> false

  /// Attempts to find the range of a function or method that is being applied. Also accounts for functions in pipelines.
  member scope.TryRangeOfFunctionOrMethodBeingApplied pos =
      let rec getIdentRangeForFuncExprInApp expr pos =
          match expr with
          | SynExpr.Ident ident -> ident.idRange

          | SynExpr.LongIdent(_, _, _, range) -> range

          | SynExpr.Paren(expr, _, _, range) when rangeContainsPos range pos ->
              getIdentRangeForFuncExprInApp expr pos

          | SynExpr.App(_, _, funcExpr, argExpr, _) ->
              match argExpr with
              | SynExpr.App (_, _, _, _, range) when rangeContainsPos range pos ->
                  getIdentRangeForFuncExprInApp argExpr pos
              | _ ->
                  match funcExpr with
                  | SynExpr.App (_, true, _, _, _) when rangeContainsPos argExpr.Range pos ->
                      // x |> List.map
                      // Don't dive into the funcExpr (the operator expr)
                      // because we dont want to offer sig help for that!
                      getIdentRangeForFuncExprInApp argExpr pos
                  | _ ->
                      // Generally, we want to dive into the func expr to get the range
                      // of the identifier of the function we're after
                      getIdentRangeForFuncExprInApp funcExpr pos
          | expr -> expr.Range // Exhaustiveness, this shouldn't actually be necessary...right?

      match scope.ParseTree with
      | Some input ->
          AstTraversal.Traverse(pos, input, { new AstTraversal.AstVisitorBase<_>() with
              member _.VisitExpr(_, _, defaultTraverse, expr) =
                  match expr with
                  | SynExpr.App (_, _, _funcExpr, _, range) as app when rangeContainsPos range pos ->
                      getIdentRangeForFuncExprInApp app pos
                      |> Some
                  | _ -> defaultTraverse expr
          })
      | None -> None

  /// Gets the ranges of all arguments, if they can be found, for a function application at the given position.
  member scope.GetAllArgumentsForFunctionApplicationAtPostion pos =
      match scope.ParseTree with
      | Some input -> SynExprAppLocationsImpl.getAllCurriedArgsAtPosition pos input
      | None -> None


module UnusedOpens =

    let symbolHash = HashIdentity.FromFunctions (fun (x: FSharpSymbol) -> x.GetEffectivelySameAsHash()) (fun x y -> x.IsEffectivelySameAs(y))

    /// Represents one namespace or module opened by an 'open' statement
    type OpenedModule(entity: FSharpEntity, isNestedAutoOpen: bool) =

        /// Compute an indexed table of the set of symbols revealed by 'open', on-demand
        let revealedSymbols : Lazy<HashSet<FSharpSymbol>> =
           lazy
            let symbols =
               [| for ent in entity.NestedEntities do
                      yield ent :> FSharpSymbol

                      if ent.IsFSharpRecord then
                          for rf in ent.FSharpFields do
                              yield rf  :> FSharpSymbol

                      if ent.IsFSharpUnion && not (Symbol.hasAttribute<RequireQualifiedAccessAttribute> ent.Attributes) then
                          for unionCase in ent.UnionCases do
                              yield unionCase :> FSharpSymbol

                      if Symbol.hasAttribute<ExtensionAttribute> ent.Attributes then
                          for fv in ent.MembersFunctionsAndValues do
                              // fv.IsExtensionMember is always false for C# extension methods returning by `MembersFunctionsAndValues`,
                              // so we have to check Extension attribute instead.
                              // (note: fv.IsExtensionMember has proper value for symbols returning by GetAllUsesOfAllSymbolsInFile though)
                              if Symbol.hasAttribute<ExtensionAttribute> fv.Attributes then
                                  yield fv :> FSharpSymbol

                  for apCase in entity.ActivePatternCases do
                      yield apCase :> FSharpSymbol

                  // The IsNamespace and IsFSharpModule cases are handled by looking at DeclaringEntity below
                  if not entity.IsNamespace && not entity.IsFSharpModule then
                      for fv in entity.MembersFunctionsAndValues do
                          yield fv :> FSharpSymbol |]

            HashSet<_>(symbols, symbolHash)

        member __.Entity = entity
        member __.IsNestedAutoOpen = isNestedAutoOpen
        member __.RevealedSymbolsContains(symbol) = revealedSymbols.Force().Contains symbol

    type OpenedModuleGroup =
        { OpenedModules: OpenedModule list }

        static member Create (modul: FSharpEntity) =
            let rec getModuleAndItsAutoOpens (isNestedAutoOpen: bool) (modul: FSharpEntity) =
                [ yield OpenedModule (modul, isNestedAutoOpen)
                  for ent in modul.NestedEntities do
                    if ent.IsFSharpModule && Symbol.hasAttribute<AutoOpenAttribute> ent.Attributes then
                      yield! getModuleAndItsAutoOpens true ent ]
            { OpenedModules = getModuleAndItsAutoOpens false modul }

    /// Represents single open statement.
    type OpenStatement =
        { /// All namespaces and modules which this open declaration effectively opens, including the AutoOpen ones
          OpenedGroups: OpenedModuleGroup list

          /// The range of open statement itself
          Range: range

          /// The scope on which this open declaration is applied
          AppliedScope: range }

    /// Gets the open statements, their scopes and their resolutions
    let getOpenStatements (openDeclarations: FSharpOpenDeclaration[]) : OpenStatement[] =
        openDeclarations
        |> Array.filter (fun x -> not x.IsOwnNamespace)
        |> Array.choose (fun openDecl ->
             match openDecl.LongId, openDecl.Range with
             | firstId :: _, Some range ->
                 if firstId.idText = MangledGlobalName then
                     None
                 else
                     Some { OpenedGroups = openDecl.Modules |> List.map OpenedModuleGroup.Create
                            Range = range
                            AppliedScope = openDecl.AppliedScope }
             | _ -> None)

    /// Only consider symbol uses which are the first part of a long ident, i.e. with no qualifying identifiers
    let filterSymbolUses (getSourceLineStr: int -> string) (symbolUses: FSharpSymbolUse[]) : FSharpSymbolUse[] =
        symbolUses
        |> Array.filter (fun su ->
             match su.Symbol with
             | :? FSharpMemberOrFunctionOrValue as fv when fv.IsExtensionMember ->
                // Extension members should be taken into account even though they have a prefix (as they do most of the time)
                true

             | :? FSharpMemberOrFunctionOrValue as fv when not fv.IsModuleValueOrMember ->
                // Local values can be ignored
                false

             | :? FSharpMemberOrFunctionOrValue when su.IsFromDefinition ->
                // Value definitions should be ignored
                false

             | :? FSharpGenericParameter ->
                // Generic parameters can be ignored, they never come into scope via 'open'
                false

             | :? FSharpUnionCase when su.IsFromDefinition ->
                false

             | :? FSharpField as field when
                     field.DeclaringEntity.IsSome && field.DeclaringEntity.Value.IsFSharpRecord ->
                // Record fields are used in name resolution
                true

             | :? FSharpField as field when field.IsUnionCaseField ->
                 false

             | _ ->
                // For the rest of symbols we pick only those which are the first part of a long ident, because it's they which are
                // contained in opened namespaces / modules. For example, we pick `IO` from long ident `IO.File.OpenWrite` because
                // it's `open System` which really brings it into scope.
                let partialName = QuickParse.GetPartialLongNameEx (getSourceLineStr su.RangeAlternate.StartLine, su.RangeAlternate.EndColumn - 1)
                List.isEmpty partialName.QualifyingIdents)

    /// Split symbol uses into cases that are easy to handle (via DeclaringEntity) and those that don't have a good DeclaringEntity
    let splitSymbolUses (symbolUses: FSharpSymbolUse[]) : FSharpSymbolUse[] * FSharpSymbolUse[] =
        symbolUses |> Array.partition (fun symbolUse ->
            let symbol = symbolUse.Symbol
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as f ->
                match f.DeclaringEntity with
                | Some ent when ent.IsNamespace || ent.IsFSharpModule -> true
                | _ -> false
            | _ -> false)

    /// Given an 'open' statement, find fresh modules/namespaces referred to by that statement where there is some use of a revealed symbol
    /// in the scope of the 'open' is from that module.
    ///
    /// Performance will be roughly NumberOfOpenStatements x NumberOfSymbolUses
    let isOpenStatementUsed (symbolUses2: FSharpSymbolUse[]) (symbolUsesRangesByDeclaringEntity: Dictionary<FSharpEntity, range list>)
                            (usedModules: Dictionary<FSharpEntity, range list>) (openStatement: OpenStatement) =

        // Don't re-check modules whose symbols are already known to have been used
        let openedGroupsToExamine =
            openStatement.OpenedGroups |> List.choose (fun openedGroup ->
                let openedEntitiesToExamine =
                    openedGroup.OpenedModules
                    |> List.filter (fun openedEntity ->
                        not (usedModules.BagExistsValueForKey(openedEntity.Entity, fun scope -> rangeContainsRange scope openStatement.AppliedScope)))

                match openedEntitiesToExamine with
                | [] -> None
                | _ when openedEntitiesToExamine |> List.exists (fun x -> not x.IsNestedAutoOpen) -> Some { OpenedModules = openedEntitiesToExamine }
                | _ -> None)

        // Find the opened groups that are used by some symbol use
        let newlyUsedOpenedGroups =
            openedGroupsToExamine |> List.filter (fun openedGroup ->
                openedGroup.OpenedModules |> List.exists (fun openedEntity ->
                    symbolUsesRangesByDeclaringEntity.BagExistsValueForKey(openedEntity.Entity, fun symbolUseRange ->
                        rangeContainsRange openStatement.AppliedScope symbolUseRange &&
                        Range.posGt symbolUseRange.Start openStatement.Range.End) ||

                    symbolUses2 |> Array.exists (fun symbolUse ->
                        rangeContainsRange openStatement.AppliedScope symbolUse.RangeAlternate &&
                        Range.posGt symbolUse.RangeAlternate.Start openStatement.Range.End &&
                        openedEntity.RevealedSymbolsContains symbolUse.Symbol)))

        // Return them as interim used entities
        let newlyOpenedModules = newlyUsedOpenedGroups |> List.collect (fun openedGroup -> openedGroup.OpenedModules)
        for openedModule in newlyOpenedModules do
            let scopes =
                match usedModules.TryGetValue openedModule.Entity with
                | true, scopes -> openStatement.AppliedScope :: scopes
                | _ -> [openStatement.AppliedScope]
            usedModules.[openedModule.Entity] <- scopes
        not (isNil newlyOpenedModules)

    /// Incrementally filter out the open statements one by one. Filter those whose contents are referred to somewhere in the symbol uses.
    /// Async to allow cancellation.
    let rec filterOpenStatementsIncremental symbolUses2 (symbolUsesRangesByDeclaringEntity: Dictionary<FSharpEntity, range list>) (openStatements: OpenStatement list)
                                            (usedModules: Dictionary<FSharpEntity, range list>) acc =
        async {
            match openStatements with
            | openStatement :: rest ->
                if isOpenStatementUsed symbolUses2 symbolUsesRangesByDeclaringEntity usedModules openStatement then
                    return! filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity rest usedModules acc
                else
                    // The open statement has not been used, include it in the results
                    return! filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity rest usedModules (openStatement :: acc)
            | [] -> return List.rev acc
        }

    let entityHash = HashIdentity.FromFunctions (fun (x: FSharpEntity) -> x.GetEffectivelySameAsHash()) (fun x y -> x.IsEffectivelySameAs(y))

    /// Filter out the open statements whose contents are referred to somewhere in the symbol uses.
    /// Async to allow cancellation.
    let filterOpenStatements (symbolUses1: FSharpSymbolUse[], symbolUses2: FSharpSymbolUse[]) openStatements =
        async {
            // the key is a namespace or module, the value is a list of FSharpSymbolUse range of symbols defined in the
            // namespace or module. So, it's just symbol uses ranges grouped by namespace or module where they are _defined_.
            let symbolUsesRangesByDeclaringEntity = Dictionary<FSharpEntity, range list>(entityHash)
            for symbolUse in symbolUses1 do
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as f ->
                    match f.DeclaringEntity with
                    | Some entity when entity.IsNamespace || entity.IsFSharpModule ->
                        symbolUsesRangesByDeclaringEntity.BagAdd(entity, symbolUse.RangeAlternate)
                    | _ -> ()
                | _ -> ()

            let! results = filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity (List.ofArray openStatements) (Dictionary(entityHash)) []
            return results |> List.map (fun os -> os.Range)
        }

    /// Get the open statements whose contents are not referred to anywhere in the symbol uses.
    /// Async to allow cancellation.
    let getUnusedOpens (checkFileResults: FSharpCheckFileResults, getSourceLineStr: int -> string) : Async<range list> =
        async {
            let! symbolUses = checkFileResults.GetAllUsesOfAllSymbolsInFile()
            let symbolUses = filterSymbolUses getSourceLineStr symbolUses
            let symbolUses = splitSymbolUses symbolUses
            let openStatements = getOpenStatements checkFileResults.OpenDeclarations
            return! filterOpenStatements symbolUses openStatements
        }
