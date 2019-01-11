//Based on VF# implementation - https://github.com/Microsoft/visualfsharp/blob/master/vsintegration/src/FSharp.Editor/Diagnostics/SimplifyNameDiagnosticAnalyzer.fs
namespace FsAutoComplete

open FsAutoComplete

module SimplifyNameDiagnosticAnalyzer =
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Range

    let getPlidLength (plid: string list) = (plid |> List.sumBy String.length) + plid.Length

    let getSimplifyNameRanges (checkResults : FSharpCheckFileResults) (source : string []) (symbolUses: FSharpSymbolUse[]) =
        async {
            let mutable result = ResizeArray()
            let symbolUses =
                symbolUses
                |> Array.Parallel.choose (fun symbolUse ->
                    match symbolUse with
                    | s when s.IsFromOpenStatement -> None
                    | symbolUse ->
                        let lineStr = source.[symbolUse.RangeAlternate.StartLine - 1]
                        // for `System.DateTime.Now` it returns ([|"System"; "DateTime"|], "Now")
                        match Parsing.findLongIdentsAndResidue (symbolUse.RangeAlternate.EndColumn - 1, lineStr) with
                        | [], "" -> None
                        | plid, name -> 
                            // `symbolUse.RangeAlternate.Start` does not point to the start of plid, it points to start of `name`,
                            // so we have to calculate plid's start ourselves.
                            let plidStartCol = symbolUse.RangeAlternate.EndColumn - name.Length - (getPlidLength plid)
                            Some(symbolUse, plid, plidStartCol, name)
                )
                |> Array.groupBy (fun (symbolUse, _, plidStartCol, _) -> symbolUse.RangeAlternate.StartLine, plidStartCol)
                |> Array.map (fun (_, xs) -> xs |> Array.maxBy (fun (symbolUse, _, _, _) -> symbolUse.RangeAlternate.EndColumn))
            
            let! _ = 
                symbolUses
                |> Array.map (fun (symbolUse, plid, plidStartCol, name) -> async {
                    if not symbolUse.IsFromDefinition then
                        let posAtStartOfName =
                            let r = symbolUse.RangeAlternate
                            if r.StartLine = r.EndLine then mkPos r.StartLine (r.EndColumn - name.Length)
                            else r.Start

                        let getNecessaryPlid (plid: string list) : Async<string list> =
                            let rec loop (rest: string list) (current: string list) =
                                async {
                                    match rest with
                                    | [] -> return current
                                    | headIdent :: restPlid ->
                                        let! res = checkResults.IsRelativeNameResolvableFromSymbol(posAtStartOfName, current, symbolUse.Symbol)
                                        if res then return current
                                        else return! loop restPlid (headIdent :: current)
                                }
                            loop (List.rev plid) []

                        let! necessaryPlid = getNecessaryPlid plid

                        match necessaryPlid with
                        | necessaryPlid when necessaryPlid = plid -> ()
                        | necessaryPlid ->
                            let r = symbolUse.RangeAlternate
                            let necessaryPlidStartCol = r.EndColumn - name.Length - (getPlidLength necessaryPlid)

                            let unnecessaryRange =
                                mkRange r.FileName (mkPos r.StartLine plidStartCol) (mkPos r.EndLine necessaryPlidStartCol)

                            let relativeName = (String.concat "." plid) + "." + name
                            result.Add (unnecessaryRange, relativeName)
                })
                |> Async.Parallel
            return result
        }
