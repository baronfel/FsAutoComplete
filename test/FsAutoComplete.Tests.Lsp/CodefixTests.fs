module FsAutoComplete.Tests.CodeFixTests

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers

let resolveNamespacesTests toolsPath =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeFixes", "ResolveNamespace")
    let resolveNamespaceConfig =
      { defaultConfigDto with
          ResolveNamespaces = Some true }
    let (server, event) = serverInitialize path resolveNamespaceConfig toolsPath
    do waitForWorkspaceFinishedParsing event
    let scriptPath = Path.Combine(path, "Script.fsx")
    let scriptTextDocumentUri = loadDocument scriptPath
    let tdop : DidOpenTextDocumentParams = { TextDocument = scriptTextDocumentUri }
    do server.TextDocumentDidOpen tdop |> Async.Start
    match waitForParseResultsForFile "Script.fsx" event with
    | Ok () ->
      server, scriptPath, None
    | Error diags ->
      server, scriptPath, Some diags
  )

  let serverTest f () = f serverStart.Value

  testList "resolve namespaces" [
    testCase "can resolve system for timespan" (serverTest (fun (server, scriptPath, diags) ->
      match diags with
      | Some diags ->
        let context : CodeActionParams =
          { TextDocument = { Uri = Path.FilePathToUri scriptPath }
            Range = { Start = { Line = 3; Character = 13 }; End = {Line = 3; Character = 13} }
            Context = {
              Diagnostics = [| diags.[0] |]
            }
          }
        match server.TextDocumentCodeAction context |> Async.RunSynchronously with
        | Ok (Some (TextDocumentCodeActionResult.CodeActions actions)) ->
          Expect.exists actions
            (function
              | { Kind = Some "quickfix"
                  Edit = {
                    DocumentChanges = Some [|
                      { Edits = [|
                          { NewText = "open System\n"; Range = { Start = {  Line = 1; Character = 0 }; End = { Line = 1; Character = 0 }} }
                          { NewText = ""; Range = { Start = {  Line = 2; Character = 0 }; End = { Line = 2; Character = 0 }} }
                          { NewText = ""; Range = { Start = {  Line = 0; Character = 0 }; End = { Line = 0; Character = 0 }} }
                        |] }
                    |]
                  }
                } -> true
              | _ -> false)
            "should have found a matching diagnostic"
        | _ -> failwith "code actions should have returned actions for this file"
      | None ->
        failwith "checking should have failed for this file"
    ))
  ]


let codeFixTests toolsPath = testList "Code Fixes" [
  resolveNamespacesTests toolsPath
]
