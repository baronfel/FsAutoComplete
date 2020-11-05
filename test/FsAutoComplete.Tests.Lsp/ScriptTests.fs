module FsAutoComplete.Tests.ScriptTest

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers

let scriptPreviewTests =
  let scriptPreviewServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let session = serverInitialize path { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] }
    do waitForWorkspaceFinishedParsing session.Events
    session, scriptPath
  )

  let scriptTest = serverTest scriptPreviewServer

  testList "script preview language features" [
    scriptTest "can typecheck scripts when preview features are used" (fun (session, scriptPath) -> async {
      do! session.Server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }
      match! session.WaitForParse(Path.FilePathToUri scriptPath, DiagnosticsVersion.Any) with
      | Ok () ->
        () // all good, no parsing/checking errors
      | Core.Result.Error errors ->
        failwithf "Errors while parsing script %s: %A" scriptPath errors
    })
  ]

let scriptEvictionTests =
  let scriptEvictionServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "PreviewScriptFeatures")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let session = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing session.Events
    session, scriptPath
  )

  let scriptTest = serverTest scriptEvictionServer

  testList "script eviction tests" [
    scriptTest "can update script typechecking when arguments change" (fun (session, scriptPath) -> async {
      let openScript () = session.Server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }
      let scriptUri = Path.FilePathToUri scriptPath
      do! openScript ()
      match! session.WaitForParse(scriptUri, DiagnosticsVersion.Any) with
      | Ok () ->
        failwithf "Expected errors before we trigger script changes"
      | Core.Result.Error errors ->
        ()

      let configChange: DidChangeConfigurationParams =
        let config : FSharpConfigRequest =
          { FSharp = { defaultConfigDto with FSIExtraParameters = Some [| "--langversion:preview" |] } }
        { Settings = Server.serialize config }
      do! session.Server.WorkspaceDidChangeConfiguration configChange
      do! openScript()

      match! session.WaitForParse(scriptUri, DiagnosticsVersion.AtLeast 1) with
      | Ok () ->
        ()
      | Core.Result.Error errors ->
        failwithf "Should be no typecheck errors after we set the preview argument"
    })
  ]

let dependencyManagerTests =
  let depManagerServer = lazy (
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DependencyManagement")
    let dependencyManagerAssemblyDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "FsAutoComplete.DependencyManager.Dummy", "bin", "Debug", "netstandard2.0")
    let dependencyManagerEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let session = serverInitialize workingDir dependencyManagerEnabledConfig
    do waitForWorkspaceFinishedParsing session.Events
    session, workingDir
  )

  let depTest = serverTest depManagerServer

  testList "dependencyManager integrations" [
    depTest "can typecheck script that depends on #r dummy dependency manager" (fun (session, workingDir) -> async {
      let scriptName = "DepManagerPresentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      do! session.Server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }
      match! session.WaitForParse(Path.FilePathToUri scriptPath, DiagnosticsVersion.Any) with
      | Ok _ -> ()
      | Core.Result.Error e ->
        failwithf "Error during typechecking: %A" e
    })

    depTest "fails to typecheck script when dependency manager is missing" (fun (session, workingDir) -> async {
      let scriptName = "DepManagerAbsentScript.fsx"
      let scriptPath = Path.Combine(workingDir, scriptName)
      let scriptUri = Path.FilePathToUri scriptPath
      do! session.Server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath }

      match! session.WaitForParse(scriptUri, DiagnosticsVersion.Any) with
      | Ok _ ->
        failwith "Expected to fail typechecking a script with a dependency manager that's missing"
      | Core.Result.Error e ->
        match e with
        | [| { Code = Some "3216" }; _ |] -> () // this is the error code that signals a missing dependency manager, so this is a 'success'
        | e -> failwithf "Unexpected error during typechecking: %A" e
    })
  ]

let scriptProjectOptionsCacheTests =
  let projOptionsServer = lazy (
    let workingDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "ScriptProjectOptsCache")
    let previewEnabledConfig =
      { defaultConfigDto with
          FSIExtraParameters = Some [| "--langversion:preview" |] }
    let session = serverInitialize workingDir previewEnabledConfig
    let scriptPath = Path.Combine(workingDir, "Script.fsx")
    do waitForWorkspaceFinishedParsing session.Events
    session, workingDir, scriptPath
  )

  let projOptionsTest = serverTest projOptionsServer

  testList "ScriptProjectOptionsCache" [
    projOptionsTest "reopening the script file should return same project options for file" (fun (session, workingDir, testFilePath) -> async {
      waitForScriptFileProjectOptions session
      do! session.Server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath }
      do System.Threading.Thread.Sleep 3000
      do! session.Server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath }
      do System.Threading.Thread.Sleep 3000

      let opts1 = projectOptsList.[0]
      let opts2 = projectOptsList.[1]

      Expect.equal opts1 opts2 "Project opts should be eqaul"
    })
  ]

let scriptGotoTests =
  let scriptGoToServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")
    let session = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing session.Events

    let scriptPath = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    (session, scriptPath)
  )

  let scriptTest = serverTest scriptGoToServer

  testSequenced <| testList "Script GoTo Tests" [
    scriptTest "Go-to-definition on #load integration test" (fun (session, scriptPath) -> async {
      let p : TextDocumentPositionParams = {
        TextDocument = { Uri = Path.FilePathToUri scriptPath }
        Position = { Line = 0; Character = 10 }
      }
      let! res = session.Server.TextDocumentDefinition p
      match res with
      | Error e -> failtestf "Request failed: %A" e
      | Ok None -> failtest "Request none"
      | Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
      | Ok (Some (GotoResult.Single r)) ->
        Expect.stringEnds r.Uri "/simple.fsx" "should navigate to the mentioned script file"
        ()
    })
  ]
