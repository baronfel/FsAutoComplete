module FsAutoComplete.Tests.CoreTest

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers

///Test for initialization of the server
let initTests =
  testAsync "InitTest" {
    let (server, event) = createServer()

    let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some __SOURCE_DIRECTORY__
        RootUri = None
        InitializationOptions = Some (Server.serialize defaultConfigDto)
        Capabilities = Some clientCaps
        trace = None}

    let! result = server.Initialize p
    match result with
    | Result.Ok res ->
      Expect.equal res.Capabilities.CodeActionProvider (Some true) "Code Action Provider"
      Expect.equal res.Capabilities.CodeLensProvider (Some {CodeLensOptions.ResolveProvider = Some true}) "Code Lens Provider"
      Expect.equal res.Capabilities.DefinitionProvider (Some true) "Definition Provider"
      Expect.equal res.Capabilities.DocumentFormattingProvider (Some true) "Document Formatting Provider"
      Expect.equal res.Capabilities.DocumentHighlightProvider (Some true) "Document Highligthing Provider"
      Expect.equal res.Capabilities.DocumentLinkProvider None "Document Link Provider"
      Expect.equal res.Capabilities.DocumentOnTypeFormattingProvider None "Document OnType Formatting Provider"
      Expect.equal res.Capabilities.DocumentRangeFormattingProvider (Some false) "Document Range Formatting Provider"
      Expect.equal res.Capabilities.DocumentSymbolProvider (Some true) "Document Symbol Provider"
      Expect.equal res.Capabilities.ExecuteCommandProvider None "Execute Command Provider"
      Expect.equal res.Capabilities.Experimental None "Experimental"
      Expect.equal res.Capabilities.HoverProvider (Some true) "Hover Provider"
      Expect.equal res.Capabilities.ImplementationProvider (Some true) "Implementation Provider"
      Expect.equal res.Capabilities.ReferencesProvider (Some true) "References Provider"
      Expect.equal res.Capabilities.RenameProvider (Some true) "Rename Provider"
      Expect.equal res.Capabilities.SignatureHelpProvider (Some {SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]} ) "Signature Help Provider"
      let td =
        { TextDocumentSyncOptions.Default with
            OpenClose = Some true
            Change = Some TextDocumentSyncKind.Full
            Save = Some { IncludeText = Some true }
        }
      Expect.equal res.Capabilities.TextDocumentSync (Some td) "Text Document Provider"
      Expect.equal res.Capabilities.TypeDefinitionProvider (Some true) "Type Definition Provider"
      Expect.equal res.Capabilities.WorkspaceSymbolProvider (Some true) "Workspace Symbol Provider"
      Expect.equal res.Capabilities.FoldingRangeProvider (Some true) "Folding Range Provider active"
    | Result.Error e ->
      failwith "Initialization failed"
  }


///Tests for basic operations like hover, getting document symbols or code lens on simple file
let basicTests =

  let basicServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
    let session = serverInitialize path defaultConfigDto
    let projectPath = Path.Combine(path, "BasicTest.fsproj")
    parseProject projectPath session.Server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}

    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (session, path)
  )

  let basicTest = serverTest basicServer

  testSequenced <| testList "Basic Tests" [
      testSequenced <| testList "Hover Tests" [

        basicTest "simple symbol" (fun (session, path) -> async {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 4}}
          let! res = session.Server.TextDocumentHover p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val t : int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.t*"
                    MarkedString.String "*Assembly: BasicTest*"|]

            Expect.equal res.Contents expected "Hover test - simple symbol"
        })

        basicTest "let keyword" (fun (session, path) -> async {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 2}}
          let! res = session.Server.TextDocumentHover p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "Used to associate, or bind, a name to a value or function."|]

            Expect.equal res.Contents expected "Hover test - let keyword"
        })

        basicTest "Hover Tests - out of position" (fun (session, path) -> async {
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 1; Character = 2}}
          let! res = session.Server.TextDocumentHover p
          match res with
          | Result.Error e -> ()
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            failtest "Expected failure"
        })
      ]

      testSequenced <| testList "Document Symbol Tests" [
        basicTest "Document Symbol" (fun (session, path) -> async {
          let p : DocumentSymbolParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = session.Server.TextDocumentDocumentSymbol p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 2 "Document Symbol has all symbols"
        })
      ]

      testSequenced <| testList "Code Lens Tests" [
        basicTest "Get Code Lens" (fun (session, path) -> async {
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = session.Server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 1 "Get Code Lens has all locations"
        })

        basicTest "Resolve Code Lens" (fun (session, path) -> async {
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = session.Server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->
            let cl = res.[0]
            let! res = session.Server.CodeLensResolve cl
            match res with
            | Result.Error e -> failtestf "Request failed: %A" e
            | Result.Ok cl ->
              Expect.equal cl.Command.Value.Title "int" "Code Lens contains signature"
        })
      ]

  ]

///Tests for getting and resolving code(line) lenses with enabled reference code lenses
let codeLensTest =
  let codeLensStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
    let session = serverInitialize path {defaultConfigDto with EnableReferenceCodeLens = Some true}
    let projectPath = Path.Combine(path, "CodeLensTest.fsproj")
    parseProject projectPath session.Server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (session, path)
  )

  let codeLensTest = serverTest codeLensStart

  testSequenced <| testList "Code Lens Tests" [
      codeLensTest "Get Code Lens" (fun (session, path) -> async {
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = session.Server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some res) ->

            Expect.equal res.Length 18 "Get Code Lens has all locations"
      })

      codeLensTest "Resolve Code Lens" (fun (session, path) -> async {
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = session.Server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[0]
            let res = session.Server.CodeLensResolve cl |> Async.RunSynchronously
            let cl = result.[9]
            let res2 = session.Server.CodeLensResolve cl |> Async.RunSynchronously
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "string -> unit" "Code Lens contains signature"

            | e -> failtestf "Request failed: %A" e
      })

      codeLensTest "Resolve Code Lens 2" (fun (session, path) -> async {
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let! res = session.Server.TextDocumentCodeLens p
          match res with
          | Result.Error e -> failtestf "Request failed: %A" e
          | Result.Ok None -> failtest "Request none"
          | Result.Ok (Some result) ->
            let cl = result.[3]
            let! res = session.Server.CodeLensResolve cl
            let cl = result.[12]
            let! res2 = session.Server.CodeLensResolve cl
            match res, res2 with
            | Result.Ok cl, Result.Ok cl2 ->
              //TODO
              //Expect.equal cl.Command.Value.Title "1 Reference" "Code Lens contains reference count"
              Expect.equal cl2.Command.Value.Title "unit -> (int64 -> System.DateTime)" "Code Lens contains signature"

            | e -> failtestf "Request failed: %A" e
      })
  ]

///Tests for getting document symbols
let documentSymbolTest =
  let documentSymbolServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentSymbolTest")
    let session = serverInitialize path defaultConfigDto
    let projectPath = Path.Combine(path, "DocumentSymbolTest.fsproj")
    parseProject projectPath session.Server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (session, path)
  )

  let symbolTest = serverTest documentSymbolServer

  testSequenced <| testList "Document Symbols Tests" [
      symbolTest "Get Document Symbols" (fun (session, path) -> async {
        let p : DocumentSymbolParams = { TextDocument = { Uri = Path.FilePathToUri path}}
        let! res = session.Server.TextDocumentDocumentSymbol p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Length 15 "Document Symbol has all symbols"
          Expect.exists res (fun n -> n.Name = "MyDateTime" && n.Kind = SymbolKind.Class) "Document symbol contains given symbol"
      })
  ]

///Tests for getting autocomplete
let autocompleteTest =
  let autocompleteServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteTest")
    let session = serverInitialize path defaultConfigDto
    let projectPath = Path.Combine(path, "AutocompleteTest.fsproj")
    parseProject projectPath session.Server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (session, path)
  )

  let autocompleteTest = serverTest autocompleteServer

  let autocompleteScriptServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteScriptTest")
    let session = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing session.Events
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    (session, path)
  )

  let autocompleteScriptTest = serverTest autocompleteScriptServer

  let makeAutocompleteTestList (forScriptProject:bool) = [
    let test =
      if forScriptProject
      then autocompleteScriptTest
      else autocompleteTest

    test "Get Autocomplete module members" (fun (session, path) -> async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 8; Character = 2}
                                     Context = None }
        let! res = session.Server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 2 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "func") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
      })

    test "Get Autocomplete namespace" (fun (session, path) -> async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 10; Character = 2}
                                     Context = None }
        let! res = session.Server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "System") "Autocomplete contains given symbol"
      })

    test "Get Autocomplete namespace members" (fun (session, path) -> async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 12; Character = 7}
                                     Context = None }
        let! res = session.Server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"
      })

    test "Get Autocomplete module doublebackticked members" (fun (session, path) -> async {
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 14; Character = 18}
                                     Context = None }
        let! res = session.Server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->

          Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "z") "Autocomplete contains given symbol"
      })

    test "Autocomplete record members" (fun (session, path) -> async {
        let p : CompletionParams = {
          TextDocument = { Uri = Path.FilePathToUri path }
          Position = { Line = 25; Character = 4 }
          Context = None
        }
        let! res = session.Server.TextDocumentCompletion p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          Expect.exists res.Items (fun n -> n.Label = "bar") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "baz") "Autocomplete contains given symbol"
      })
  ]

  testSequenced (
    testList "Autocomplete Tests" [
      testList "Autocomplete within project files" (makeAutocompleteTestList false)
      testList "Autocomplete within script files" (makeAutocompleteTestList true)
    ]
  )

///Rename tests
let renameTest =
  let renameServer = lazy (
    let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest")
    let session = serverInitialize testDir defaultConfigDto

    let pathTest = Path.Combine(testDir, "Test.fs")
    let path = Path.Combine(testDir, "Program.fs")

    do waitForWorkspaceFinishedParsing session.Events

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument pathTest}
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    //Hack to wait for typechecking of 2 opened files
    System.Threading.Thread.Sleep 1000

    (session, path, pathTest)
  )

  let renameTest = serverTest renameServer

  testSequenced <| testList "Rename Tests" [
      renameTest "Rename from usage" (fun (session, path, _) -> async {
        let p : RenameParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                 Position = { Line = 7; Character = 12}
                                 NewName = "y" }
        let! res = session.Server.TextDocumentRename p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            Expect.equal result.Length 2 "Rename has all changes"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      })

      renameTest "Rename from definition" (fun (session, path, pathTest) -> async {
        let p : RenameParams = { TextDocument = { Uri = Path.FilePathToUri pathTest}
                                 Position = { Line = 2; Character = 4}
                                 NewName = "y" }
        let! res = session.Server.TextDocumentRename p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            // TODO
            Expect.equal result.Length 2 "Rename has all changes"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      })
  ]

///GoTo tests
let gotoTest =
  let gotoServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

    let session = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing session.Events
    System.Threading.Thread.Sleep 1000
    let definitionPath = Path.Combine(path, "Definition.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument definitionPath }
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let externalPath = Path.Combine(path, "External.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument externalPath }
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    let path = Path.Combine(path, "Library.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously

    //Hack to wait for typechecking of 3 opened files
    System.Threading.Thread.Sleep 1000

    (session, path, externalPath, definitionPath)
  )

  let gotoTest = serverTest gotoServer


  testSequenced <| testList "GoTo Tests" [
      gotoTest "Go-to-definition on external symbol (System.Net.HttpWebRequest)" (fun (session, path, externalPath, definitionPath) -> async {
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 4; Character = 30 }
        }

        let! res = session.Server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some (GotoResult.Multiple _)) -> failtest "Should only get one location"
        | Result.Ok (Some (GotoResult.Single r)) when r.Uri.EndsWith("startup") ->
          failtest "Should not generate the startup dummy file"
        | Result.Ok (Some (GotoResult.Single r)) ->
          Expect.stringEnds r.Uri ".cs" "should have generated a C# code file"
          Expect.stringContains r.Uri "System.Net.HttpWebRequest" "The generated file should be for the HttpWebRequest type"
          () // should
      })

      gotoTest "Go-to-definition on external namespace (System.Net) should error when going to a namespace " (fun (session, path, externalPath, definitionPath) -> async {
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 2; Character = 15 }
        }

        let! res = session.Server.TextDocumentDefinition p
        match res with
        | Result.Error e ->
          Expect.equal "Could not find declaration" e.Message "Should report failure for navigating to a namespace"
        | Result.Ok r -> failtestf "Declaration request should not work on a namespace, instead we got %A" r
      })

      gotoTest "Go-to-definition" (fun (session, path, externalPath, definitionPath) -> async {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 2; Character = 29}}
        let! res = session.Server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 16 }} "Result should have correct range"
      })

      gotoTest "Go-to-definition on custom type binding" (fun (session, path, externalPath, definitionPath) -> async {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let! res = session.Server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 6; Character = 4 }; End = {Line = 6; Character = 19 }} "Result should have correct range"
      })

      gotoTest "Go-to-implementation-on-interface-definition" (fun (session, path, externalPath, definitionPath) -> async {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri definitionPath}
            Position = { Line = 8; Character = 11}}
        let! res = session.Server.TextDocumentImplementation p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Single res -> failtest "Should be multiple GotoResult"
          | GotoResult.Multiple res ->
            // TODO???
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 7; Character = 8 }; End = {Line = 7; Character = 30 }}) "First result should be in Library.fs"
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 13; Character = 14 }; End = {Line = 13; Character = 36 }}) "Second result should be in Library.fs"
            ()
      })

      gotoTest "Go-to-implementation on sourcelink file with sourcelink in PDB" (fun (session, path, externalPath, definitionPath) -> async {
        // check for the 'button' member in giraffe view engine
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 9; Character = 34} }

        let! res = session.Server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "GiraffeViewEngine.fs" "Result should be in GiraffeViewEngine"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      gotoTest "Go-to-implementation on sourcelink file with sourcelink in DLL" (fun (session, path, externalPath, definitionPath) -> async {
        // check for the 'List.concat' member in FSharp.Core
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 36} }

        let! res = session.Server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/list.fs" "Result should be in FSharp.Core's list.fs"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      // marked pending because we don't have filename information for C# sources
      gotoTest "Go-to-implementation on C# file" (fun (session, path, externalPath, definitionPath) -> async {
        // check for the 'Stirng.Join' member in the BCL
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 14; Character = 79} }

        let! res = session.Server.TextDocumentDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            if localPath.Contains "System.String netstandard_ Version_2.0.0.0_ Culture_neutral_ PublicKeyToken_cc7b13ffcd2ddd51"
            then failwithf "should not decompile when sourcelink is available"
            Expect.stringContains localPath "System.String" "Result should be in the BCL's source files"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      gotoTest "Go-to-type-definition" (fun (session, path, externalPath, definitionPath) -> async {
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let! res = session.Server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 4; Character = 5 }; End = {Line = 4; Character = 6 }} "Result should have correct range"
      })

      gotoTest "Go-to-type-defintion on parameter" (fun (session, path, externalPath, definitionPath) -> async {
        // check for parameter of type `'a list` -> FSharp.Core
        (*
          `let myConcat listA listB = List.concat [listA; listB]`
                          ^
                          position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 16}}
        let! res = session.Server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      gotoTest "Go-to-type-defintion on variable" (fun (session, path, externalPath, definitionPath) -> async {
        // check for variable of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                 ^
                 position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 6}}
        let! res = session.Server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      gotoTest "Go-to-type-defintion on constructor" (fun (session, path, externalPath, definitionPath) -> async {
        // check for constructor of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                                                     ^
                                                     position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 42}}
        let! res = session.Server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      gotoTest "Go-to-type-defintion on union case" (fun (session, path, externalPath, definitionPath) -> async {
        // check for union case of type `_ option`
        (*
          `let o v = Some v`
                       ^
                       position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 18; Character = 12}}
        let! res = session.Server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })

      gotoTest "Go-to-type-defintion on property" (fun (session, path, externalPath, definitionPath) -> async {
        // check for property of type `string option`
        (*
          `b.Value |> ignore`
                ^
                position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 24; Character = 5}}
        let! res = session.Server.TextDocumentTypeDefinition p
        match res with
        | Result.Error e -> failtestf "Request failed: %A" e
        | Result.Ok None -> failtest "Request none"
        | Result.Ok (Some res) ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      })
  ]

let foldingTests =
  let foldingServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FoldingTests")

    let session = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing session.Events
    let libraryPath = Path.Combine(path, "Library.fs")
    let libFile = loadDocument libraryPath
    let tdop : DidOpenTextDocumentParams = { TextDocument = libFile }
    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    session, libraryPath
  )

  let foldingTest = serverTest foldingServer

  testList "folding tests" [
    foldingTest "can get ranges for sample file" (fun (session, libraryPath) -> async {
      let! rangeResponse = session.Server.TextDocumentFoldingRange({ TextDocument = { Uri = Path.FilePathToUri libraryPath } })
      match rangeResponse with
      | Ok(Some(ranges)) ->
        Expect.hasLength ranges 3 "Should be three ranges: one comment, one module, one let-binding"
      | Ok(None) -> failwithf "No ranges found in file, problem parsing?"
      | LspResult.Error e -> failwithf "Error from range LSP call: %A" e
    })
  ]

let tooltipTests =
  let (|Tooltip|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String newline; MarkedString.String fullname; MarkedString.String assembly |] } -> Some tooltip
    | _ -> None

  let (|Description|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String description; MarkedString.String fullname; MarkedString.String assembly |] } -> Some description
    | _ -> None

  let tooltipServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Tooltips")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let scriptPathUri = Path.FilePathToUri scriptPath
    let session = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing session.Events
    do session.Server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
    match session.WaitForParse(scriptPathUri, DiagnosticsVersion.Any) |> Async.RunSynchronously with
    | Ok () ->
      () // all good, no parsing/checking errors
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" scriptPath errors
    session, scriptPath
  )

  let tooltipTest = serverTest tooltipServer

  let verifyTooltip line character expectedTooltip =
    tooltipTest (sprintf "tooltip for line %d character %d should be '%s" line character expectedTooltip) (fun (session, scriptPath) -> async {
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match! session.Server.TextDocumentHover pos with
      | Ok (Some (Tooltip tooltip)) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | Ok _ ->
        failwithf "Should have gotten hover text"
      | Result.Error errors ->
        failwithf "Error while getting hover text: %A" errors
    })

  let verifyDescription line character expectedTooltip =
    tooltipTest (sprintf "tooltip for line %d character %d should be '%s" line character expectedTooltip) (fun (session, scriptPath) -> async {
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match! session.Server.TextDocumentHover pos with
      | Ok (Some (Description tooltip)) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | Ok _ ->
        failwithf "Should have gotten hover text"
      | Result.Error errors ->
        failwithf "Error while getting hover text: %A" errors
    })

  let concatLines = String.concat Environment.NewLine

  testList "tooltip evaluation" [
    verifyTooltip 0 4 "val arrayOfTuples : (int * int) array"
    verifyTooltip 1 4 "val listOfTuples : list<int * int>"
    verifyTooltip 2 4 "val listOfStructTuples : list<struct(int * int)>"
    verifyTooltip 3 4 "val floatThatShouldHaveGenericReportedInTooltip : float" //<MeasureOne>
    //verifyDescription 4 4 """**Description**\n\nPrint to a string using the given format.\n\n**Parameters**\n\n* `format`: The formatter.\n\n**Returns**\n\nThe formatted result.\n\n**Generic parameters**\n\n* `'T` is `string`"""
    verifyDescription 13 10 (concatLines ["**Description**"; ""; "\nMy super summary\n "; ""; "**Parameters**"; ""; "* `c`: foo"; "* `b`: bar"; "* `a`: baz"; ""; "**Returns**"; ""; ""])
  ]

let highlightingTests =
  let highlightingServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
    let session = serverInitialize path defaultConfigDto
    let projectPath = Path.Combine(path, "CodeLensTest.fsproj")
    parseProject projectPath session.Server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}

    do session.Server.TextDocumentDidOpen tdop |> Async.RunSynchronously
    session, path
  )

  let highlightingTest = serverTest highlightingServer

  highlightingTest "Document Highlighting" (fun (session, path) -> async {
    let p : HighlightingRequest = { FileName =  path}
    let! res = session.Server.GetHighlighting p
    printfn "%A" res
    ()
    // Expect.equal res.Length 2 "Document Symbol has all symbols"
  })

let signatureHelpTests =

  let signatureServer = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "SignatureHelpTest")
    let scriptPath = Path.Combine(path, "Script1.fsx")
    let scriptUri = Path.FilePathToUri scriptPath
    let session = serverInitialize path defaultConfigDto
    do waitForWorkspaceFinishedParsing session.Events
    do session.Server.TextDocumentDidOpen { TextDocument = loadDocument scriptPath } |> Async.RunSynchronously
    match session.WaitForParse(scriptUri, DiagnosticsVersion.Any) |> Async.RunSynchronously with
    | Ok () ->
      () // all good, no parsing/checking errors
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" scriptPath errors

    session, scriptPath
  )

  let signatureTest = serverTest signatureServer

  testSequenced <| testList "SignatureHelp" [
    signatureTest "signature help is also shown for overload without parameters" (fun (session, testFilePath) -> async {
        do! session.Server.TextDocumentDidOpen { TextDocument = loadDocument testFilePath }

        let getSignatureHelpAt line character = session.Server.TextDocumentSignatureHelp { TextDocument = { Uri = Path.FilePathToUri testFilePath }; Position = { Line = line; Character = character } }

        let expectSomeOverloads sigHelpLspRes =
          let sigHelp : SignatureHelp =
            sigHelpLspRes
            |> Flip.Expect.wantOk "Expected success SLP result"
            |> Flip.Expect.wantSome "Expected some signature help"
          sigHelp.Signatures |> Flip.Expect.isNonEmpty "Expected some overloads"

        // let __ = new System.IO.MemoryStream(|)
        let! result = getSignatureHelpAt 0 36
        result |> expectSomeOverloads

        // let ___ = new System.IO.MemoryStream (|||)
        for c in 38 .. 40 do
          let! result = getSignatureHelpAt 1 c
          result |> expectSomeOverloads

        // let _____ = new System.IO.MemoryStream(|4|2|)
        for c in 39 .. 41 do
          let! result = getSignatureHelpAt 2 c
          result |> expectSomeOverloads
    })
  ]
