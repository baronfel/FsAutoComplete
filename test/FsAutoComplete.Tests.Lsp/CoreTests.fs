module FsAutoComplete.Tests.CoreTest

open System
open Expecto
open System.IO
open LanguageServerProtocol
open LanguageServerProtocol.Types
open FsAutoComplete
open FsAutoComplete.LspHelpers
open Helpers
open System.Threading
open System.Threading.Tasks

///Test for initialization of the server
let initTests toolsPath workspaceLoaderFactory =
  test "InitTest" {
    let (server, event) = createServer toolsPath workspaceLoaderFactory

    let p : InitializeParams =
      { ProcessId = Some 1
        RootPath = Some __SOURCE_DIRECTORY__
        RootUri = None
        InitializationOptions = Some (Server.serialize defaultConfigDto)
        Capabilities = Some clientCaps
        trace = None}

    let res = server.Initialize(p, CancellationToken.None).GetAwaiter().GetResult()
    Expect.equal res.Capabilities.CodeActionProvider (Some true) "Code Action Provider"
    Expect.equal res.Capabilities.CodeLensProvider (Some {CodeLensOptions.ResolveProvider = Some true}) "Code Lens Provider"
    Expect.equal res.Capabilities.DefinitionProvider (Some true) "Definition Provider"
    Expect.equal res.Capabilities.DocumentFormattingProvider (Some true) "Document Formatting Provider"
    Expect.equal res.Capabilities.DocumentHighlightProvider (Some true) "Document Highlighting Provider"
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
    Expect.equal res.Capabilities.SignatureHelpProvider (Some {
      TriggerCharacters = Some [| '('; ','|]
      RetriggerCharacters = Some [| ')' |]
    } ) "Signature Help Provider"
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
  }


///Tests for basic operations like hover, getting document symbols or code lens on simple file
let basicTests toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "BasicTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    let projectPath = Path.Combine(path, "BasicTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}

    do server.TextDocumentDidOpen(tdop, CancellationToken.None).Wait()
    (server, path)
    )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  /// normalizes the line endings in returned markdown strings for cross-platform comparisons
  let normalizeMarkedString = function | MarkedString.WithLanguage l -> MarkedString.WithLanguage l
                                       | MarkedString.String s -> MarkedString.String (s.Replace("\r\n", "\n"))

  let normalizeHoverContent = function | HoverContent.MarkedStrings strings -> MarkedStrings (strings |> Array.map normalizeMarkedString)
                                       | HoverContent.MarkedString str -> MarkedString (normalizeMarkedString str)
                                       | HoverContent.MarkupContent content -> MarkupContent content

  testSequenced <| testList "Basic Tests" [
      testSequenced <| testList "Hover Tests" [

        testCase "Hover Tests - simple symbol" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 4}}
          let res = server.TextDocumentHover(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val t : int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.t*"
                    MarkedString.String "*Assembly: BasicTest*"|]

            Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - simple symbol"
        ))

        testCase "Hover Tests - let keyword" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 0; Character = 2}}
          let res = server.TextDocumentHover(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "let"}
                    MarkedString.String "**Description**\n\n\nUsed to associate, or bind, a name to a value or function.\n"|]

            Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - let keyword"
        ))

        testCase "Hover Tests - out of position" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 1; Character = 2}}
          Expect.throws (fun () -> server.TextDocumentHover(p, CancellationToken.None).Wait()) "should fail when out of position"
        ))

        //Test to reproduce: https://github.com/ionide/ionide-vscode-fsharp/issues/1203
        testCase "Hover Tests - operator" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 2; Character = 7}}
          let res = server.TextDocumentHover(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val ( .>> ): \n   x: int ->\n   y: int \n   -> int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.( .>> )*"
                    MarkedString.String "*Assembly: BasicTest*"|]

            Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - let keyword"
        ))

        //Test to reproduce: https://github.com/ionide/ionide-vscode-fsharp/issues/1203
        testCase "Hover Tests - operator ^" (serverTest (fun server path ->
          let p : TextDocumentPositionParams =
            { TextDocument = { Uri = Path.FilePathToUri path}
              Position = { Line = 4; Character = 6}}
          let res = server.TextDocumentHover(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            let expected =
              MarkedStrings
                [|  MarkedString.WithLanguage {Language = "fsharp"; Value = "val ( ^ ): \n   x: int ->\n   y: int \n   -> int"}
                    MarkedString.String ""
                    MarkedString.String "*Full name: Script.( ^ )*"
                    MarkedString.String "*Assembly: BasicTest*"|]

            Expect.equal (normalizeHoverContent res.Contents) expected "Hover test - let keyword"
        ))
      ]
      testSequenced <| testList "Document Symbol Tests" [
        testCase "Document Symbol" (serverTest (fun server path ->
          let p : DocumentSymbolParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentDocumentSymbol(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            Expect.equal res.Length 4  "Document Symbol has all symbols"
        ))
      ]
      testSequenced <| testList "Code Lens Tests" [
        testCase "Get Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            Expect.equal res.Length 3 "Get Code Lens has all locations"
        ))

        testCase "Resolve Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            let lensToResolve = res.[0]
            let cl = server.CodeLensResolve(lensToResolve, CancellationToken.None).GetAwaiter().GetResult()
            Expect.equal cl.Command.Value.Title "int -> int -> int" "Code Lens contains signature"
        ))
      ]
  ]

///Tests for getting and resolving code(line) lenses with enabled reference code lenses
let codeLensTest toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "CodeLensTest")
    let (server, event) = serverInitialize path {defaultConfigDto with EnableReferenceCodeLens = Some true} toolsPath workspaceLoaderFactory
    let projectPath = Path.Combine(path, "CodeLensTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()
    (server, path)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Code Lens Tests" [
      testCase "Get Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some res ->
            Expect.equal res.Length 20 "Get Code Lens has all locations"
      ))

      testCase "Resolve Code Lens" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some result ->
            let cl = result.[1]
            let cl1 = server.CodeLensResolve(cl, CancellationToken.None).GetAwaiter().GetResult()
            let cl = result.[11]
            let cl2 = server.CodeLensResolve(cl, CancellationToken.None).GetAwaiter().GetResult()
            let cl = result.[10]
            let cl3 = server.CodeLensResolve(cl, CancellationToken.None).GetAwaiter().GetResult()
            //TODO
            //Expect.equal cl1.Command.Value.Title "1 Reference" "Code Lens contains reference count"
            Expect.equal cl2.Command.Value.Title "string -> unit" "Code Lens contains signature"
      ))

      testCase "Resolve Code Lens 2" (serverTest (fun server path ->
          let p : CodeLensParams = { TextDocument = { Uri = Path.FilePathToUri path}}
          let res = server.TextDocumentCodeLens(p, CancellationToken.None).GetAwaiter().GetResult()
          match res with
          | None -> failtest "Request none"
          | Some result ->
            let cl = result.[3]
            let cl1 = server.CodeLensResolve(cl, CancellationToken.None).GetAwaiter().GetResult()
            let cl = result.[14]
            let cl2 = server.CodeLensResolve(cl, CancellationToken.None).GetAwaiter().GetResult()
            //TODO
            //Expect.equal cl1.Command.Value.Title "1 Reference" "Code Lens contains reference count"
            Expect.equal cl2.Command.Value.Title "unit -> (int64 -> System.DateTime)" "Code Lens contains signature"
      ))
  ]

///Tests for getting document symbols
let documentSymbolTest toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "DocumentSymbolTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    let projectPath = Path.Combine(path, "DocumentSymbolTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()
    (server, path)
  )
  let serverTest f () =
    let (server, path) = serverStart.Value
    f server path

  testSequenced <| testList "Document Symbols Tests" [
      testCase "Get Document Symbols" (serverTest (fun server path ->
        let p : DocumentSymbolParams = { TextDocument = { Uri = Path.FilePathToUri path}}
        let res = server.TextDocumentDocumentSymbol(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->

          Expect.equal res.Length 15 "Document Symbol has all symbols"
          Expect.exists res (fun n -> n.Name = "MyDateTime" && n.Kind = SymbolKind.Class) "Document symbol contains given symbol"
      ))
  ]

///Tests for getting autocomplete
let autocompleteTest toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    let projectPath = Path.Combine(path, "AutocompleteTest.fsproj")
    parseProject projectPath server |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()
    (server, path)
  )

  let scriptProjServerStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "AutocompleteScriptTest")
    let (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing event |> Async.RunSynchronously
    let path = Path.Combine(path, "Script.fsx")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()
    (server, path)
  )

  let makeAutocompleteTestList (serverConfig: (LspServer.FSharpLspServer * string) Lazy) = [
    testCaseAsync "Get Autocomplete module members" (
      async {
        let server, path = serverConfig.Value
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 8; Character = 2}
                                     Context = None }
        let! ctok = Async.CancellationToken
        let! res = server.TextDocumentCompletion(p, ctok) |> Async.AwaitTask
        match res with

        | None -> failtest "Request none"
        | Some res ->

          Expect.equal res.Items.Length 2 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "func") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "sample func") "Autocomplete contains given symbol"
      })

    testCaseAsync "Get Autocomplete namespace" (
      async {
        let server, path = serverConfig.Value
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 10; Character = 2}
                                     Context = None }
        let! ctok = Async.CancellationToken
        let! res = server.TextDocumentCompletion(p, ctok) |> Async.AwaitTask
        match res with

        | None -> failtest "Request none"
        | Some res ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "System") "Autocomplete contains given symbol"

      })

    testCaseAsync "Get Autocomplete namespace members" (
      async {
        let server, path = serverConfig.Value
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 12; Character = 7}
                                     Context = None }
        let! ctok = Async.CancellationToken
        let! res = server.TextDocumentCompletion(p, ctok) |> Async.AwaitTask
        match res with

        | None -> failtest "Request none"
        | Some res ->
          //TODO
          // Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "DateTime") "Autocomplete contains given symbol"

      })

    testCaseAsync "Get Autocomplete module doublebackticked members" (
      async {
        let server, path = serverConfig.Value
        let p : CompletionParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                     Position = { Line = 14; Character = 18}
                                     Context = None }
        let! ctok = Async.CancellationToken
        let! res = server.TextDocumentCompletion(p, ctok) |> Async.AwaitTask
        match res with
        | None -> failtest "Request none"
        | Some res ->

          Expect.equal res.Items.Length 1 "Autocomplete has all symbols"
          Expect.exists res.Items (fun n -> n.Label = "z") "Autocomplete contains given symbol"
      })

    testCaseAsync "Autocomplete record members" (
      async {
        let server, path = serverConfig.Value
        let p : CompletionParams = {
          TextDocument = { Uri = Path.FilePathToUri path }
          Position = { Line = 25; Character = 4 }
          Context = None
        }
        let! ctok = Async.CancellationToken
        let! res = server.TextDocumentCompletion(p, ctok) |> Async.AwaitTask
        match res with

        | None -> failtest "Request none"
        | Some res ->
          Expect.exists res.Items (fun n -> n.Label = "bar") "Autocomplete contains given symbol"
          Expect.exists res.Items (fun n -> n.Label = "baz") "Autocomplete contains given symbol"
      })

    testCaseAsync "Autocomplete class constructor with properties" (
      async {
        let server, path = serverConfig.Value
        let p : CompletionParams = {
          TextDocument = { Uri = Path.FilePathToUri path }
          Position = { Line = 32; Character = 26 }
          Context = None
        }
        let! ctok = Async.CancellationToken
        let! res = server.TextDocumentCompletion(p, ctok) |> Async.AwaitTask
        match res with

        | None -> failtest "Request none"
        | Some res ->
          Expect.isTrue ((res.Items |> Seq.findIndex (fun n -> n.Label = "Bar")) < 2) "Autocomplete contains given symbol"
          Expect.isTrue ((res.Items |> Seq.findIndex (fun n -> n.Label = "Baz")) < 2) "Autocomplete contains given symbol"
      })
  ]

  testSequenced (
    testList "Autocomplete Tests" [
      testList "Autocomplete within project files" (makeAutocompleteTestList serverStart)
      testList "Autocomplete within script files" (makeAutocompleteTestList scriptProjServerStart)
    ]
  )

///Rename tests
let renameTest toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let testDir = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "RenameTest")
    let (server, event) = serverInitialize testDir defaultConfigDto toolsPath workspaceLoaderFactory

    let pathTest = Path.Combine(testDir, "Test.fs")
    let path = Path.Combine(testDir, "Program.fs")

    do waitForWorkspaceFinishedParsing event |> Async.RunSynchronously

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument pathTest}
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()

    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path}
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()

    //Hack to wait for typechecking of 2 opened files
    System.Threading.Thread.Sleep 1000

    (server, path, pathTest) )

  let serverTest f () =
    let (server, path, pathTest) = serverStart.Value
    f server path pathTest

  testSequenced <| testList "Rename Tests" [
      testCase "Rename from usage" (serverTest (fun server path _ ->

        let p : RenameParams = { TextDocument = { Uri = Path.FilePathToUri path}
                                 Position = { Line = 7; Character = 12}
                                 NewName = "y" }

        let res = server.TextDocumentRename(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            Expect.equal result.Length 2 "Rename has all changes"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      ))

      testCase "Rename from definition" (serverTest (fun server path pathTest ->
        let p : RenameParams = { TextDocument = { Uri = Path.FilePathToUri pathTest}
                                 Position = { Line = 2; Character = 4}
                                 NewName = "y" }
        let res = server.TextDocumentRename(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res.DocumentChanges with
          | None -> failtest "No changes"
          | Some result ->
            // TODO
            Expect.equal result.Length 2 "Rename has all changes"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Program.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 7; Character = 12 }; End = {Line = 7; Character = 13 } }) ) "Rename contains changes in Program.fs"
            Expect.exists result (fun n -> n.TextDocument.Uri.Contains "Test.fs" && n.Edits |> Seq.exists (fun r -> r.Range = { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 5 } }) ) "Rename contains changes in Test.fs"
            ()
      ))

  ]

///GoTo tests
let gotoTest toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "GoToTests")

    let (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing event |> Async.RunSynchronously
    System.Threading.Thread.Sleep 1000
    let definitionPath = Path.Combine(path, "Definition.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument definitionPath }
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()

    let externalPath = Path.Combine(path, "External.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument externalPath }
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()

    let path = Path.Combine(path, "Library.fs")
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument path }
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()

    //Hack to wait for typechecking of 3 opened files
    System.Threading.Thread.Sleep 1000

    (server, path, externalPath, definitionPath)
  )
  let serverTest f () =
    let (server, path, externalPath, definitionPath) = serverStart.Value
    f server path externalPath definitionPath

  testSequenced <| testList "GoTo Tests" [
      testCase "Go-to-definition on external symbol (System.Net.HttpWebRequest)" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 4; Character = 30 }
        }

        let res = server.TextDocumentDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some (GotoResult.Multiple _) -> failtest "Should only get one location"
        | Some (GotoResult.Single r) when r.Uri.EndsWith("startup") ->
          failtest "Should not generate the startup dummy file"
        | Some (GotoResult.Single r) ->
          Expect.stringEnds r.Uri ".cs" "should have generated a C# code file"
          Expect.stringContains r.Uri "System.Net.HttpWebRequest" "The generated file should be for the HttpWebRequest type"
      ))

      testCase "Go-to-definition on external namespace (System.Net) should error when going to a namespace " (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams = {
          TextDocument = { Uri = Path.FilePathToUri externalPath }
          Position = { Line = 2; Character = 15 }
        }

        Expect.throws (fun _ -> server.TextDocumentDefinition(p, CancellationToken.None).Wait()) "Should report failure for navigating to a namespace"
      ))

      testCase "Go-to-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 2; Character = 29}}
        let res = server.TextDocumentDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 2; Character = 4 }; End = {Line = 2; Character = 16 }} "Result should have correct range"
      ))

      testCase "Go-to-definition on custom type binding" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let res = server.TextDocumentDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 6; Character = 4 }; End = {Line = 6; Character = 19 }} "Result should have correct range"
      ))

      testCase "Go-to-implementation-on-interface-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri definitionPath}
            Position = { Line = 8; Character = 11}}
        let res = server.TextDocumentImplementation(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Single res -> failtest "Should be multiple GotoResult"
          | GotoResult.Multiple res ->
            // TODO???
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 7; Character = 8 }; End = {Line = 7; Character = 30 }}) "First result should be in Library.fs"
            // Expect.exists res (fun r -> r.Uri.Contains "Library.fs" && r.Range = { Start = {Line = 13; Character = 14 }; End = {Line = 13; Character = 36 }}) "Second result should be in Library.fs"
            ()
      ))

      testCase "Go-to-implementation on sourcelink file with sourcelink in PDB" (serverTest (fun server path externalPath definitionPath ->
        // check for the 'button' member in giraffe view engine
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 9; Character = 34} }

        let res = server.TextDocumentDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "GiraffeViewEngine.fs" "Result should be in GiraffeViewEngine"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-implementation on sourcelink file with sourcelink in DLL" (serverTest (fun server path externalPath definitionPath ->
        // check for the 'List.concat' member in FSharp.Core
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 36} }

        let res = server.TextDocumentDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/list.fs" "Result should be in FSharp.Core's list.fs"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      // marked pending because we don't have filename information for C# sources
      ptestCase "Go-to-implementation on C# file" (serverTest (fun server path externalPath definitionPath ->
        // check for the 'Stirng.Join' member in the BCL
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 14; Character = 79} }

        let res = server.TextDocumentDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            if localPath.Contains "System.String netstandard_ Version_2.0.0.0_ Culture_neutral_ PublicKeyToken_cc7b13ffcd2ddd51"
            then failwithf "should not decompile when sourcelink is available"
            Expect.stringContains localPath "System.String" "Result should be in the BCL's source files"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-definition" (serverTest (fun server path externalPath definitionPath ->
        let p : TextDocumentPositionParams  =
          { TextDocument = { Uri = Path.FilePathToUri path}
            Position = { Line = 4; Character = 24}}
        let res = server.TextDocumentTypeDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "Definition.fs" "Result should be in Definition.fs"
            Expect.equal res.Range { Start = {Line = 4; Character = 5 }; End = {Line = 4; Character = 6 }} "Result should have correct range"
      ))

      testCase "Go-to-type-defintion on parameter" (serverTest (fun server path externalPath definitionPath ->
        // check for parameter of type `'a list` -> FSharp.Core
        (*
          `let myConcat listA listB = List.concat [listA; listB]`
                          ^
                          position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 12; Character = 16}}
        let res = server.TextDocumentTypeDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on variable" (serverTest (fun server path externalPath definitionPath ->
        // check for variable of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                 ^
                 position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 6}}
        let res = server.TextDocumentTypeDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on constructor" (serverTest (fun server path externalPath definitionPath ->
        // check for constructor of type `System.Collections.Generic.List<_>`
        (*
          `let myList = System.Collections.Generic.List<string>()`
                                                     ^
                                                     position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 16; Character = 42}}
        let res = server.TextDocumentTypeDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.stringContains res.Uri "System.Collections.Generic.List" "Result should be for System.Collections.Generic.List"
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on union case" (serverTest (fun server path externalPath definitionPath ->
        // check for union case of type `_ option`
        (*
          `let o v = Some v`
                       ^
                       position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 18; Character = 12}}
        let res = server.TextDocumentTypeDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))

      testCase "Go-to-type-defintion on property" (serverTest (fun server path externalPath definitionPath ->
        // check for property of type `string option`
        (*
          `b.Value |> ignore`
                ^
                position
        *)
        let p: TextDocumentPositionParams =
          { TextDocument = { Uri = Path.FilePathToUri externalPath}
            Position = { Line = 24; Character = 5}}
        let res = server.TextDocumentTypeDefinition(p, CancellationToken.None).GetAwaiter().GetResult()
        match res with

        | None -> failtest "Request none"
        | Some res ->
          match res with
          | GotoResult.Multiple _ -> failtest "Should be single GotoResult"
          | GotoResult.Single res ->
            Expect.stringContains res.Uri "FSharp.Core/prim-types" "Result should be in FSharp.Core's prim-types"
            let localPath = Path.FileUriToLocalPath res.Uri
            Expect.isTrue (System.IO.File.Exists localPath) (sprintf "File '%s' should exist locally after being downloaded" localPath)
      ))
  ]


let foldingTests toolsPath workspaceLoaderFactory=
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "FoldingTests")

    let (server, event) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing event |> Async.RunSynchronously
    let libraryPath = Path.Combine(path, "Library.fs")
    let libFile = loadDocument libraryPath
    let tdop : DidOpenTextDocumentParams = { TextDocument = libFile }
    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()
    server, libraryPath
  )
  let serverTest f () = f serverStart.Value
  testList "folding tests" [
    testCase "can get ranges for sample file" (serverTest (fun (server, libraryPath) ->
      let rangeResponse = server.TextDocumentFoldingRange({ TextDocument = { Uri = Path.FilePathToUri libraryPath } }, CancellationToken.None).GetAwaiter().GetResult()
      match rangeResponse with
      | Some(ranges) ->
        Expect.hasLength ranges 3 "Should be three ranges: one comment, one module, one let-binding"
      | None -> failwithf "No ranges found in file, problem parsing?"
    ))
  ]


let tooltipTests toolsPath workspaceLoaderFactory =
  let (|Tooltip|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String newline; MarkedString.String fullname; MarkedString.String assembly |] } -> Some tooltip
    | _ -> None

  let (|Description|_|) (hover: Hover) =
    match hover with
    | { Contents = MarkedStrings [| MarkedString.WithLanguage { Language = "fsharp"; Value = tooltip }; MarkedString.String description; MarkedString.String fullname; MarkedString.String assembly |] } -> Some description
    | _ -> None

  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "Tooltips")
    let scriptPath = Path.Combine(path, "Script.fsx")
    let (server, events) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing events |> Async.RunSynchronously
    do server.TextDocumentDidOpen({ TextDocument = loadDocument scriptPath }, CancellationToken.None) |> ignore<Task<unit>>
    match waitForParseResultsForFile "Script.fsx" events with
    | Ok () ->
      () // all good, no parsing/checking errors
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" scriptPath errors

    server, scriptPath
  )

  let verifyTooltip line character expectedTooltip =
    testCase (sprintf "tooltip for line %d character %d should be '%s" line character expectedTooltip) (fun _ ->
      let server, scriptPath = serverStart.Value
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match server.TextDocumentHover(pos, CancellationToken.None).GetAwaiter().GetResult() with
      | Some (Tooltip tooltip) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | _ ->
        failwithf "Should have gotten hover text"
    )

  let verifyDescription line character expectedTooltip =
    testCase (sprintf "description for line %d character %d should be '%s" line character expectedTooltip) (fun _ ->
      let server, scriptPath = serverStart.Value
      let pos: TextDocumentPositionParams = {
        TextDocument =  { Uri = sprintf "file://%s" scriptPath }
        Position = { Line = line; Character = character }
      }
      match server.TextDocumentHover(pos, CancellationToken.None).GetAwaiter().GetResult() with
      | Some (Description tooltip) ->
        Expect.equal tooltip expectedTooltip (sprintf "Should have a tooltip of '%s'" expectedTooltip)
      | _ ->
        failwithf "Should have gotten hover text"
    )

  let concatLines = String.concat Environment.NewLine

  testList "tooltip evaluation" [
    verifyTooltip 0 4 "val arrayOfTuples : (int * int) array"
    verifyTooltip 1 4 "val listOfTuples : list<int * int>"
    verifyTooltip 2 4 "val listOfStructTuples : list<struct(int * int)>"
    verifyTooltip 3 4 "val floatThatShouldHaveGenericReportedInTooltip : float" //<MeasureOne>
    //verifyDescription 4 4 """**Description**\n\nPrint to a string using the given format.\n\n**Parameters**\n\n* `format`: The formatter.\n\n**Returns**\n\nThe formatted result.\n\n**Generic parameters**\n\n* `'T` is `string`"""
    verifyDescription 13 10 (concatLines ["**Description**"; ""; "\nMy super summary\n "; ""; "**Parameters**"; ""; "* `c`: foo"; "* `b`: bar"; "* `a`: baz"; ""; "**Returns**"; ""; ""])
    verifyTooltip 14 4 "val nestedTuples : int * ((int * int) * int)"
    verifyTooltip 15 4 "val nestedStructTuples : int * struct(int * int)"
  ]


let highlightingTests toolsPath workspaceLoaderFactory =
  let testPath = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "HighlightingTest")
  let scriptPath = Path.Combine(testPath, "Script.fsx")

  let serverParsed = lazy (
    let (server, event) = serverInitialize testPath defaultConfigDto toolsPath workspaceLoaderFactory
    let tdop : DidOpenTextDocumentParams = { TextDocument = loadDocument scriptPath }

    do server.TextDocumentDidOpen(tdop, CancellationToken.None).GetAwaiter().GetResult()
    match waitForParseResultsForFile "Script.fsx" event with
    | Ok () -> server
    | Error e -> failwithf "Errors while parsing highlighting script %A" e
  )

  let decodeHighlighting (data: uint32 []) =
    let zeroLine = [| 0u; 0u; 0u; 0u; 0u |]

    let lines =
      Array.append [| zeroLine |] (Array.chunkBySize 5 data)

    let structures =
      let mutable lastLine = 0
      let mutable lastCol = 0
      lines
      |> Array.map (fun current ->
        let startLine = lastLine + int current.[0]
        let startCol = if current.[0] = 0u then lastCol + int current.[1] else int current.[1]
        let endLine = int startLine // assuming no multiline for now
        let endCol = startCol + int current.[2]
        lastLine <- startLine
        lastCol <- startCol
        let tokenType = enum<ClassificationUtils.SemanticTokenTypes> (int current.[3])
        let tokenMods = enum<ClassificationUtils.SemanticTokenModifier> (int current.[4])
        let range =
          { Start = { Line = startLine; Character = startCol }
            End   = { Line = endLine; Character = endCol }}
        range, tokenType, tokenMods
      )

    structures

  let fullHighlights = lazy (
    let p : SemanticTokensParams = { TextDocument = { Uri = Path.FilePathToUri scriptPath } }
    let highlights = serverParsed.Value.TextDocumentSemanticTokensFull(p, CancellationToken.None).GetAwaiter().GetResult()
    match highlights with
    | Some highlights ->
      let decoded =
        highlights.Data
        |> decodeHighlighting
      // printfn "%A" decoded
      decoded
    | None ->
      failwithf "Expected to get some highlighting"
  )

  let rangeContainsRange (parent: Types.Range) (child: Types.Position) =
    parent.Start.Line <= child.Line &&
    parent.Start.Character <= child.Character &&
    parent.End.Line >= child.Line &&
    parent.End.Character >= child.Character

  let tokenIsOfType ((line, char) as pos) testTokenType (highlights: (Types.Range * ClassificationUtils.SemanticTokenTypes * ClassificationUtils.SemanticTokenModifier) [] Lazy) =
    testCase $"can find token of type {testTokenType} at %A{pos}" (fun () ->
      let pos = { Line = line; Character = char }
      Expect.exists
        highlights.Value
        ((fun (r, token, _modifiers) ->
          rangeContainsRange r pos
          && token = testTokenType))
        "Could not find a highlighting range that contained the given position"
    )

  /// this tests the range endpoint by getting highlighting for a range then doing the normal highlighting test
  let tokenIsOfTypeInRange ((startLine, startChar), (endLine, endChar)) ((line, char)) testTokenType (server: FsAutoComplete.LspServer.FSharpLspServer Lazy) =
    testCase $"can find token of type {testTokenType} in a subrange from ({startLine}, {startChar})-({endLine}, {endChar})" (fun () ->
      let range: Types.Range =
        { Start = { Line = startLine; Character = startChar}
          End = { Line = endLine; Character = endChar }}
      let pos = { Line = line; Character = char }
      match server.Value.TextDocumentSemanticTokensRange({ Range = range; TextDocument =  { Uri = Path.FilePathToUri scriptPath } }, CancellationToken.None).GetAwaiter().GetResult() with
      | Some highlights ->
        let decoded = decodeHighlighting highlights.Data
        Expect.exists
          decoded
          (fun (r, token, _modifiers) ->
            rangeContainsRange r pos
            && token = testTokenType)
          "Could not find a highlighting range that contained the given position"
      | None -> failwithf "Expected to get some highlighting"
    )

  testList "Document Highlighting Tests" [
    tokenIsOfType (0, 29) ClassificationUtils.SemanticTokenTypes.TypeParameter fullHighlights // the `^a` type parameter in the SRTP constraint
    tokenIsOfType (0, 44) ClassificationUtils.SemanticTokenTypes.Member fullHighlights // the `PeePee` member in the SRTP constraint
    tokenIsOfType (3, 52) ClassificationUtils.SemanticTokenTypes.Type fullHighlights // the `string` type annotation in the PooPoo srtp member
    tokenIsOfType (6, 21) ClassificationUtils.SemanticTokenTypes.EnumMember fullHighlights // the `PeePee` AP application in the `yeet` function definition
    tokenIsOfType (14, 10) ClassificationUtils.SemanticTokenTypes.Type fullHighlights //the `SomeJson` type should be a type
    tokenIsOfTypeInRange ((0, 0), (0, 100)) (0, 29) ClassificationUtils.SemanticTokenTypes.TypeParameter serverParsed
  ]

let signatureHelpTests toolsPath workspaceLoaderFactory =
  let serverStart = lazy (
    let path = Path.Combine(__SOURCE_DIRECTORY__, "TestCases", "SignatureHelpTest")
    let scriptPath = Path.Combine(path, "Script1.fsx")
    let (server, events) = serverInitialize path defaultConfigDto toolsPath workspaceLoaderFactory
    do waitForWorkspaceFinishedParsing events |> Async.RunSynchronously
    do server.TextDocumentDidOpen({ TextDocument = loadDocument scriptPath }, CancellationToken.None).Wait()
    match waitForParseResultsForFile "Script1.fsx" events with
    | Ok () ->
      () // all good, no parsing/checking errors
    | Core.Result.Error errors ->
      failwithf "Errors while parsing script %s: %A" scriptPath errors
    do server.TextDocumentDidOpen({ TextDocument = loadDocument scriptPath }, CancellationToken.None).Wait()
    server, scriptPath
  )

  let getSignatureHelpAt (line, character) file =
    let sigHelpParams: SignatureHelpParams =
      { TextDocument = { Uri = Path.FilePathToUri file }
        Position = { Line = line; Character = character }
        Context = Some {
          TriggerKind = SignatureHelpTriggerKind.Invoked
          TriggerCharacter = None
          IsRetrigger = false
          ActiveSignatureHelp = None
        } }
    sigHelpParams

  let expectSomeOverloads sigHelpLspRes =
    let sigHelp : SignatureHelp =
      sigHelpLspRes
      |> Flip.Expect.wantSome "Expected some signature help"
    sigHelp.Signatures |> Flip.Expect.isNonEmpty "Expected some overloads"

  let checkOverloadsAt pos name = testCase name (fun _ ->
    let server, testFilePath = serverStart.Value
    let p = getSignatureHelpAt pos testFilePath
    let overloads = server.TextDocumentSignatureHelp(p, CancellationToken.None).GetAwaiter().GetResult()
    expectSomeOverloads overloads
  )

  testSequenced <| testList "SignatureHelp" [
    checkOverloadsAt (0, 36) "Can get overloads of MemoryStream with attached parens"
    for c in 38..40 do
      checkOverloadsAt (1, c) $"Can get overloads at whitespace position {c-38} of unattached parens"
    for c in 39..41 do
      checkOverloadsAt (2, c) $"Can get overloads at whitespace position {c-39} of attached parens"
  ]
