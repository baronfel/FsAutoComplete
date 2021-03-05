#r "nuget: Fake.Core.Target, 5.20.3"
#r "nuget: Fake.Core.Process, 5.20.3"
#r "nuget: Fake.DotNet.Cli, 5.20.3"
#r "nuget: Fake.Core.ReleaseNotes, 5.20.3"
#r "nuget: Fake.DotNet.AssemblyInfoFile, 5.20.3"
#r "nuget: Fake.DotNet.Paket, 5.20.3"
#r "nuget: Fake.Tools.Git, 5.20.3"
#r "nuget: Fake.Core.Environment, 5.20.3"
#r "nuget: Fake.Core.UserInput, 5.20.3"
#r "nuget: Fake.IO.FileSystem, 5.20.3"
#r "nuget: Fake.DotNet.MsBuild, 5.20.3"
#r "nuget: Fake.Api.GitHub, 5.20.3"

// open Fake
open Fake.Core
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet
open Fake.Api
open Fake.Tools
open System.IO

let project = "FsAutoComplete"

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

let configuration = DotNet.BuildConfiguration.Release

let buildDir = "src" </> project </> "bin" </> "Debug"
let buildReleaseDir = "src" </> project </>  "bin" </> "Release"
let pkgsDir = "bin" </> "pkgs"
let releaseArchiveNetCore = pkgsDir </> "fsautocomplete.netcore.zip"

let gitOwner = "fsharp"
let gitName = project
let gitHome = "https://github.com/" + gitOwner


let lspTest = fun _ ->
  DotNet.exec
      (fun p ->
          { p with
              Timeout = Some (System.TimeSpan.FromMinutes 10.) })
      "run"
      """-c Release --no-build -p "./test/FsAutoComplete.Tests.Lsp/FsAutoComplete.Tests.Lsp.fsproj" -- --fail-on-focused-tests --debug --summary"""
  |> fun r -> if not r.OK then failwithf "Errors while running LSP tests:\n%s" (r.Errors |> String.concat "\n\t")

let rec allFiles basePath = seq {
  for fileOrDirectory in System.IO.Directory.EnumerateFileSystemEntries basePath do
    if Directory.Exists fileOrDirectory then
      yield! allFiles fileOrDirectory
    else if File.Exists fileOrDirectory then yield fileOrDirectory
}

let zipDir baseDir (outputFilePath: FileInfo) =
  Trace.trace $"Zipping contents of {baseDir} to {outputFilePath.FullName}"
  use zipFile = outputFilePath.OpenWrite()
  use archive = new Compression.ZipArchive(zipFile, Compression.ZipArchiveMode.Create)
  for file in allFiles baseDir do
    let entryPath =  file.Replace(baseDir + Path.directorySeparator, "") // remove the start of the file path
    // create entry
    Trace.trace $"Adding entry for {file} at {entryPath}"
    let entry = archive.CreateEntry(entryPath)
    // copy data
    Trace.trace $"Copying {file} contents into {entry.FullName}"
    use entryStream = entry.Open()
    use fileStream = FileInfo(file).OpenRead()
    fileStream.CopyTo entryStream
    // patch up attributes
    entry.ExternalAttributes <- entry.ExternalAttributes ||| (System.Convert.ToInt32("664", 8) <<< 16);

let replaceFsLibLogNamespaces = fun _ ->
  let replacements =
    [ "FsLibLog\\n", "FsAutoComplete.Logging\n"
      "FsLibLog\\.", "FsAutoComplete.Logging" ]
  replacements
  |> List.iter (fun (``match``, replace) ->
    (!! "paket-files/TheAngryByrd/FsLibLog/**/FsLibLog*.fs")
    |> Shell.regexReplaceInFilesWithEncoding ``match`` replace System.Text.Encoding.UTF8
  )

let localRelease = fun _ ->
    Directory.ensure "bin/release"
    Shell.cleanDirs [ "bin/release"; "bin/release_netcore" ]

    Shell.cleanDirs [ "bin/release_netcore" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_netcore")
           Framework = Some "net5.0"
           Configuration = configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion ] } }) "src/FsAutoComplete"


    Shell.cleanDirs [ "bin/release_as_tool" ]
    DotNet.publish (fun p ->
       { p with
           OutputPath = Some (__SOURCE_DIRECTORY__ </> "bin/release_as_tool")
           Configuration = configuration
           MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion; "PackAsTool", "true" ] } }) "src/FsAutoComplete"


let releaseArchive =
  replaceFsLibLogNamespaces >> localRelease >>
  fun _ ->
    Shell.cleanDirs [ "bin/pkgs" ]
    Directory.ensure "bin/pkgs"

    zipDir "bin/release_netcore" (FileInfo releaseArchiveNetCore)

    !! (sprintf "bin/release_as_tool/fsautocomplete.%s.nupkg" release.AssemblyVersion)
    |> Shell.copy "bin/pkgs"


let clean = fun _ ->
  Shell.cleanDirs [ buildDir; buildReleaseDir; pkgsDir ]

let restore = fun _ ->
    DotNet.restore id ""

let build = fun _ ->
  DotNet.build (fun p ->
     { p with
         Configuration = configuration
         MSBuildParams = { MSBuild.CliArguments.Create () with Properties =  [ "SourceLinkCreate","true"; "Version", release.AssemblyVersion ] } }) "FsAutoComplete.sln"



let releaseGithub = fun _ ->
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.filter (fun (s: string) -> s.EndsWith("(push)"))
        |> Seq.tryFind (fun (s: string) -> s.Contains(gitOwner + "/" + gitName))
        |> function None -> gitHome + "/" + gitName | Some (s: string) -> s.Split().[0]

    Git.Staging.stageAll ""
    Git.Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")


    Git.Branches.tag "" release.NugetVersion
    Git.Branches.pushTag "" remote release.NugetVersion

    let client =
        let token =
            match Environment.environVarOrNone "github-token" with
            | Some s when not (String.isNullOrWhiteSpace s) -> s
            | _ -> UserInput.getUserInput "Token: "

        GitHub.createClientWithToken token

    let files = !! (pkgsDir </> "*.*")

    let notes =
      release.Notes
      |> List.map (fun s -> "* " + s)

    // release on github
    let cl =
        client
        |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) notes
    (cl,files)
    ||> Seq.fold (fun acc e -> acc |> GitHub.uploadFile e)
    |> GitHub.publishDraft//releaseDraft
    |> Async.RunSynchronously


let build_target: unit -> unit =
  restore >> replaceFsLibLogNamespaces >> build

let test_target: unit -> unit =
  build_target >> lspTest

let release_target: unit -> unit =
  replaceFsLibLogNamespaces >> localRelease >> releaseArchive >> releaseGithub

let all_target: unit -> unit =
  test_target >> releaseArchive

let targets =
  Map.ofList [
    "build", build_target
    "test", test_target
    "release", release_target
    "all", all_target
    "releasearchive", releaseArchive
  ]

let targetList () =
  targets |> Seq.map (fun (KeyValue(target, _)) -> $"*\t{target}") |> String.concat System.Environment.NewLine

match System.Environment.GetCommandLineArgs() with
| [| fsi_binary; "build.fsx" |] ->
  build_target ()
| [| fsi_binary; "build.fsx"; target |] ->
  match targets |> Map.tryFind (target.ToLowerInvariant()) with
  | Some func -> func ()
  | None ->
    printfn $"Unknown target {target}. Known targets are:\n{targetList()}"
    exit 1
| args ->
  printfn $"unknown arguments {args |> List.ofArray}"
  exit 2
