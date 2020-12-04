namespace FsAutoComplete

open FSharp.Compiler.AbstractIL.Internal.Library
open System
open System.IO
open FsAutoComplete.Logging
open System.Collections.Generic

type FileLines (lines: string[]) =
  let mutable lineStarts = lazy (
    seq {
        let mutable startPos = 0;
        for line in lines do
          yield startPos
          startPos <- startPos + line.Length
      }
      |> Seq.toArray
  )

  let totalLength = lazy (
    let mutable len = 0
    for line in lines do
      len <- len + line.Length

    // handle newlines, 1 newline between each line except the last
    if lines.Length = 0 then len else len + lines.Length - 1
  )

  let stringHash = lazy (hash lines)
  let backingString = lazy (String.concat "\n" lines)

  let lineNumberForIndex idx =
    match lineStarts.Value |> Array.tryFindIndexBack (fun lineStart -> lineStart <= idx) with
    | Some closestLineNumber ->
      let closestLineStart = lineStarts.Value.[closestLineNumber]
      Some(closestLineNumber, closestLineStart)
    | None ->
      None

  member _.LineStarts = lineStarts.Value
  member _.StringHash = stringHash.Value

  member _.BackingString = backingString.Value

  member _.CopyTo(sourceIndex, destination, destinationIndex, count) =
    backingString.Value.CopyTo(sourceIndex, destination, destinationIndex, count)

  member _.SubTextString(start, length) =
    backingString.Value.Substring(start, length)

  member _.CharAsPosition (pos: int) =
    match lineNumberForIndex pos with
    | Some (lineNo, lineStartPos) ->
      let lineOffset = pos - lineStartPos
      Some lines.[lineNo].[lineOffset]
    | None -> None
type FileText(lines: string []) =
  let fileLines = FileLines lines


  // member _.Lines =

  member _.StringHash = fileLines.StringHash

  interface FSharp.Compiler.Text.ISourceText with
    member this.ContentEquals(sourceText: FSharp.Compiler.Text.ISourceText): bool =
        match sourceText with
        | :? FileText as ft when ft = this || this.StringHash = ft.StringHash -> true
        | _ -> false

    member this.CopyTo(sourceIndex: int, destination: char [], destinationIndex: int, count: int): unit =
        fileLines.CopyTo(sourceIndex, destination, destinationIndex, count)

    member this.GetLastCharacterPosition(): int * int =
        if lines.Length = 0 then 0, 0
        else lines.Length, lines.[lines.Length - 1].Length

    member this.GetLineCount(): int =
        lines.Length

    member this.GetLineString(lineIndex: int): string =
        lines.[lineIndex]

    member this.GetSubTextString(start: int, length: int): string =
        fileLines.SubTextString(start, length)

    member this.Item
        with get (pos: int): char =
            match lineNumberForIndex pos with
            | Some (lineNo, lineStartPos) ->
              let lineOffset = pos - lineStartPos
              lines.[lineNo].[lineOffset]
            | None -> invalidArg "pos" (sprintf "couldn't get char at position %d in text" pos)

    member this.Length: int =
        lines.Length

    member this.SubTextEquals(target: string, startIndex: int): bool =
        if startIndex < 0 || startIndex >= totalLength.Value then
            invalidArg "startIndex" "Out of range."

        if String.IsNullOrEmpty(target) then
            invalidArg "target" "Is null or empty."

        let lastIndex = startIndex + target.Length
        if lastIndex <= startIndex || lastIndex >= totalLength.Value then
            invalidArg "target" "Too big."

        backingString.Value.IndexOf(target, startIndex, target.Length) <> -1


  interface IReadOnlyList<string> with
     member this.Count: int = lines.Length
     member this.GetEnumerator(): Collections.IEnumerator =
         lines.GetEnumerator()
     member this.GetEnumerator(): IEnumerator<string> =
          (seq {
            for line in lines do yield line
          }).GetEnumerator()
     member this.Item
         with get (index: int): string =
             failwith "Not Implemented"


type VolatileFile =
  { Touched: DateTime
    Lines: FileText
    Version: int option}

open System.IO

type FileSystem (actualFs: IFileSystem, tryFindFile: SourceFilePath -> VolatileFile option) =
    let getContent (filename: string) =
         filename
         |> tryFindFile
         |> Option.map (fun file ->
              System.Text.Encoding.UTF8.GetBytes (String.Join ("\n", file.Lines)))

    let fsLogger = LogProvider.getLoggerByName "FileSystem"
    /// translation of the BCL's Windows logic for Path.IsPathRooted.
    ///
    /// either the first char is '/', or the first char is a drive identifier followed by ':'
    let isWindowsStyleRootedPath (p: string) =
        let isAlpha (c: char) =
            (c >= 'A' && c <= 'Z')
            || (c >= 'a' && c <= 'z')
        (p.Length >= 1 && p.[0] = '/')
        || (p.Length >= 2 && isAlpha p.[0] && p.[1] = ':')

    /// translation of the BCL's Unix logic for Path.IsRooted.
    ///
    /// if the first character is '/' then the path is rooted
    let isUnixStyleRootedPath (p: string) =
        p.Length > 0 && p.[0] = '/'

    interface IFileSystem with
        (* for these two members we have to be incredibly careful to root/extend paths in an OS-agnostic way,
           as they handle paths for windows and unix file systems regardless of your host OS.
           Therefore, you cannot use the BCL's Path.IsPathRooted/Path.GetFullPath members *)

        member _.IsPathRootedShim (p: string) =
          let r =
            isWindowsStyleRootedPath p
            || isUnixStyleRootedPath p
          fsLogger.debug (Log.setMessage "Is {path} rooted? {result}" >> Log.addContext "path" p >> Log.addContext "result" r)
          r

        member _.GetFullPathShim (f: string) =
          let expanded =
            Path.FilePathToUri f
            |> Path.FileUriToLocalPath
          fsLogger.debug (Log.setMessage "{path} expanded to {expanded}" >> Log.addContext "path" f >> Log.addContext "expanded" expanded)
          expanded

        (* These next members all make use of the VolatileFile concept, and so need to check that before delegating to the original FS implementation *)

        (* Note that in addition to this behavior, we _also_ do not normalize the file paths anymore for any other members of this interfact,
           because these members are always used by the compiler with paths returned from `GetFullPathShim`, which has done the normalization *)

        member _.ReadAllBytesShim (f) =
          getContent f
          |> Option.defaultWith (fun _ -> actualFs.ReadAllBytesShim f)

        member _.FileStreamReadShim (f) =
          getContent f
          |> Option.map (fun bytes -> new MemoryStream(bytes) :> Stream)
          |> Option.defaultWith (fun _ -> actualFs.FileStreamReadShim f)

        member _.GetLastWriteTimeShim (f) =
          tryFindFile f
          |> Option.map (fun f -> f.Touched)
          |> Option.defaultWith (fun _ -> actualFs.GetLastWriteTimeShim f)

        member _.FileStreamCreateShim (f) = actualFs.FileStreamCreateShim f
        member _.FileStreamWriteExistingShim (f) = actualFs.FileStreamWriteExistingShim f
        member _.IsInvalidPathShim (f) = actualFs.IsInvalidPathShim f
        member _.GetTempPathShim () = actualFs.GetTempPathShim()
        member _.SafeExists (f) = actualFs.SafeExists f
        member _.FileDelete (f) = actualFs.FileDelete f
        member _.AssemblyLoadFrom (f) = actualFs.AssemblyLoadFrom f
        member _.AssemblyLoad (f) = actualFs.AssemblyLoad f
        member _.IsStableFileHeuristic (f) = actualFs.IsStableFileHeuristic f
