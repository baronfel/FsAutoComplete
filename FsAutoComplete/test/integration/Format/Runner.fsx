#load "../TestHelpers.fsx"
open TestHelpers
open System.IO
open System
open FsAutoComplete.Types

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
File.Delete "output.json"

let p = new FsAutoCompleteWrapper()

p.project "Format.fsproj"

// this file has no changes when the default fantomas config is applied
p.parse "okbydefault.fs"
p.format "okbydefault.fs" None

// this file has a single offset indentation change when the dfault fantomas config is applied
p.parse "defaultoffset.fs"
p.format "defaultoffset.fs" None

// this file has a single indentation change when an indentation of 5 is specified, proving that we can override flags from the command line
p.parse "offset5.fs"
p.format "offset5.fs" (Some ({FormatConfig.Default with IndentSpaceNum = 5}))

// testing selection formatting, this range should be formatted with 5 spaces
p.formatselection "offset5.fs" (1,1,3,19) (Some ({FormatConfig.Default with IndentSpaceNum = 5}))

p.send "quit\n"
p.finalOutput ()
|> writeNormalizedOutput "output.json"