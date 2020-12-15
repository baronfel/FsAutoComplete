module FsAutoComplete.Tests.Formatting

open Expecto
open FsAutoComplete

let signatureFormattingTests = ftestList "Info Panel formatting" [
  test "can format complex type name into links" {
    let typesig = "Async<Result<int list, exn>>"
    let expected = "Async<Result<List<int>, exn>>"
    let actual = DocumentationFormatter.scrubOcamlTypeSigs typesig
    Expect.equal actual expected "expected to format ocaml-esque function sig with .net-style generics"
  }
]
