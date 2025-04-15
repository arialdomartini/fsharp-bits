module FSharpBits.ParserCombinators.EOF

open FsCheck
open FsCheck.Xunit
open global.Xunit
open Swensen.Unquote

open FSharpBits.ParserCombinators.ParserCombinators

let private extraChars =
    { Expected = "the end of input"
      Encountered = "other chars" }

let eof: Unit Parser =
    Parser(fun input ->
        match Seq.toList input with
        | [] -> ("", Ok())
        | xs -> (string xs, Error extraChars))

[<Property>]
let ``error with non empty strings`` (s: NonEmptyString) =
    let _, result = runParser eof s.Get

    test <@ result = Error { Expected = "the end of input"; Encountered = "other chars" } @>

[<Fact>]
let ``parses the end of file`` () =
    let _, result = runParser eof ""

    test <@ result = (Ok ()) @>
