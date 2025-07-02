module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Any

open FsCheck
open FsCheck.Xunit
open global.Xunit
open Swensen.Unquote

open ParserCombinators

let private unexpectedEndOfFile =
    { Expected = "a char"
      Encountered = "the end of input" }

let any: Char Parser =
    Parser(fun input ->
        match Seq.toList input with
        | [] -> ("", Error unexpectedEndOfFile)
        | head :: tail -> (string tail, Ok(head)))


[<Property>]
let ``parses any char`` (s: NonEmptyString) =
    let _, result = runParser any s.Get

    test <@ result = Ok s.Get[0] @>

[<Fact>]
let ``error in case of empty input`` () =
    let _, result = runParser any ""

    test <@ result = Error { Expected = "a char"; Encountered = "the end of input" }  @>
