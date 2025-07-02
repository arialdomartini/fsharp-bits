module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Try

open ParserCombinators
open Xunit
open Swensen.Unquote

let ``try`` (parser: Parser<'a>) : Parser<'a> =
    Parser (fun input ->
        let _, result as r = runParser parser input
        match result with
        | Ok _ -> r
        | Error _ -> (input, Error { Expected = "to apply a parser"; Encountered = "something different. Backtracking." }))


[<Fact>]
let ``return result if successful`` () =
    let successful = Parser (fun _ -> ("", Ok "success"))

    let trying = ``try`` successful

    let _, result = runParser trying "input"

    test <@ result = Ok "success" @>

let someError = Error { Expected = ""; Encountered = "" }

[<Fact>]
let ``does not consume input in case it fails`` () =
    let failing = Parser (fun _ ->  ("", someError))

    let trying = ``try`` failing

    let originalInput = "input"
    let rest, result = runParser trying originalInput

    let isError = result.IsError
    test <@ isError @>
    test <@ rest = originalInput @>
