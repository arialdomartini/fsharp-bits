module FSharpBits.ParserCombinators.Alternative

open FSharpBits.ParserCombinators.ParserCombinators
open FSharpBits.ParserCombinators.Try
open Xunit
open Swensen.Unquote

let (<|>) (tryFirst: Parser<'a>) (fallback: Parser<'a>): Parser<'a> =
    Parser (fun input ->
        let first = runParser tryFirst input
        match first with
        | rest, Error _ when rest = input -> runParser fallback rest
        | _ -> first)


[<Fact>]
let ``applies the second parser, if the first is unsuccessful and did not consume the input`` () =
    let success1 = Parser (fun _ -> ("", someError))
    let success2 = Parser (fun _ -> ("", Ok "success2"))

    let trying = (``try`` success1) <|> success2

    let _, result = runParser trying "input"

    test <@ result = Ok "success2" @>

[<Fact>]
let ``propagate the error if the first is unsuccessful and consumed the input`` () =
    let success1 = Parser (fun input ->  ("", someError))
    let success2 = Parser (fun _ -> ("", Ok "success2"))

    let trying = success1 <|> success2

    let _, result = runParser trying "input"

    let isError = result.IsError
    test <@ result = someError @>
