module FSharpBits.ParserCombinators.ForFunAndProfit.OrElse


open FSharpBits.ParserCombinators.ForFunAndProfit
open FSharpBits.ParserCombinators.ForFunAndProfit.ParseChar
open Parser
open ParseResult
open Xunit
open Swensen.Unquote

let orElse (parser1: Parser<'a>) (parser2: Parser<'a>) : Parser<'a>  =
    let inner input =
        let parseResult = run parser1 input
        match parseResult with
        | Success (a, rest) as parser1Success -> parser1Success
        | Failure e ->
            let parseResult = run parser2 input
            match parseResult with
            | Failure e -> Failure e
            | Success (a, rest) as parser2Success -> parser2Success

    Parser inner

let (<|>) = orElse

let parseA = parseChar 'A'
let parseB = parseChar 'B'

let parseAB = parseA <|> parseB

[<Fact>]
let ``uses parseA with "AZZ"`` () =
    test <@ run parseAB "AZZ" = Success ('A', "ZZ") @>

[<Fact>]
let ``uses parseB with "BZZ"`` () =
    test <@ run parseAB "BZZ" = Success ('B', "ZZ") @>

[<Fact>]
let ``neither "A" nor "B" -> error`` () =
    test <@ run parseAB "ZZZ" = Failure "Expecting 'B'. Got 'Z'" @>
