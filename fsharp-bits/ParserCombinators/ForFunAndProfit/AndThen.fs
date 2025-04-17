module FSharpBits.ParserCombinators.ForFunAndProfit.AndThen

open FSharpBits.ParserCombinators.ForFunAndProfit
open FSharpBits.ParserCombinators.ForFunAndProfit.ParseChar
open Parser
open ParseResult
open Xunit
open Swensen.Unquote

let andThen (parser1: Parser<'a>) (parser2: Parser<'b>) : Parser<('a * 'b)>  =
    let inner input =
        let parseResult = run parser1 input
        match parseResult with
        | Failure e -> Failure e
        | Success (a, rest) ->
            let parseResult = run parser2 rest
            match parseResult with
            | Failure e -> Failure e
            | Success (b, rest) -> Success ((a, b), rest)

    Parser inner

let (.>>.) = andThen

let parseA = parseChar 'A'
let parseB = parseChar 'B'

let parseAB = parseA .>>. parseB

[<Fact>]
let ``parses "AB"`` () =
    test <@ run parseAB "ABC" = Success (('A', 'B'), "C") @>

[<Fact>]
let ``empty string -> error`` () =
    test <@ run parseAB "" = Failure "Expecting 'A'. No input" @>

[<Fact>]
let ``not "A" -> error`` () =
    test <@ run parseAB "ZBC" = Failure "Expecting 'A'. Got 'Z'" @>
