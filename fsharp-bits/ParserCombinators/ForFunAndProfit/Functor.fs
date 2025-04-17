module FSharpBits.ParserCombinators.ForFunAndProfit.Functor

open FSharpBits.ParserCombinators.ForFunAndProfit
open FSharpBits.ParserCombinators.ForFunAndProfit.ParseResult
open Parser
open ParseChar
open AndThen
open Choice
open Xunit
open Swensen.Unquote

// this won't compile
// let parseString (s: string) =
//     s
//     |> Seq.map parseChar
//     |> Seq.reduce (.>>.)

let parseDigit: char Parser =
    anyOf ['0'..'9']


let parse3Digits: Parser<(char * char) * char> =
    parseDigit .>>.parseDigit .>>.parseDigit

[<Fact>]
let ``parses 3 digits in a sequence`` () =
    test <@ run parse3Digits "123ZZ" = Success ((('1', '2'), '3'), "ZZ") @>


let mapP (f: 'a -> 'b) (parser: 'a Parser) : 'b Parser =
    let inner input =
        let result = run parser input
        match result with
        | Failure error -> Failure error
        | Success (parsed, rest) -> Success (f parsed, rest)

    Parser inner

let (<!>) = mapP

let parse3Digits' =
    let f (((a, b), c)) = System.String [|a;b;c|]

    f <!> parse3Digits

[<Fact>]
let ``streamlines the parsed 3 digits as a single string`` () =
    test <@ run parse3Digits' "123ZZ" = Success ("123", "ZZ") @>
