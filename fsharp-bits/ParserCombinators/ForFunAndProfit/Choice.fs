module FSharpBits.ParserCombinators.ForFunAndProfit.Choice

open FSharpBits.ParserCombinators.ForFunAndProfit
open FSharpBits.ParserCombinators.ForFunAndProfit.ParseResult
open ParseChar
open Parser
open OrElse
open Xunit
open Swensen.Unquote

let choice (parsers: 'a Parser list) : 'a Parser =
    List.reduce (<|>) parsers

let parseLowerCase =
    ['a'..'z']
    |> List.map parseChar
    |> choice

[<Fact>]
let success () =
    test <@ run parseLowerCase "aZZ" = Success ('a', "ZZ") @>

[<Fact>]
let failure () =
    test <@ run parseLowerCase "XZZ" = Failure ("Expecting 'z'. Got 'X'") @>
