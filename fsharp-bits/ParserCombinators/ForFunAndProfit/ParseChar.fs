module FSharpBits.ParserCombinators.ForFunAndProfit.ParseChar

open System
open FSharpBits.ParserCombinators.ForFunAndProfit
open Parser
open ParseResult
open Xunit
open Swensen.Unquote

let parseChar (charToMatch: char) : char Parser =
    let inner (input: string)  =
        if String.IsNullOrEmpty(input)
        then Failure "Expecting 'A'. No input"
        else
            let first = input[0]
            if first = charToMatch
            then
                let remaining = input[1..]
                Success (first, remaining)
            else Failure $"Expecting '{charToMatch}'. Got '{first}'"

    Parser inner

let parseA = parseChar 'A'

[<Fact>]
let ``parses "A"`` () =
    test <@ run parseA "ABC" = Success ('A', "BC") @>

[<Fact>]
let ``empty string -> error`` () =
    test <@ run parseA "" = Failure "Expecting 'A'. No input" @>

[<Fact>]
let ``not "A" -> error`` () =
    test <@ run parseA "ZBC" = Failure "Expecting 'A'. Got 'Z'" @>
