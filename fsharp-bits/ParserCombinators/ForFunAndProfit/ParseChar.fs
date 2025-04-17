module FSharpBits.ParserCombinators.ForFunAndProfit.ParseChar

open System
open FSharpBits.ParserCombinators.ForFunAndProfit
open ParseResult
open Xunit
open Swensen.Unquote

let parseChar (charToMatch: char) : string -> ParseResult<char * string> =
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

    inner

let parseA = parseChar 'A'

[<Fact>]
let ``parses "A"`` () =
    test <@ parseA "ABC" = Success ('A', "BC") @>

[<Fact>]
let ``empty string -> error`` () =
    test <@ parseA "" = Failure "Expecting 'A'. No input" @>

[<Fact>]
let ``not "A" -> error`` () =
    test <@ parseA "ZBC" = Failure "Expecting 'A'. Got 'Z'" @>
