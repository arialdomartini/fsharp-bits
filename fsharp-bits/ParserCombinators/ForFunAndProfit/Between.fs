module FSharpBits.ParserCombinators.ForFunAndProfit.Between

open Xunit
open Swensen.Unquote
open Many1
open ParseChar
open Parser
open ParseResult
open ThrowingResultsAway

let parser: uint Parser =
    let doubleQuotes = parseChar '"'

    doubleQuotes >>. parseNat .>> doubleQuotes

[<Fact>]
let ``parses a number surrounded by double quotes`` () =
    test <@ run parser "\"123\" something else" = Success(123u, " something else") @>
