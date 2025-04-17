module FSharpBits.ParserCombinators.ForFunAndProfit.ParseChar

open System
open Xunit
open Swensen.Unquote

let ParseChar (charToMatch: char) (input: string) : (string * string) =
    if String.IsNullOrEmpty(input)
    then ("Expecting 'A'. No input", "")
    else
        let first = input[0]
        if first = charToMatch
        then
            let remaining = input[1..]
            ($"Found '{charToMatch}'", remaining)
        else ($"Expecting '{charToMatch}'. Got '{first}'", input)

let parseA = ParseChar 'A'

[<Fact>]
let ``parses "A"`` () =
    test <@ parseA "ABC" = ("Found 'A'", "BC") @>

[<Fact>]
let ``empty string -> error`` () =
    test <@ parseA "" = ("Expecting 'A'. No input", "") @>

[<Fact>]
let ``not "A" -> error`` () =
    test <@ parseA "ZBC" = ("Expecting 'A'. Got 'Z'", "ZBC") @>
