module FSharpBits.ParserCombinators.ForFunAndProfit.ParseA

open System
open Xunit
open Swensen.Unquote

let parseA (input: string) : (bool * string) =
    if String.IsNullOrEmpty(input)
    then (false, "")
    else
        if input[0] = 'A'
        then
            let remaining = input[1..]
            (true, remaining)
        else (false, input)


[<Fact>]
let ``parses "A"`` () =
    test <@ parseA "ABC" = (true, "BC") @>

[<Fact>]
let ``empty string -> error`` () =
    test <@ parseA "" = (false, "") @>

[<Fact>]
let ``not "A" -> error`` () =
    test <@ parseA "ZBC" = (false, "ZBC") @>
