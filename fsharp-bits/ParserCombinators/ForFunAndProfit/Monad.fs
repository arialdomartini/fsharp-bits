module FSharpBits.ParserCombinators.ForFunAndProfit.Monad

open Xunit
open Swensen.Unquote
open Parser
open ParseResult

let returnP v =
    Parser (fun input -> Success (v, input))

[<Fact>]
let ``Monad's return`` () =

    let parseX = returnP "X"

    test <@ run parseX "" =  Success ("X", "")@>
    test <@ run parseX "whatever" =  Success ("X", "whatever")@>
