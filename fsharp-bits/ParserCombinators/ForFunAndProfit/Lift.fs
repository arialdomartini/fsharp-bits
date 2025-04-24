module FSharpBits.ParserCombinators.ForFunAndProfit.Lift

open FSharpBits.ParserCombinators.ForFunAndProfit.Functor
open FSharpBits.ParserCombinators.ForFunAndProfit.Applicative
open Parser
open ParseResult
open Xunit
open Swensen.Unquote


let lift2 (f: 'f -> 'a -> 'b) : 'f Parser -> 'a Parser -> 'b Parser =
    fun a b -> f <!> a <*> b

let lift3 (f: 'f -> 'a -> 'b -> 'c) : 'f Parser -> 'a Parser -> 'b Parser -> 'c Parser=
    fun a b c -> f <!> a <*> b <*> c

let parse3Digits' =
    lift3 combines3Digits parseDigit parseDigit parseDigit

[<Fact>]
let ``with lift3`` () =
    test <@ run parse3Digits' "123ZZZ" = Success (123, "ZZZ")@>
