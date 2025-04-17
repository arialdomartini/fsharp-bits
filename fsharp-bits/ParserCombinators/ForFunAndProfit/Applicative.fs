module FSharpBits.ParserCombinators.ForFunAndProfit.Applicative

open FSharpBits.ParserCombinators.ForFunAndProfit.Functor
open FSharpBits.ParserCombinators.ForFunAndProfit.Monad
open Parser
open ParseResult
open AndThen
open Xunit
open Swensen.Unquote

let applyP' (fp: ('a -> 'b) Parser) (ap: 'a Parser) : 'b Parser =
    Parser (fun input ->
        match run fp input with
        | Failure error -> Failure error
        | Success (f, rest) ->
            match run ap rest with
            | Failure error -> Failure error
            | Success (a, rest) -> Success (f a, rest) )

let applyP fp ap =
    (fun (f, x) -> f x) <!> (fp .>>. ap)

let (<*>) = applyP

let combines3Digits (a: char) (b: char) (c: char) : int = [| a; b; c |] |> System.String |> int

let parse3Digits: Parser<int> =
    combines3Digits <!> parseDigit <*> parseDigit <*> parseDigit


[<Fact>]
let ``applicative functor`` () =
    test <@ run parse3Digits "123ZZZ" = Success (123, "ZZZ")@>


let lift2 (f: 'f -> 'a -> 'b) : 'f Parser -> 'a Parser -> 'b Parser =
    fun a b -> f <!> a <*> b

let lift3 (f: 'f -> 'a -> 'b -> 'c) : 'f Parser -> 'a Parser -> 'b Parser -> 'c Parser=
    fun a b c -> f <!> a <*> b <*> c

let parse3Digits' =
    lift3 combines3Digits parseDigit parseDigit parseDigit

[<Fact>]
let ``with lift3`` () =
    test <@ run parse3Digits' "123ZZZ" = Success (123, "ZZZ")@>
