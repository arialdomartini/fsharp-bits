module FSharpBits.ParserCombinators.ForFunAndProfit.Applicative

open FSharpBits.ParserCombinators.ForFunAndProfit.Functor
open FSharpBits.ParserCombinators.ForFunAndProfit.Monad
open Parser
open ParseResult
open Xunit
open Swensen.Unquote

let applyP (f: ('a -> 'b) Parser) (a: 'a Parser) : 'b Parser =
    Parser (fun input ->
        match run f input with
        | Failure error -> Failure error
        | Success (f', rest) ->
            match run a rest with
            | Failure error -> Failure error
            | Success (a', rest) -> Success (f' a', rest) )

let (<*>) = applyP

let combines3Digits (a: char) (b: char) (c: char) : int = [| a; b; c |] |> System.String |> int

let combines3Digits': Parser<char -> char -> char -> int> = returnP combines3Digits

let parse3Digits: Parser<int> = combines3Digits' <*> parseDigit <*> parseDigit <*> parseDigit


[<Fact>]
let ``applicative functor`` () =

    test <@ run parse3Digits "123ZZZ" = Success (123, "ZZZ")@>
