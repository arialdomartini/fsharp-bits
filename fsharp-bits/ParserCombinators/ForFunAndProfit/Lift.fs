module FSharpBits.ParserCombinators.ForFunAndProfit.Lift

open FSharpBits.ParserCombinators.ForFunAndProfit.Functor
open FSharpBits.ParserCombinators.ForFunAndProfit.Applicative
open FSharpBits.ParserCombinators.ForFunAndProfit.Monad
open Parser
open ParseResult
open Xunit
open Swensen.Unquote


let lift2 (f: 'f -> 'a -> 'b) : 'f Parser -> 'a Parser -> 'b Parser = fun a b -> f <!> a <*> b

let lift3 (f: 'f -> 'a -> 'b -> 'c) : 'f Parser -> 'a Parser -> 'b Parser -> 'c Parser = fun a b c -> f <!> a <*> b <*> c

let parse3Digits' = lift3 combines3Digits parseDigit parseDigit parseDigit

[<Fact>]
let ``with lift3`` () =
    test <@ run parse3Digits' "123ZZZ" = Success(123, "ZZZ") @>

type Parsed = One | Two | Three | Space


let rec sequence (parsers: ('a Parser) list) : ('a list) Parser =
    let cons x xs = x :: xs
    let consP: Parser<'a> -> Parser<'a list> -> Parser<'a list> = lift2 cons

    match parsers with
    | [] -> returnP []
    | p::ps -> consP p (sequence ps)


[<Fact>]
let ``list of parsers`` () =

    let parser (expecting: string) emitting =
        Parser (fun input ->
            if input[0..expecting.Length-1] = expecting
            then Success (emitting, input[expecting.Length..])
            else Failure $"Expecting '{expecting}'")

    let parseSpace = parser " " Space
    let parseOne = parser "one" One
    let parseTwo = parser "two" Two
    let parseThree = parser "three" Three


    let parseList = sequence [parseOne; parseSpace; parseTwo; parseSpace; parseThree]

    test <@ run parseList "one two three" = Success ([ One; Space; Two; Space; Three ], "") @>
