module FSharpBits.ParserCombinators.ForFunAndProfit.ThrowingResultsAway

open Xunit
open Swensen.Unquote
open Parser
open ParseResult
open AndThen
open Pipe

type Symbol = One | Two

let (.>>) p1 p2 = p1 .>>. p2 |>> snd

let (>>.) p1 p2 = p1 .>>. p2 |>> fst

[<Fact>]
let ``drops left value`` () =
    let parse1 = Parser (fun input -> Success(One, input[1..]))
    let parse2 = Parser (fun input -> Success(Two, input[1..]))
    
    test <@ run (parse1 .>> parse2) "12345" = Success (Two, "345") @>

[<Fact>]
let ``drops right value`` () =
    let parse1 = Parser (fun input -> Success(One, input[1..]))
    let parse2 = Parser (fun input -> Success(Two, input[1..]))

    test <@ run (parse1 >>. parse2) "12345" = Success (One, "345") @>
