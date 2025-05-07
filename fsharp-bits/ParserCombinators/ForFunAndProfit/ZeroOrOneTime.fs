module FSharpBits.ParserCombinators.ForFunAndProfit.ZeroOrOneTime

open Lift
open Many1
open Monad
open Xunit
open Swensen.Unquote
open Parser
open ParseResult
open OrElse
open ParseChar
open Pipe

let opt (parser: 'a Parser) : 'a option Parser=
    let some = parser |>> Some
    let none = returnP None
    some <|> none

let parseIntegers : int Parser =

    let buildInt (minus: char option) (i: uint) =
        match minus with
        | Some _ -> -1 * int i
        | None -> int i

    // buildInt <!> (opt (parseChar '-')) <*> parseNat
    (lift2 buildInt) (opt (parseChar '-')) parseNat

[<Fact>]
let ``optional minus sign`` () =

    test <@ run parseIntegers "123C" =  Success (123, "C")  @>
    test <@ run parseIntegers "-123C" = Success (-123, "C") @>
