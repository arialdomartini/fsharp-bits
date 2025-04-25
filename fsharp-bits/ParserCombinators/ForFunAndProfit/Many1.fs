module FSharpBits.ParserCombinators.ForFunAndProfit.Many1

open AndThen
open FSharpBits.ParserCombinators.ForFunAndProfit.Choice
open FSharpBits.ParserCombinators.ForFunAndProfit.Lift
open Many
open Functor
open Applicative
open Parser
open ParseResult
open Xunit
open Swensen.Unquote


let many1 (parser: Parser<'a>) : Parser<'a list> =
    (fun f s -> f::s) <!> parser <*> (many parser)

let many1' (parser: Parser<'a>) : Parser<'a list> =
    (fun (f, s) -> f::s) <!> (parser .>>. (many parser))

[<Fact>]
let ``parses at least 1 space, with Applicative`` () =
    let spaceParser = Lift.parseChar ' '

    let spacesParser = many1 spaceParser

    test <@ run spacesParser "     foo" = Success ("     " |> Seq.toList, "foo") @>

[<Fact>]
let ``fails if there is not at least 1 space, with Applicative`` () =
    let spaceParser = Lift.parseChar ' '

    let spacesParser = many1 spaceParser

    test <@ run spacesParser "foo" = Failure "Expecting ' '" @>

[<Fact>]
let ``parses at least 1 space`` () =
    let spaceParser = Lift.parseChar ' '

    let spacesParser = many1' spaceParser

    test <@ run spacesParser "     foo" = Success ("     " |> Seq.toList, "foo") @>

[<Fact>]
let ``fails if there is not at least 1 space`` () =
    let spaceParser = Lift.parseChar ' '

    let spacesParser = many1' spaceParser

    test <@ run spacesParser "foo" = Failure "Expecting ' '" @>

let parseInt : int Parser=

    let buildInteger (cs: char list) : int =
        System.String.Join("", cs)
        |> System.Int32.Parse

    let parseDigit = anyOf ['0'..'9']

    lift buildInteger (many1 parseDigit)



[<Fact>]
let ``parse an int`` () =
    let result = run parseInt "1234 something else"

    test <@ result = Success (1234, " something else") @>
