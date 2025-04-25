module FSharpBits.ParserCombinators.ForFunAndProfit.Many

open FSharpBits.ParserCombinators.ForFunAndProfit.Functor
open Parser
open ParseResult
open Xunit
open Swensen.Unquote

type Space = Space

let many (parser: Parser<'a>) : Parser<'a list> = Parser (fun input ->
    let rec zeroOrMore parser input =
        let result = run parser input
        match result with
        | Failure _ -> ([], input)
        | Success (r, rest) ->
            let others, remainingInput = (zeroOrMore parser) rest
            (r :: others, remainingInput)

    let result, rest = zeroOrMore parser input
    Success (result, rest))


[<Fact>]
let ``parses 0 or many spaces`` () =
    let spaceParser = Lift.parseChar ' '

    let spacesParser = many spaceParser

    test <@ run spacesParser "     foo" = Success ("     " |> Seq.toList, "foo") @>

let toSpace _ = Space

[<Fact>]
let ``parses 0 or many spaces as a symbol`` () =
    let spaceParser = toSpace <!> (Lift.parseChar ' ')

    let spacesParser = toSpace <!> (many spaceParser)

    test <@ run spacesParser "     foo" = Success (Space, "foo") @>
