module FSharpBits.ParserCombinators.ForTheRestOfUs.Chapter3.ParseMultiple

open global.Xunit
open Swensen.Unquote

exception ParseException of string

let rec parseSequence<'a> (parsers: (string -> (string * 'a)) list) (input: string) : (string * ('a list)) =
    let rec parseRec remainingInput parsers acc =
        match parsers with
        | [] -> (remainingInput, List.rev acc)
        | currentParser::remainingParsers ->
            let (newRemaining, parsedValue) = currentParser remainingInput
            parseRec newRemaining remainingParsers (parsedValue::acc)

    parseRec input parsers []


type Something = Something of int

let mockParser (i: int) (input: string) = (input[1..], Something i)

[<Fact>]
let ``applies all the parsers consuming 1 character for parser`` () =

    let fiveParsers =
        [ 1..5 ]
        |> Seq.map mockParser
        |> Seq.toList

    let parser = fiveParsers |> parseSequence

    let parsedValues = [
        Something 1
        Something 2
        Something 3
        Something 4
        Something 5 ]

    test <@ parser "12345abc" = ("abc", parsedValues) @>
