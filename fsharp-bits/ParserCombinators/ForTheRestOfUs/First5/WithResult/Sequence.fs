module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Chapter3.WithResultSequence

open FSharpBits.ParserCombinators.ForTheRestOfUs.First5.WithResult.Parser
open global.Xunit
open Swensen.Unquote

exception ParseException of string

let rec sequence<'a> (parsers: 'a Parser list) : 'a list Parser =
    fun input ->
        let rec parseRec remainingInput parsers acc =
            match parsers with
            | [] -> Ok (remainingInput, List.rev acc)
            | currentParser::remainingParsers ->
                let result = currentParser remainingInput
                match result with
                | Error error -> Error error
                | Ok (rest, parsedValue) ->
                    parseRec rest remainingParsers (parsedValue::acc)

        parseRec input parsers []

type Something = Something of int

let mockParser (i: int) : Something Parser =
    fun input -> Ok (input[1..], Something i)

[<Fact>]
let ``applies all the parsers consuming 1 character for parser`` () =

    let fiveParsers=
        [ 1..5 ]
        |> Seq.map mockParser
        |> Seq.toList

    let parser = fiveParsers |> sequence

    let parsedValues = [
        Something 1
        Something 2
        Something 3
        Something 4
        Something 5 ]

    test <@ parser "12345abc" = Ok ("abc", parsedValues) @>
