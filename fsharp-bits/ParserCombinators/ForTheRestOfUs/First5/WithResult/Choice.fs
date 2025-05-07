module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Chapter3.WithResult.Choice

open global.Xunit
open Swensen.Unquote
open FSharpBits.ParserCombinators.ForTheRestOfUs.First5.WithResult.Parser

let rec choice<'a> (parsers: 'a Parser list) : 'a Parser =
    fun input ->
        match parsers with
        | [] -> Error "All parsers failed"
        | parser::others ->
            match parser input with
            | Ok success -> Ok success
            | Error _ -> choice others input

let failingParser i : 'a Parser =
    fun _ ->
        Result.Error $"parser {i} failed"

let failingParsers<'a>: 'a Parser list =
    [1..10]
    |> Seq.map failingParser
    |> Seq.toList


[<Fact>]
let ``it fails if all the parsers fail`` () =

    let parser = choice failingParsers

    test <@ parser "whatever input" = Error "All parsers failed" @>

[<Fact>]
let ``uses the first successful parser`` () =

    let first _ = Ok ("", "first succeeded!")
    let second _  = Ok ("", "first succeeded!")

    let parser: string Parser =
                choice
                     [failingParser 1
                      failingParser 2
                      first
                      failingParser 3
                      second ]

    test <@ parser "whatever input" = Ok ("", "first succeeded!") @>
