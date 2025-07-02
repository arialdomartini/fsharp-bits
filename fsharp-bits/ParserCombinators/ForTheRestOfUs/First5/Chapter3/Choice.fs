module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Chapter3.Choice

open global.Xunit
open Swensen.Unquote
open FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Parser

exception ParseException of string

let rec choice<'a> (parsers: 'a Parser list) : 'a Parser =
    fun input ->
    match parsers with
    | [] -> raise (ParseException "All parsers failed")
    | parser::others ->
        try
            parser input
        with
        | ParseException _ -> choice others input

let failingParser<'a> (i: int) (input: string) : 'a =
    raise (ParseException $"parser {i} failed")

let failingParsers<'a> : (string -> 'a) list =
    [1..10]
    |> Seq.map failingParser
    |> Seq.toList

[<Fact>]
let ``it fails if all the parsers fail`` () =

    let parser = choice failingParsers

    raisesWith<ParseException>
        <@ parser "whatever input" @>
        (fun e -> <@ e.Data0 = "All parsers failed" @>)

[<Fact>]
let ``uses the first successful parser`` () =

    let first _ = ("", "first succeeded!")
    let second _  = ("", "first succeeded!")

    let parser: string Parser =
                choice
                     [failingParser 1
                      failingParser 2
                      first
                      failingParser 3
                      second ]

    test <@ parser "whatever input" = ("", "first succeeded!") @>
