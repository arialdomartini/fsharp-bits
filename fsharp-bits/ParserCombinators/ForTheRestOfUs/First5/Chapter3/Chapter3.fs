module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Chapter3.Chapter3

open System
open FsCheck.Xunit
open global.Xunit
open Swensen.Unquote

type Person =
    { Id: Guid
      Name: string
      Birthday: DateOnly }

type RockTrio =
    { Name: string
      BassPlayer: Person
      GuitarPlayer: Person
      Drummer: Person }

type SoloArtist = { NickName: string; Artist: Person }

type RockBand =
    | RockTrio of RockTrio
    | SoloArtist of SoloArtist

exception ParseException

let isRockTrio =
    function
    | RockTrio _ -> true
    | SoloArtist _ -> false

[<Property>]
let ``parse a RockTrio`` (aRockTrio: RockTrio) (input: string) =
    let parseRockTrio input = RockBand.RockTrio aRockTrio
    let parseSoloArtist input = raise ParseException

    let parseBand input : RockBand =
        try
            parseRockTrio input
        with ParseException ->
            parseSoloArtist input

    Assert.Equal(parseBand input, RockTrio aRockTrio)

let parseBand parseRockTrio parseSoloArtist input : RockBand =
    try
        parseRockTrio input
    with ParseException ->
        parseSoloArtist input

[<Property>]
let ``it parses a SoloArtist if parsing of RockTrio fails`` (artist: SoloArtist) (input: string) =
    let justFail input = raise ParseException
    let successfullyParseSoloArtist input = SoloArtist artist

    let parsed = parseBand justFail successfullyParseSoloArtist input

    test <@ parsed = SoloArtist artist @>
