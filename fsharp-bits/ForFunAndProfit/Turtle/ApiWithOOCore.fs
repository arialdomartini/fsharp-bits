module FSharpBits.ForFunAndProfit.Turtle.ApiWithOOCore

open Common

exception TurtleApiException of string

let private parseDistance (distanceStr: string) =
    try
        float distanceStr
    with
    | ex ->
        raise (TurtleApiException $"Invalid distance: '{distanceStr}'")

let private parseAngle (angleStr: string) =
    try
        (float angleStr) * 1.0<Degrees>
    with
    | ex ->
        raise (TurtleApiException $"Invalid angle: '{angleStr}'")

let private parseColor (colorStr: string) =
    match colorStr with
    | "Black" -> Black
    | "Red" ->  Red
    | "Blue" ->  Blue
    | _ -> raise (TurtleApiException $"Invalid color: {colorStr}")

let private trim (s: string) =
    s.Trim()

let private split (separator: char) (s: string) =
    s.Split(separator)

let private toTokens (commandLine: string) =
    commandLine
    |> split ' '
    |> Array.map trim


open BasicOO

type TurtleApi() =

    let turtle = Turtle(stringLog)

    member this.Exec command =
        match command |> toTokens with
        | [| "Move"; distanceStr |] ->
            let distance = distanceStr |> parseDistance
            turtle.Move distance
        | [| "Turn"; angleStr |] ->
            let angle = angleStr |> parseAngle
            turtle.Turn angle
        | [| "SetColor"; colorStr |] ->
            let color = colorStr |> parseColor
            turtle.SecColor color
        | [| "PenUp" |] ->
            turtle.PenUp()
        | [| "PenDown" |] ->
            turtle.PenDown()
        | _ -> raise (TurtleApiException $"Invalid command: {command}")



open Xunit
open Swensen.Unquote

[<Fact>]
let ``draw a triangle`` () =

    let drawPolygon numberOfSides =
        let angle = 180.0 - (360.0 / float numberOfSides)
        let api = TurtleApi()

        let drawOneSide () =
            api.Exec "Move 100.0"
            api.Exec $"Turn %f{angle}"

        for _ in [ 1..numberOfSides ] do
            drawOneSide ()


    drawPolygon 4

    test
        <@
            logS = """
Move: 100.0
...Draw Black line from (0.0, 0.0) -> (100.0, 0.0)
Turn: 90.0 from 0.0 -> 90.0
Move: 100.0
...Draw Black line from (100.0, 0.0) -> (100.0, 100.0)
Turn: 90.0 from 90.0 -> 180.0
Move: 100.0
...Draw Black line from (100.0, 100.0) -> (0.0, 100.0)
Turn: 90.0 from 180.0 -> 270.0
Move: 100.0
...Draw Black line from (0.0, 100.0) -> (-0.0, 0.0)
Turn: 90.0 from 270.0 -> 0.0""" @>
