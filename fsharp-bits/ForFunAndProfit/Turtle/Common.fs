module FSharpBits.ForFunAndProfit.Turtle.Common

open System

type Distance = float

[<Measure>]
type Degrees

// can also be written
// type [<Measure>] Degrees

type Angle = float<Degrees>

type PenState =
    | Up
    | Down

type PenColor =
    | Black
    | Red
    | Blue

type Position = { x: float; y: float }


let round (n: float) = Math.Round(n, 2)

let calculateNewPosition (distance: Distance) (angle: Angle) (currentPosition: Position) =

    let project projection n =
        let angleInRads = angle * (Math.PI/180.0 * 1.0<1/Degrees>)
        n + distance * (projection angleInRads)

    let newX = (project cos >> round) currentPosition.x
    let newY = (project sin >> round) currentPosition.y

    { x = newX; y = newY }


let drawDummyLine log currentPosition newPosition color =
    log $"...Draw %A{color} line from (%0.1f{currentPosition.x}, %0.1f{currentPosition.y}) -> (%0.1f{newPosition.x}, %0.1f{newPosition.y})"

let mutable logS = ""

let stringLog msg =
    logS <- $"{logS}\r\n{msg}"
    ()
