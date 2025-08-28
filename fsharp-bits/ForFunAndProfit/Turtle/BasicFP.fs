module FSharpBits.ForFunAndProfit.Turtle.BasicFP
open Common

module Turtle =

    type TurtleState =
        { position: Position
          angle: Angle
          penColor: PenColor
          penState: PenState }

    let move log distance state =
        log $"Move: %0.1f{distance}"

        let newPosition = calculateNewPosition distance state.angle state.position

        if state.penState = Down
        then drawDummyLine log state.position newPosition state.penColor

        { state with position = newPosition }

    let turn log angle state =
        let newAngle = (state.angle + angle) % 360.0<Degrees>
        log $"Turn: %0.1f{angle} from %0.1f{state.angle} -> %0.1f{newAngle}"
        { state with angle = newAngle }

    let penUp log state =
        log "Pen up"
        { state with penState = Up }

    let penDown log state =
        log "Pen down"
        { state with penState = Down }

    let secColor log newColor state =
        log $"Set Color: from %A{state.penColor} -> %A{newColor}"
        { state with penColor = state.penColor }


open Xunit
open Swensen.Unquote
open Turtle

[<Fact>]
let ``draw a triangle`` () =

    let initialState =
        { position = {x = 0; y = 0}
          angle = 0.0<Degrees>
          penColor = Black
          penState = Down }

    let log = stringLog
    let move = move log
    let turn = turn log

    initialState
    |> move  100.0
    |> turn  120.0<Degrees>
    |> move  100.0
    |> turn  120.0<Degrees>
    |> move  100.0
    |> turn  120.0<Degrees>
    |> ignore

    test <@ logS = """
Move: 100.0
...Draw Black line from (0.0, 0.0) -> (100.0, 0.0)
Turn: 120.0 from 0.0 -> 120.0
Move: 100.0
...Draw Black line from (100.0, 0.0) -> (50.0, 86.6)
Turn: 120.0 from 120.0 -> 240.0
Move: 100.0
...Draw Black line from (50.0, 86.6) -> (-0.0, -0.0)
Turn: 120.0 from 240.0 -> 0.0""" @>
