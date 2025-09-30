module FSharpBits.ForFunAndProfit.Turtle.BasicOO
open Common


type Turtle(log) =
    let initialPosition = {x = 0; y = 0}
    let initialColor = Black
    let initialPenState = Down

    // member private val currentPosition = initialPosition with get, set
    let mutable currentPosition = initialPosition
    let mutable currentAngle = 0.0<Degrees>
    let mutable currentColor = initialColor
    let mutable currentPenState = initialPenState


    member this.Move(distance) =
        log $"Move: %0.1f{distance}"

        let newPosition = calculateNewPosition distance currentAngle currentPosition

        if currentPenState = Down
        then drawDummyLine log currentPosition newPosition currentColor

        currentPosition <- newPosition

    member this.Turn(angle) =
        let newAngle = (currentAngle + angle) % 360.0<Degrees>
        log $"Turn: %0.1f{angle} from %0.1f{currentAngle} -> %0.1f{newAngle}"
        currentAngle <- newAngle

    member this.PenUp() =
        log "Pen up"
        currentPenState <- Up

    member this.PenDown() =
        log "Pen down"
        currentPenState <- Down

    member this.SecColor(newColor) =
        log $"Set Color: from %A{currentColor} -> %A{newColor}"
        currentColor <- newColor


open Xunit
open Swensen.Unquote

[<Fact>]
let ``draw a triangle`` () =
    let turtle = Turtle(stringLog)

    turtle.Move 100.0
    turtle.Turn 120.0<Degrees>
    turtle.Move 100.0
    turtle.Turn 120.0<Degrees>
    turtle.Move 100.0
    turtle.Turn 120.0<Degrees>

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
