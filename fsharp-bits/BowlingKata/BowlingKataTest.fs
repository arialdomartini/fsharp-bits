module FSharpBits.BowlingKata.BowlingKataTest

open Xunit
open Swensen.Unquote

type RandomGenerator = unit -> int

let makeRandom (expectedValues: int list) =
    let mutable callNumber = 0

    fun () ->
        let toReturn = expectedValues[callNumber]
        callNumber <- callNumber + 1
        toReturn

[<Fact>]
let ``make rnd`` () =
    let rnd = makeRandom [ 2; 8; 3; 4 ]

    Assert.Equal(2, rnd ())
    Assert.Equal(8, rnd ())
    Assert.Equal(3, rnd ())
    Assert.Equal(4, rnd ())

type GameState =
    { Frame: int
      RollNumber: int
      Score: int
      FrameScore: int
      PreviousFrameBonus: int }

let isEven n = n % 2 = 0

let makeGame (rnd: RandomGenerator) =

    let incrementIf predicate value = if predicate then value + 1 else value

    let resetIf predicate value = if predicate then 0 else value

    let game (state: GameState) : GameState =

        let hit = rnd ()
        let evenRoll = state.RollNumber |> isEven

        { Frame = state.Frame |> incrementIf evenRoll
          RollNumber = state.RollNumber + 1
          Score = state.Score + hit + state.PreviousFrameBonus
          FrameScore = state.FrameScore |> resetIf evenRoll
          PreviousFrameBonus = if state.FrameScore + hit = 10 then 10 else 0 }

    game
//
// [<Fact>]
// let ``no hits, no score`` () =
//     let rnd: RandomGenerator = fun () -> 0
//     let play = makeGame rnd
//     let score1 = play ()
//
//     test <@ score1 = 0 @>

// Spare
// frame roll  hit  bonus  score
// 1     1     2    0      2
// 1     2     8           10
// 2     3     3    10     10+3
// 2     4     4    0      10+3+4

[<Fact>]
let ``spare case`` () =

    let rnd = makeRandom [ 2; 8; 3; 4 ]
    let play = makeGame rnd

    let state =
        { Frame = 1
          RollNumber = 1
          Score = 0
          FrameScore = 0
          PreviousFrameBonus = 0 }

    let s1 = play state
    let s2 = play s1
    let s3 = play s2
    let s4 = play s3

    test <@ [ s1.Score; s2.Score; s3.Score; s4.Score ] = [ 2; 10; 10 + 3; 10 + 3 + 4 ] @>



// [<Fact>]
// Strike
// frame roll  hit  bonus  score
// 1    1      10   0      10
// 1    STOP
// 2    1      3    10     3
// 2    2      4    10     10+3+10+4

// let ``strike case`` () =
//     let rnd: RandomGenerator = fun () -> 10
//
//     let score1 = game rnd 1
//
//     test <@ score1 = 0 @>
