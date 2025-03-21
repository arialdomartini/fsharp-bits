module FSharpBits.MonadTransformers.FunctionOverloading.InlineFunctionsWithTypeConstraintsCurried

open Xunit
open Swensen.Unquote

module Tasks =
    type Task = { Title: string; Duration: int }
    
    let private join (t1: Task) (t2: Task) =
        { Title = $"{t1.Title} and {t2.Title}"
          Duration = t1.Duration + t2.Duration }

    type Task with
        static member Join (t1: Task)  (t2: Task) =
            join t1 t2

module Boxes =
    type Box = Box of (int * int * int)

    let private joinBoxes (boxLeft: Box) (boxRight: Box) =
        let (Box (x1, y1, z1)) = boxLeft
        let (Box (x2, y2, z2)) = boxRight
        Box (x1 + x2, max y1 y2, max z1 z2)

    type Box with
        static member Join (t1: Box) (t2: Box) =
            joinBoxes t1 t2


open Tasks
open Boxes
    
let inline genericJoin<'a when (^a) : (static member Join : ^a -> ^a -> ^a)> (a: ^a) (b: ^a) =
    (^a : (static member Join : ^a -> ^a -> ^a) (a, b)) 
//     
// [<Fact>]
// let ``joins Tasks`` () =
//     let before = { Title = "buy new keyboards"; Duration = 1 }
//     let after = { Title = "write more bugs"; Duration = 5 }
//     
//     test <@ genericJoin before after = {Title = "buy new keyboards and write more bugs"; Duration = 6} @>
//     
// [<Fact>]
// let ``joins Boxes`` () =
//     let left = Box (1, 2, 3)
//     let right = Box (10, 20, 30)
//     
//     test <@ genericJoin left right = Box (11, 20, 30) @>
