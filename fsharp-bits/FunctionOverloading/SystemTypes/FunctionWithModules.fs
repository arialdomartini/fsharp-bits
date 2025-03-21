module FSharpBits.MonadTransformers.FunctionOverloading.SystemTypes.FunctionOverloadingWithModules

open Xunit
open Swensen.Unquote

module Tasks =
    type Task = { Title: string; Duration: int }

    let join (t1: Task) (t2: Task) =
        { Title = $"{t1.Title} and {t2.Title}"
          Duration = t1.Duration + t2.Duration }

module Boxes =
    type Box = Box of (int * int * int)

    let join (boxLeft: Box) (boxRight: Box) =
        let (Box (x1, y1, z1)) = boxLeft
        let (Box (x2, y2, z2)) = boxRight
        Box (x1 + x2, max y1 y2, max z1 z2)

module Lists =
    let join (before: 'a list) (after: 'a list) =
        before @ after


open Tasks
open Boxes
    
[<Fact>]
let ``joins Tasks`` () =
    let before = { Title = "buy new keyboards"; Duration = 1 }
    let after = { Title = "write more bugs"; Duration = 5 }
    
    test <@ Tasks.join before after = {Title = "buy new keyboards and write more bugs"; Duration = 6} @>
    
[<Fact>]
let ``joins Boxes`` () =
    let left = Box (1, 2, 3)
    let right = Box (10, 20, 30)
    
    test <@ Boxes.join left right = Box (11, 20, 30) @>

[<Fact>]
let ``joins Lists`` () =
    let before = [1;2;3]
    let after = [4;5;6]
    
    test <@ Lists.join before after = [1;2;3;4;5;6] @>
