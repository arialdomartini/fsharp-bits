module FSharpBits.MonadTransformers.FunctionOverloading.SystemTypes.FunctionOverloadingWithModules

open Xunit
open Swensen.Unquote

module Tasks =
    type Task = { Title: string; Duration: int }

    let combine (before: Task) (after: Task) =
        { Title = $"{before.Title} and then {after.Title}"
          Duration = before.Duration + after.Duration }

module Boxes =
    type Box = Box of (int * int * int)

    let combine (boxLeft: Box) (boxRight: Box) =
        let (Box (x1, y1, z1)) = boxLeft
        let (Box (x2, y2, z2)) = boxRight
        Box (x1 + x2, max y1 y2, max z1 z2)

module Lists =    
    let combine (before: 'a list) (after: 'a list) =
        before @ after

module Strings =
    let combine (message: string) (pronunciation: string) =
        $"{message} /{pronunciation}/"

open Tasks
open Boxes

[<Fact>]
let ``combines Tasks`` () =
    let before = { Title = "buy new keyboards"; Duration = 1 }
    let after = { Title = "write more bugs"; Duration = 5 }
    
    test <@ Tasks.combine before after = {Title = "buy new keyboards and then write more bugs"; Duration = 6} @>
    
[<Fact>]
let ``combines Boxes`` () =
    let left = Box (1, 2, 3)
    let right = Box (10, 20, 30)
    
    test <@ Boxes.combine left right = Box (11, 20, 30) @>

[<Fact>]
let ``combine strings`` () =
    let message = "F# is cool!"
    let pronunciation = "ɛf ʃɑːrp ɪz kuːl"
    
    test <@ Strings.combine message pronunciation = "F# is cool! /ɛf ʃɑːrp ɪz kuːl/" @>

[<Fact>]
let ``combine Lists`` () =
    let before = [1;2;3]
    let after = [4;5;6]
    
    test <@ Lists.combine before after = [1;2;3] @>
