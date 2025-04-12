module FSharpBits.MonadTransformers.FunctionOverloading.SystemTypes.NoFunctionOverloading

open Xunit
open Swensen.Unquote

type Task = { Title: string; Duration: int }
type Box = Box of (int * int * int)


let combineTasks (before: Task) (after: Task) =
    { Title = $"{before.Title} and then {after.Title}"
      Duration = before.Duration + after.Duration }

let combineBoxes (boxLeft: Box) (boxRight: Box) =
    let (Box (x1, y1, z1)) = boxLeft
    let (Box (x2, y2, z2)) = boxRight
    Box (x1 + x2, max y1 y2, max z1 z2)
    
let combineLists (before: 'a list) (after: 'a list) =
    before @ after

let combineStrings (message: string) (pronunciation: string) =
    $"{message} /{pronunciation}/"

[<Fact>]
let ``combines Tasks`` () =
    let before = { Title = "buy new keyboards"; Duration = 1 }
    let after = { Title = "write more bugs"; Duration = 5 }
    
    test <@ combineTasks before after = {Title = "buy new keyboards and then write more bugs"; Duration = 6} @>
    
[<Fact>]
let ``combines Boxes`` () =
    let left = Box (1, 2, 3)
    let right = Box (10, 20, 30)
    
    test <@ combineBoxes left right = Box (11, 20, 30) @>

[<Fact>]
let ``combine strings`` () =
    let message = "F# is cool!"
    let pronunciation = "ɛf ʃɑːrp ɪz kuːl"
    
    test <@ combineStrings message pronunciation = "F# is cool! /ɛf ʃɑːrp ɪz kuːl/" @>

[<Fact>]
let ``combine Lists`` () =
    let before = [1;2;3]
    let after = [4;5;6]
    
    test <@ combineLists before after = [1;2;3] @>
