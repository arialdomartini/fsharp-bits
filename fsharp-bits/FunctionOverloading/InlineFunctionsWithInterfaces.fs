module FSharpBits.MonadTransformers.FunctionOverloading.InlineFunctionsWithInterfaces

open Xunit
open Swensen.Unquote

type WithJoin<'a> =
    abstract member Join : 'a -> 'a


module Tasks =
    type Task =
        { Title: string; Duration: int }
        
        interface WithJoin<Task> with
            member this.Join (t2: Task) =
                { Title = $"{this.Title} and {t2.Title}"
                  Duration = this.Duration + t2.Duration }
    

module Boxes =
    type Box = Box of (int * int * int) with 
        interface WithJoin<Box> with
            member this.Join (boxRight: Box) =
                let boxLeft = this
                let (Box (x1, y1, z1)) = boxLeft
                let (Box (x2, y2, z2)) = boxRight
                Box (x1 + x2, max y1 y2, max z1 z2)



open Tasks
open Boxes
    
let genericJoin<'a when 'a :> WithJoin<'a>> (a: 'a) (b: 'a) =
    a.Join(b) 

[<Fact>]
let ``joins Tasks`` () =
    let before = { Title = "buy new keyboards"; Duration = 1 }
    let after = { Title = "write more bugs"; Duration = 5 }
    
    test <@ genericJoin before after = {Title = "buy new keyboards and write more bugs"; Duration = 6} @>
    
[<Fact>]
let ``joins Boxes`` () =
    let left = Box (1, 2, 3)
    let right = Box (10, 20, 30)
    
    test <@ genericJoin left right = Box (11, 20, 30) @>
