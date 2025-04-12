module FSharpBits.MonadTransformers.FunctionOverloading.SystemTypes.InlineFunctionsWithInterfaces

open System
open System.Threading.Tasks.Dataflow
open Xunit
open Swensen.Unquote

type Combinable<'a> =
    abstract member Combine : 'a -> 'a


module Tasks =
    type Task =
        { Title: string; Duration: int }
        
        interface Combinable<Task> with
            member this.Combine (t2: Task) =
                { Title = $"{this.Title} and {t2.Title}"
                  Duration = this.Duration + t2.Duration }
    

module Boxes =
    type Box = Box of (int * int * int) with 
        interface Combinable<Box> with
            member this.Combine (boxRight: Box) =
                let boxLeft = this
                let (Box (x1, y1, z1)) = boxLeft
                let (Box (x2, y2, z2)) = boxRight
                Box (x1 + x2, max y1 y2, max z1 z2)

// module Lists =
//     type List<'a> with
//         interface WithJoin<List<'a>> with
//             member this.Join (after: List<'a>) =
//                 ...
            

open Tasks
open Boxes
    
let genericJoin<'a when 'a :> Combinable<'a>> (a: 'a) (b: 'a) =
    a.Combine(b) 

[<Fact>]
let ``combine Tasks`` () =
    let before = { Title = "buy new keyboards"; Duration = 1 }
    let after = { Title = "write more bugs"; Duration = 5 }
    
    test <@ genericJoin before after = {Title = "buy new keyboards and write more bugs"; Duration = 6} @>
    
[<Fact>]
let ``combine Boxes`` () =
    let left = Box (1, 2, 3)
    let right = Box (10, 20, 30)
    
    test <@ genericJoin left right = Box (11, 20, 30) @>
