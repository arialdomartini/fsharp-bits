module FSharpBits.MonadTransformers.FunctionOverloading.WithInterface

open Xunit
open Swensen.Unquote


type Joinable<'T> =
    abstract member Join : 'T -> 'T

type Task = { Title: string; Duration: int } with
    interface Joinable<Task> with
        member this.Join(other: Task) =
            failwith "Join method for Task is not implemented"


type Box = Box of (int * int * int) with
    interface Joinable<Box> with
        member this.Join(other: Box) =
            failwith "Join method for Box is not implemented"

let join (t1: 'T when 'T :> Joinable<'T>) (t2: 'T) =
    (t1 :> Joinable<'T>).Join(t2)

[<Fact>]
let ``joins Tasks`` () =
    let before = { Title = "buy new keyboards"; Duration = 1 }
    let after = { Title = "write more bugs"; Duration = 5 }
    
    test <@ join before after = {Title = "buy new keyboards and write more bugs"; Duration = 6} @>
    
[<Fact>]
let ``joins Boxes`` () =
    let left = Box (1, 2, 3)
    let right = Box (10, 20, 30)
    
    test <@ join left right = Box (11, 20, 30) @>
