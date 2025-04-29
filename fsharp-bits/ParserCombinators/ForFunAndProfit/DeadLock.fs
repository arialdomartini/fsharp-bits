module FSharpBits.ParserCombinators.ForFunAndProfit.DeadLock

open System
open System.Threading
open Xunit

let x = obj ()
let y = obj ()
let z = obj ()

let threadA: Async<unit> =
    async {
        lock x (fun () ->
            Thread.Sleep(1000)

            lock y (fun () ->
                Console.WriteLine("Thread A: Acquired both locks")))
    }

let threadB =
    async {
        lock y (fun () ->
            Thread.Sleep(1000)

            lock z (fun () ->
                Console.WriteLine("Thread B: Acquired both locks")))
    }

let ``A and B combined`` () = task {
    let taskA = Async.StartAsTask threadA
    let taskB = Async.StartAsTask threadB

    do! taskA
    do! taskB
}


[<Fact>]
let ``a deadlock 2`` () =
    ``A and B combined`` ()

// [<Fact>]
// let ``a deadlock!`` () = task {
//     let taskA = Async.StartAsTask threadA
//     let taskB = Async.StartAsTask threadB
//
//     do! taskA
//     do! taskB
// }
