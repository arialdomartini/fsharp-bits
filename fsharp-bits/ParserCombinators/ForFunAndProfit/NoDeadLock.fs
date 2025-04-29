module FSharpBits.ParserCombinators.ForFunAndProfit.NoDeadLock

open System
open System.Threading
open Xunit

let x = obj ()

let threadA: Async<unit> =
    async {
        lock x (fun () ->
            Console.WriteLine("Thread A: Acquired lock")
            Thread.Sleep(1000))
    }

let threadB =
    async {
        lock x (fun () ->
            Console.WriteLine("Thread B: Acquired lock")
            Thread.Sleep(1000))
    }

[<Fact>]
let ``Run async threads concurrently`` () = task {
    let taskA = Async.StartAsTask threadA
    let taskB = Async.StartAsTask threadB

    do! taskA
    do! taskB
}
