module FSharpBits.ParserCombinators.ForFunAndProfit.DeadLockInt

open System.Threading
open Xunit
open Swensen.Unquote

let x = obj ()
let y = obj ()

let threadA =
    async {
        return
            lock x (fun () ->
                Thread.Sleep(1000)

                lock y (fun () -> 21))
    }

let threadB =
    async {
        return
            lock y (fun () ->
                Thread.Sleep(1000)

                lock x (fun () -> 21))
    }

let combined =
    task {
        let taskA = Async.StartAsTask threadA
        let taskB = Async.StartAsTask threadB

        let! a = taskA
        let! b = taskB

        return a + b
    }

[<Fact>]
let ``threadA only`` () =
    task {
        let! b = threadA
        test <@ b = 21 @>
    }

[<Fact>]
let ``threadB only`` () =
    task {
        let! a = threadB
        test <@ a = 21 @>
    }


[<Fact(Skip="they produce a deadlock")>]
let ``thread A and B combined cause a deadlock`` () =
    task {
        let! ab = combined
        test <@ ab = 42 @>
    }
