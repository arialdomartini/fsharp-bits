module FSharpBits.ForFunAndProfit.Catamorphism.Folds.WeirdRecursion

open Xunit

let rec succeeds (dummy: bool) acc n =
    match n with
    | 0 -> acc
    | _ -> succeeds true (acc + 1) (n - 1)


[<TailCall>]
let rec fails (dummy: bool) acc n =
    let recurse = fails true
    match n with
    | 0 -> acc
    | _ -> recurse (acc + 1) (n - 1)


[<Fact>]
let ``this succeeds`` () =
    let n = 20000
    Assert.Equal(n, succeeds true 0 n)

[<Fact(Skip = "This causes a stack overflow")>]
let ``this fails`` () =
    let n = 20000
    Assert.Equal(n, fails true 0 n)
