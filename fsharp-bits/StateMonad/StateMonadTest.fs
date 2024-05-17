module FSharpBits.StateMonad.StateMonadTest

open Xunit

type Tree =
    | Leaf of string
    | Node of (Tree * Tree)

let rec countLeaves tree =
    match tree with
    | Leaf l -> 1
    | Node(l, r) -> countLeaves l + countLeaves r

[<Fact>]
let ``counting tree leaves`` () =

    let tree: Tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let numberOfLeaves = countLeaves tree

    Assert.Equal(3, numberOfLeaves)
