module FSharpBits.StateMonad.StateMonadTest

open Xunit
open Swensen.Unquote

type Tree<'c> =
    | Leaf of 'c
    | Node of GNode<'c>
and GNode<'c> = Tree<'c> * Tree<'c>

// type Tree = GTree<string>
// type Leaf = GLeaf<string>
// type Node = Tree * Tree

let rec countLeaves tree : int =
    match tree with
    | Leaf _ -> 1
    | Node (l, r) -> countLeaves l + countLeaves r


[<Fact>]
let ``counting tree leaves`` () =

    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let numberOfLeaves = countLeaves tree

    test <@ numberOfLeaves = 3 @>
