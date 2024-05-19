module FSharpBits.StateMonad.StateMonadTest

open Xunit
open Swensen.Unquote

type Tree<'v> = Leaf of 'v | Node of Node<'v>
and Node<'v> = Tree<'v> * Tree<'v>

// Tree 'v -> int
let rec countLeaves tree =
    match tree with
    | Leaf _ -> 1
    | Node (l, r) -> countLeaves l + countLeaves r  


// (string -> int) -> Tree<'string> -> Tree<'int>
let rec map f tree =
    match tree with
    | Leaf v -> Leaf (f v)
    | Node (l, r) -> Node (map f l, map f r)

let (^) = map

[<Fact>]
let ``count tree leaves`` () =
    let tree = Node(Node(Leaf("One"), Leaf("Two")), Leaf "Three")
    
    let numberOfLeaves = countLeaves tree
    
    test <@ numberOfLeaves = 3 @>


[<Fact>]
let ``count length of leaves value`` () =
    let tree = Node(Node(Leaf("One"), Leaf("Two")), Leaf "Three")
    let treeOfNumbers = Node(Node(Leaf(3), Leaf(3)), Leaf 5)
    
    let treeOfLengths = String.length^ tree
    
    test <@ treeOfLengths = treeOfNumbers @>
