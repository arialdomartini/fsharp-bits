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

let map f (tree: Tree<string>) =
    match tree with
    | Leaf v -> Leaf(v.Length)
    | Node (l, r) -> Node(f l, f r)

let rec calculateLengths (tree: Tree<string>) =
    map calculateLengths tree
    
[<Fact>]
let ``counting tree leaves`` () =

    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let numberOfLeaves = countLeaves tree

    test <@ numberOfLeaves = 3 @>

[<Fact>]
let ``changing tree leaves content`` () =

    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let containingLengths = calculateLengths tree

    let treeOfLengths = Node(Leaf 3, Node(Leaf 3, Leaf 5))

    test <@ containingLengths = treeOfLengths @>

let rec index tree count =
    match tree with
    | Leaf v -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        let (vl, cl) = index l count
        let (vr, cr) = index r cl
        (Node (vl, vr), cr)

[<Fact>]
let ``indexes a tree``() =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let indexed = index tree 1
    let withIndexes = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3)))
    
    test <@ fst indexed = withIndexes @>
