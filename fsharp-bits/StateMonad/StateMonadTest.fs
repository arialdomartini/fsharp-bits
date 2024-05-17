module FSharpBits.StateMonad.StateMonadTest

open Xunit

type GTree<'c> =
    | GLeaf of 'c
    | GNode of (GTree<'c> * GTree<'c>)

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

let rec mapTree (f: 'a -> 'b) : (GTree<'a> -> GTree<'b>) =
    fun tree ->
        match tree with
        | GLeaf v -> GLeaf (f v)
        | GNode (l, r) -> GNode ((mapTree f l), (mapTree f r)) 
        
        
[<Fact>]
let ``mapping a tree`` () =
    let tree               = GNode(GLeaf "one", GNode(GLeaf "two", GLeaf "three"))
    
    let mappedToIntegers = mapTree String.length tree
    let containingIntegers = GNode(GLeaf 3, GNode(GLeaf 3, GLeaf 5))
 
    Assert.Equal(containingIntegers, mappedToIntegers)
