module FSharpBits.StateMonad.StateMonadTest

open Xunit
open Swensen.Unquote

type GTree<'c> =
    | GLeaf of 'c
    | GNode of (GTree<'c> * GTree<'c>)

type Tree =
    | Leaf of string
    | Node of (Tree * Tree)

type ITree =
    | ILeaf of (string * int)
    | INode of (ITree * ITree)

let rec countLeaves tree =
    match tree with
    | Leaf l -> 1
    | Node(l, r) -> countLeaves l + countLeaves r


type MyGeneric<'a> = MyGeneric of 'a

let x: MyGeneric<int> = MyGeneric 12

[<Fact>]
let ``counting tree leaves`` () =

    let tree: Tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))

    let numberOfLeaves = countLeaves tree

    test <@ numberOfLeaves = 3 @>

let rec mapTree (f: 'a -> 'b) : (GTree<'a> -> GTree<'b>) =
    fun tree ->
        match tree with
        | GLeaf v -> GLeaf(f v)
        | GNode(l, r) -> GNode((mapTree f l), (mapTree f r))


[<Fact>]
let ``mapping a tree`` () =
    let tree = GNode(GLeaf "one", GNode(GLeaf "two", GLeaf "three"))

    let mappedToIntegers = mapTree String.length tree
    let containingLengthOfWords = GNode(GLeaf 3, GNode(GLeaf 3, GLeaf 5))

    test <@ mappedToIntegers = containingLengthOfWords @>


type WithCounter =
    WithCounter of (int -> ITree * int)

let (>>!) (WithCounter f) counter = f counter



let (>>>) (f : WithCounter) (g : ITree -> WithCounter) : WithCounter =
    WithCounter (
        fun counter->
            let rf, cf = f >>! counter
            let rg, cg = g rf >>! cf
            (rg, cg))


let pure' v =
    WithCounter (fun counter -> (v, counter))

type IndexTreeComputation() =
    member this.Bind(m, f) = (m >>> f)

    member this.Return(v) = pure' v

let keepTrack = IndexTreeComputation()
    
let rec indexTree tree : WithCounter =
    match tree with
    | Leaf v -> WithCounter (fun counter -> (ILeaf(v, counter), counter + 1))
    | Node(l, r) ->
        keepTrack {
            let! rl = indexTree l
            let! rr = indexTree r
            return (INode (rl, rr))
        }
        
[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let withIndexes = INode(ILeaf("one", 1), INode(ILeaf("two", 2), ILeaf("three", 3)))

    test <@ fst (indexTree tree >>! 1) = withIndexes @>
