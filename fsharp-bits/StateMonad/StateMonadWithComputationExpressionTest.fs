module FSharpBits.StateMonad.StateMonadWithParserCombinatorTest

open System.Collections.Generic
open Xunit
open Swensen.Unquote

type GTree<'c> =
    | GLeaf of 'c
    | GNode of GNode<'c>

and GNode<'c> = GTree<'c> * GTree<'c>

type Tree =
    | Leaf of string
    | Node of Node

and Node = Tree * Tree

type ITree<'v> =
    | ILeaf of ('v * int)
    | INode of INode<'v>

and INode<'v> = ITree<'v> * ITree<'v>

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

type WithCounter<'v> = WithCounter of (int -> 'v * int)

let (>>!) (WithCounter f) (counter: int) = f counter

let (>>=) (f: WithCounter<'v>) (g: 'v -> WithCounter<'w>) : WithCounter<'w> =
    WithCounter(fun counter ->
        let rf, cf = f >>! counter
        let rg, cg = g rf >>! cf
        (rg, cg))

let pure' v =
    WithCounter(fun counter -> (v, counter))

type IndexTreeComputation() =
    member this.Bind(m, f) = (m >>= f)

    member this.Return(v) = pure' v

let keepTrack = IndexTreeComputation()
//let keepTrack = StatefulBuilder()

let mapState (from: WithCounter<'v>) (f: int -> int) : WithCounter<'v> =
    WithCounter(fun counter -> from >>! (f counter))

let getCounter = WithCounter(fun c -> (c, c))
let incCounter = WithCounter(fun c -> ((), c + 1))
let setCounter v = WithCounter(fun _ -> ((), v))

let rec indexTree = function
    | Leaf v ->
        keepTrack {
            let! counter = getCounter
            do! setCounter (counter + 1)
            return ILeaf(v, counter)
        }
    | Node(l, r) ->
        keepTrack {
            let! leftIndexed = indexTree l
            let! rightIndexed = indexTree r
            return INode(leftIndexed, rightIndexed)
        }

[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let withIndexes = INode(ILeaf("one", 1), INode(ILeaf("two", 2), ILeaf("three", 3)))

    test <@ fst (indexTree tree >>! 1) = withIndexes @>

let isEven i = i % 2 = 0

type IEnumerable<'v> with
    member this.filter' predicate =
        Seq.foldBack
            (fun i a -> if predicate i then i :: a else a)
            this
            []  

[<Fact>]
let ``filter with fold`` () =
    let xs = [1;2;3;4;5;6]
    let even: int list = List.filter isEven xs
    let myEven: int list = xs.filter' isEven
    
    test <@ even = myEven @>


let (+++) xs ys = xs |> List.append ys

let diamond (upTo: char) =
    let asList x = x |> List.map (_.ToString())
    let letters: string list = ['a' .. upTo] |> asList
    let length = List.length letters
    let spaces n = List.replicate n " " |> asList

    let mirrored xs = xs @ (xs |> List.rev |> List.tail) 
        
    letters
        |> List.mapi (fun i c ->
            (spaces i) +++ [c] +++ (spaces (length - i - 1)))
        |> List.map mirrored
        |> mirrored
        
    
[<Fact>]
let ``diamond test`` () =
    let d = diamond 'd'
    let expected =
        [
            "   a   ";
            "  b b  ";
            " c   c ";
            "d     d";
            " c   c ";
            "  b b  ";
            "   a   ";]
    test <@ expected = (d |> List.map (String.concat "")) @>
