module FSharpBits.StateMonad.StateMonadTest

open Xunit
open Swensen.Unquote

type Tree<'v> = Leaf of 'v | Node of Node<'v>
and Node<'v> = Tree<'v> * Tree<'v>

let buildNode left right = Node (left, right)

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
    let tree = Node(Node(Leaf "One", Leaf "Two"), Leaf "Three")
    
    let numberOfLeaves = countLeaves tree
    
    test <@ numberOfLeaves = 3 @>

[<Fact>]
let ``count length of leaves value`` () =
    let tree = Node(Node(Leaf "One", Leaf "Two"), Leaf "Three")
    let treeOfNumbers = Node(Node(Leaf 3, Leaf 3), Leaf 5)
    
    let treeOfLengths = String.length^ tree
    
    test <@ treeOfLengths = treeOfNumbers @>

type WithCounter<'v> = WithCounter of (int -> 'v * int)

let run (WithCounter f) counter = f counter
let pure' v = WithCounter (fun counter -> (v, counter))

type ITree = Tree<string * int>

let (<*>) (f : WithCounter<'a -> 'b>) (a: WithCounter<'a>) : WithCounter<'b> =
    WithCounter (fun counter ->
        let rf, cf = run f counter
        let ra, ca = run a cf
        let ra = rf ra
        (ra, ca))

let getState = WithCounter (fun counter -> (counter, counter))
let incState = WithCounter (fun counter -> ((), counter+1))
let buildLeaf = pure' (fun v state _ -> Leaf (v, state))

let rec index tree =
    match tree with
    | Leaf v ->
        buildLeaf <*> (pure' v) <*> getState <*> incState
    | Node (l, r) ->
        pure' buildNode <*> index l <*> index r

[<Fact>]
let ``index tree`` () =
    let tree = Node(Node(Leaf "One", Leaf "Two"), Leaf "Three")
    let treeWithIndexes = Node(Node(Leaf("One", 1), Leaf("Two", 2)), Leaf ("Three", 3))
    
    let indexedTree = run (index tree) 1
    
    test <@ fst indexedTree = treeWithIndexes @>
