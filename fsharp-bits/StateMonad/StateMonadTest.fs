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


type State<'v> = State of (int -> 'v * int) 
let (>!) (State f) index = f index 

type ITree = Tree<string * int>
type App = State<Tree<string * int>>

let mapS (f: 'a -> 'b) (state: State<'a>) =
    State (fun c -> state >! c)

// applicative
// State (a -> b) -> State a -> State b
let (<*>) (f : State<'a -> 'b>) (a: State<'a>) : State<'b> =
    State (fun counter ->
        let vf, cf = f >! counter
        let va, ca = a >! cf
        let result = vf va
        (result, ca))

let pure' v = State (fun counter -> (v, counter))
    
    
let rec index tree : State<ITree> = 
    match tree with
    | Leaf v -> State (fun count -> (Leaf (v, count), count + 1))
    | Node (l, r)  ->
        let buildNode li ri = Node (li, ri)
        (pure' buildNode) <*> index l <*> index r
        
        //     State (fun count ->
        //         let vl, cl = sL >! count
        //         let vr, cr = sR >! cl
        //         Node (vl, vr), cr)
        //     
        //
        // state stateLeft stateRight
        

[<Fact>]
let ``indexes a tree``() =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let indexed = (index tree) >! 1
    let withIndexes = Node(Leaf ("one", 1), Node(Leaf ("two", 2), Leaf ("three", 3)))
    
    test <@ fst indexed = withIndexes @>
