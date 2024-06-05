module FSharpBits.StateMonad.StateMonadTest

open Xunit
open Swensen.Unquote

type Tree<'a> =
    | Leaf of 'a
    | Node of (Tree<'a> * Tree<'a>)

type State<'a> = State of (int -> 'a * int)

let run (State f) s = f s

let (<*>) f v =
    State(fun s ->
        let fv, fs = run f s
        let vv, vs = run v fs
        (fv vv, vs))

let rec map f sm =
    State(fun s ->
        let v, s' = run sm s
        f v, s')

let build l r = Node(l, r)
let (^) = map

let (<*) f v =
    State(fun s ->
        let fv, fs = run f s
        let _, s' = run v fs
        (fv, s'))

let get = State(fun s -> (s, s))
let put v = State(fun _ -> ((), v))
let inc = State(fun s -> ((), s + 1))
let pure' v = State(fun s -> (v, s))
let buildLeaf v s = Leaf (v, s)

let rec index =
    function
    // | Leaf a -> State (fun s -> (Leaf (a, s), s + 1))
    | Leaf a -> buildLeaf^ (pure' a) <*> get <* inc
    | Node(l, r) ->
        let li = index l
        let ri = index r
        build ^ li <*> ri

[<Fact>]
let ``indexes a tree`` () =
    let tree = Node(Leaf "one", Node(Leaf "two", Leaf "three"))
    let withIndexes = Node(Leaf("one", 1), Node(Leaf("two", 2), Leaf("three", 3)))

    let indexed, _ = run (index tree) 1

    test <@ indexed = withIndexes @>
