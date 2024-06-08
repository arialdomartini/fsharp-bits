module FSharpBits.StateMonad.StateMonadTest

open Xunit
open Swensen.Unquote

//   type  = data constructor 
type Tree<'a> = Leaf of 'a | Node of (Tree<'a>* Tree<'a>)
    
    

//     Leaf

//       Node
//       /  \
//     Leaf Leaf


//          Node
//         /    \
//      Leaf one    Node
//              /   \
//            Leaf two  Leaf three

//          Node
//         /    \
//      Leaf one,1    Node
//              /   \
//         Leaf two,2  Leaf three,3


let rec countLeaves = function
    | Leaf _ -> 1
    | Node (l, r) -> countLeaves l  + countLeaves r 

let rec index tree =
    match tree with
    | Leaf v      -> fun count -> (Leaf (v, count), count + 1)
    | Node (l, r) ->
        fun count ->
            let li, lc = index l count
            let ri, rc = index r lc
            Node (li, ri), rc


[<Fact>]
let ``indexes a tree`` () =
    let tree = Node (Leaf "one", (Node (Leaf "two",Leaf "three")))
    let tree' = Node (Leaf ("one", 1), (Node (Leaf ("two", 2),Leaf ("three",3))))
    
    let indexedTree, _ = index tree 1
    
    test <@ indexedTree = tree' @>
