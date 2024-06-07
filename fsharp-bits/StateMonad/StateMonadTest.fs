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


// Tree a -> int
let rec countLeaves = function
    | Leaf _ -> 1
    | Node (l, r) -> countLeaves l  + countLeaves r 

//    f          tree      out
// (a -> b) -> (Tree a -> Tree b)
let rec map f =
   fun tree ->
       match tree with
       | Leaf v -> Leaf (f v)
       | Node (l, r) -> Node (map f l, map f r) 

// Tree a -> count -> (Tree (a, Int), Int)    
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


[<Fact>]
let ``count number of leaves`` () =
    let tree = Node (Leaf (), (Node (Leaf (),Leaf ())))
    
    let numberOfLeaves = countLeaves tree
    
    test <@ numberOfLeaves = 3 @>

let (^) = map

[<Fact>]
let ``calculate length of leaves`` () =
    let treeOfWords = Node (Leaf "one", (Node (Leaf "two",Leaf "three")))
    let treeOfNumbers = Node (Leaf 3, (Node (Leaf 3,Leaf 5)))
    
    // String -> Int
    let f = String.length
    
    // Tree String -> Tree Int
    let foo = (map String.length)
    
    let treeOfLengths = String.length^ treeOfWords
    
    test <@ treeOfLengths = treeOfNumbers @>
