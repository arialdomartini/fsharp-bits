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

type WithCount<'a> = WithCount of (int -> ('a * int)) 

let run (WithCount f) count = f count
let build l r = Node (l, r)    
let pure' v = WithCount (fun count -> (v, count))

//
        // WithCount (fun count ->
        //     let li, lc = run (index l) count
        //     let ri, rc = run (index r) lc
        //     build li ri, rc)


// (a -> b) -> a -> b 
// WithCount (a -> b) -> WithCount a  -> WithCount b 
let (<*>) f v =
    WithCount (fun count ->
        let fv, fc = run f count
        let vv, vc = run v fc
        (fv vv, vc))

// (a -> b) -> (WithCount a -> WithCount b)
let (^) f =
    fun a ->
        WithCount (fun count ->
            let va, ca = run a count
            (f va), ca)
    
// a -> (a -> b) -> b

// a' -> (a -> b') -> b'
// a' -> (a -> b) ->  b'
    
// WithCount a -> (a -> WithCount b) -> WithCount b
let (>>=) v f =
    WithCount (fun count ->
        let vv, cv = run v count
        let withCountB = f vv
        run withCountB cv)

type KeepState() =
    member this.Bind(v, f) = v >>= f
    member this.Return(v) = pure' v

// .... -> WithCount b
let getCount =
    WithCount (fun count -> (count, count))

// Int -> WithCount a
let putCount newCount =
    WithCount (fun _ -> (() , newCount))
    
    
let keepState = KeepState()    

let rec index tree =
    match tree with
    | Leaf v      ->
        keepState {
            let! count = getCount
            do! putCount (count + 1)
            return Leaf (v, count)
        }
        //WithCount (fun count -> (Leaf (v, count), count + 1))
        
    | Node (l, r) ->
        // let li = index l
        // li >>= (fun lv -> (
        //     let ri = index r
        //     ri >>= fun rv -> pure' (build lv rv)))

        keepState {
            let! li = index l
            let! ri = index r
            return build li ri
        } 
        
        //build^ (index l) <*> index r
        
        // WithCount (fun count ->
        //     let li, lc = run (index l) count
        //     let ri, rc = run (index r) lc
        //     build li ri, rc)


[<Fact>]
let ``indexes a tree`` () =
    let tree = Node (Leaf "one", (Node (Leaf "two",Leaf "three")))
    let tree' = Node (Leaf ("one", 1), (Node (Leaf ("two", 2),Leaf ("three",3))))
    
    let indexedTree, _ = run (index tree) 1
    
    test <@ indexedTree = tree' @>
