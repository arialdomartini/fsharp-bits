module FSharpBits.MonadTransformers.FunctionOverloading.SystemTypes.InlineFunctionsWithTypeConstraints

open Xunit
open Swensen.Unquote

type Task = { Title: string; Duration: int }

type Box = Box of (int * int * int)

let combineLists = (@)

type List<'a> with
    static member Combine (before: List<'a>, after: List<'a>) : List<'a> =
        combineLists before after

let combineStrings (before: string) (after: string) =
    $"{before} and {after}"

type System.String with
    static member Combine (before: string, after: string) : string =
        combineStrings before after


let inline combine (a: ^a) (b: ^a) : 'a when (^a) : (static member Combine : ^a * ^a -> ^a) =
    (^a : (static member Combine : ^a * ^a -> ^a) (a, b)) 


let combineTasks (before: Task) (after: Task) =
        { Title = $"{before.Title} and then {after.Title}"
          Duration = before.Duration + after.Duration }

type Task with
    static member Combine (t1: Task,  t2: Task) =
        combineTasks t1 t2

let combineBoxes (boxLeft: Box) (boxRight: Box) =
    let (Box (x1, y1, z1)) = boxLeft
    let (Box (x2, y2, z2)) = boxRight
    Box (x1 + x2, max y1 y2, max z1 z2)

type Box with
    static member Combine (b1: Box,  b2: Box) =
        combineBoxes b1 b2



[<Fact>]
let ``combines Tasks`` () =
    let before = { Title = "buy new keyboards"; Duration = 1 }
    let after = { Title = "write more bugs"; Duration = 5 }
    
    test <@ combine before after = {Title = "buy new keyboards and then write more bugs"; Duration = 6} @>
    
[<Fact>]
let ``combines Boxes`` () =
    let left = Box (1, 2, 3)
    let right = Box (10, 20, 30)
    
    test <@ combine left right = Box (11, 20, 30) @>



[<Fact>]
let ``works: combine Strings`` () =
    let before : string = "buy new keyboards"
    let after : string = "write more bugs"
    
    test <@ System.String.Combine(before, after) = "buy new keyboards and write more bugs" @>

[<Fact>]
let ``works: combine Lists invoking directly JoinThem`` () =
    let before : List<int> = [1;2;3]
    let after : List<int> = [4;5;6]
    
    test <@ List<int>.Combine(before, after) = [1;2;3;4;5;6] @>


// The issue arises because statically resolved type parameters in F# do not recognize static members added via type extensions.
// When you use inline functions with static member constraints, the compiler looks for the members directly defined on the type,
// not those added through extensions. Here's the breakdown:
//
// Type Extensions Limitations:
// F# allows adding static members via type extensions, but these are not considered part of the type's intrinsic static methods.
// They are syntactic sugar and not visible when resolving static constraints in inline functions.

// [<Fact>]
// let ``does not work: combine Strings`` () =
//     let before : System.String = "buy new keyboards"
//     let after : System.String = "write more bugs"
//     
//     test <@ combine before after = "buy new keyboards and write more bugs" @>


// [<Fact>]
// let ``does not work: combine Lists`` () =
//     let before : List<int> = [1;2;3]
//     let after : List<int> = [4;5;6]
//     
//     test <@ combine before after = [1;2;3;4;5;6] @>
