module FSharpBits.MonadTransformers.FunctionOverloading.SystemTypes.InlineFunctionsWithTypeConstraints

open Xunit
open Swensen.Unquote

let joinLists = (@)

type List<'a> with
    static member JoinThem (before: List<'a>, after: List<'a>) : List<'a> =
        joinLists before after

let joinStrings (before: string) (after: string) =
    $"{before} and {after}"

type System.String with
    static member JoinThem (before: string, after: string) : string =
        joinStrings before after


let inline genericJoinThem (a: ^a) (b: ^a) : 'a when (^a) : (static member JoinThem : ^a * ^a -> ^a) =
    (^a : (static member JoinThem : ^a * ^a -> ^a) (a, b)) 



[<Fact>]
let ``works: joins Strings`` () =
    let before : string = "buy new keyboards"
    let after : string = "write more bugs"
    
    test <@ System.String.JoinThem(before, after) = "buy new keyboards and write more bugs" @>

[<Fact>]
let ``works: joins Lists invoking directly JoinThem`` () =
    let before : List<int> = [1;2;3]
    let after : List<int> = [4;5;6]
    
    test <@ List<int>.JoinThem(before, after) = [1;2;3;4;5;6] @>


// The issue arises because statically resolved type parameters in F# do not recognize static members added via type extensions.
// When you use inline functions with static member constraints, the compiler looks for the members directly defined on the type,
// not those added through extensions. Here's the breakdown:
//
// Type Extensions Limitations:
// F# allows adding static members via type extensions, but these are not considered part of the type's intrinsic static methods.
// They are syntactic sugar and not visible when resolving static constraints in inline functions.

// [<Fact>]
// let ``does not work: joins Strings`` () =
//     let before : System.String = "buy new keyboards"
//     let after : System.String = "write more bugs"
//     
//     test <@ genericJoinThem before after = "buy new keyboards and write more bugs" @>


// [<Fact>]
// let ``does not work: joins Lists`` () =
//     let before : List<int> = [1;2;3]
//     let after : List<int> = [4;5;6]
//     
//     test <@ genericJoinThem before after = [1;2;3;4;5;6] @>
