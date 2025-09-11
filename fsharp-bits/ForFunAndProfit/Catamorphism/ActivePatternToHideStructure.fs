module FSharpBits.ForFunAndProfit.Catamorphism.ActivePatternToHideStructure

type Number =
    | One
    | Two
    | Three
    | Four

let (|One|Two|Three|) number =
    match number with
    | One -> One
    | Two -> Two
    | Three -> Three
    | Four -> One


open Xunit
open Swensen.Unquote

let getNumber () = Four

[<Fact>]
let ``Four is not visible`` () =

    let number = getNumber ()

    let result =
        match number with
        | One -> "one"
        | Two -> "two"
        | Three -> "three"

    test <@ result = "one" @>
