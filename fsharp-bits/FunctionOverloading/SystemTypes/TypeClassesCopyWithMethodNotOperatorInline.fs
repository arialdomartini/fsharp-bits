module FSharpBits.StateMonad.TypeClassesCopyWithMethodNotOperatorInline

open Xunit
open Swensen.Unquote
open System.Runtime.InteropServices

type PoorBox = PoorBox of string

let poorBoxPlus (PoorBox b1) (PoorBox b2) = PoorBox ($"{b1}, {b2}")

type DefaultImplementations =
    static member Plus(x: list<_>, y, [<Optional>] _mthd: DefaultImplementations) = x @ y
    static member Plus(x: PoorBox, y: PoorBox, [<Optional>]_mthd: DefaultImplementations) = poorBoxPlus x y
    static member Plus(x: string, y: string, [<Optional>]_mthd: DefaultImplementations) = x + y

let inline plus< ^withPlus when (DefaultImplementations or ^withPlus): 
        (static member Plus: ^withPlus * ^withPlus * DefaultImplementations -> ^withPlus)> 
            (x: ^withPlus) (y: ^withPlus) : ^withPlus =
    let inline call (mthd: ^defaultImplementations, input1: ^customType, input2: ^customType) =
        ((^customType or ^defaultImplementations): (static member Plus: _*_*_ -> _) input1, input2, mthd)
    call (Unchecked.defaultof<DefaultImplementations>, x, y)


type Box =
    | Box of string

    static member Plus(Box b1, Box b2, [<Optional>] _mthd: DefaultImplementations) = Box $"{b1}, {b2}"


[<Fact>]
let ``Box.plus`` () =

    let a = Box "first"
    let b = Box "second"

    let result = plus a b

    test <@ result = Box("first, second") @>

[<Fact>]
let ``PoorBox.plus`` () =

    let a = PoorBox "first"
    let b = PoorBox "first"

    let result = plus a b

    test <@ result = PoorBox("first, second") @>

[<Fact>]
let ``String.plus`` () =

    let a = "first"
    let b = "first"

    let result = plus a b

    test <@ result = "first, second" @>
