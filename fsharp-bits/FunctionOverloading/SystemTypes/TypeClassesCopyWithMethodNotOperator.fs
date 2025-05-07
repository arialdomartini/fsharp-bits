module FSharpBits.StateMonad.TypeClassesCopyWithMethodNotOperator

open Xunit
open Swensen.Unquote
open System.Runtime.InteropServices

type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end

type PoorBox = PoorBox of string

type Plus =
    static member        Plus (x: list<_>           , y                    , _mthd: Plus    ) = x @ y
    static member        Plus (x: PoorBox           , y: PoorBox           , _mthd: Plus    ) = x
    static member        Plus (x: string            , y: string            , _mthd: Plus    ) = x + y
    
    static member inline Invoke (x: 'Plus) (y: 'Plus) : 'Plus =
        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member Plus : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Plus>, x, y)

    
let inline plus<^Monoid when (Plus or ^Monoid) : (static member Plus : ^Monoid * ^Monoid * Plus -> ^Monoid)>
    (x: 'Monoid) (y: 'Monoid) : 'Monoid =
        Plus.Invoke x y

    
type Box = Box of string with
    static member Plus (Box b1, Box b2, [<Optional>]_mthd: Plus) =
        Box $"{b1}, {b2}"
        

[<Fact>]
let ``Box.plus`` () =
   
    let a = Box "first"
    let b = Box "first"

    let result = plus a b
    
    test <@ result = Box ("first, second") @>

[<Fact>]
let ``PoorBox.plus`` () =
   
    let a = PoorBox "first"
    let b = PoorBox "first"

    let result = plus a b
    
    test <@ result = PoorBox ("first, second") @>

[<Fact>]
let ``String.plus`` () =
   
    let a = "first"
    let b = "first"

    let result = plus a b
    
    test <@ result = ("first, second") @>
