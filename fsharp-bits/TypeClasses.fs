module FSharpBits.StateMonad.TypeClasses

open Xunit
open Swensen.Unquote
open System.Runtime.InteropServices
open System.Text

type Default6 = class end
type Default5 = class inherit Default6 end
type Default4 = class inherit Default5 end
type Default3 = class inherit Default4 end
type Default2 = class inherit Default3 end
type Default1 = class inherit Default2 end


type Plus =
//    static member inline ``+`` (x: 'Plus             , y: 'Plus             ,             _mthd: Default2) = (^Plus :  (static member (<|>) : _*_ -> _) x, y) : ^Plus

    static member inline ``+`` (x: 'Plus             , y: 'Plus             , _mthd: Default1) = x + y : ^Plus
    // static member inline ``+`` (_: ^t when ^t: null and ^t: struct, _: ^t   , _mthd: Default1) = id
    
    static member        ``+`` (x: list<_>           , y                    , _mthd: Plus    ) = x @ y
    static member        ``+`` (x: array<_>          , y                    , _mthd: Plus    ) = Array.append x y
    static member        ``+`` (()                   , ()                   , _mthd: Plus    ) = ()
    static member        ``+`` (x: bool              , y: bool              , _mthd: Plus    ) = x <> y
    static member        ``+`` (x: Set<_>            , y                    , _mthd: Plus    ) = Set.union x y
    
    static member        ``+`` (x: StringBuilder     , y: StringBuilder     , _mthd: Plus    ) = StringBuilder().Append(x).Append(y)
    // static member        ``+`` (_: Id0               , _: Id0               , [<Optional>]_mthd: Plus    ) = Id0 ""    
    // static member        ``+`` (x: AggregateException, y: AggregateException, [<Optional>]_mthd: Plus    ) = Exception.add x y
    // static member        ``+`` (x: exn               , y: exn               , [<Optional>]_mthd: Plus    ) = Exception.add x y :> exn
    
    static member inline Invoke (x: 'Plus) (y: 'Plus) : 'Plus =
        let inline call (mthd : ^M, input1 : ^I, input2 : ^I) = ((^M or ^I) : (static member ``+`` : _*_*_ -> _) input1, input2, mthd)
        call (Unchecked.defaultof<Plus>, x, y)

    
let inline plus<^Monoid when (Plus or ^Monoid) : (static member ``+`` : ^Monoid * ^Monoid * Plus -> ^Monoid)>
    (x: 'Monoid) (y: 'Monoid) : 'Monoid =
        Plus.Invoke x y


type Writer<'l, 'a> = Writer of ('a * 'l)

let run (Writer (a, log)) = (a, log)
    
let inline bind f (Writer (a: 'T, w)) =
    Writer (
        let b, w' = run (f a) in
        (b, plus w w')) : Writer<'Monoid,'U>


type Box = Box of string with
    static member ``+`` (Box b1, Box b2, [<Optional>]_mthd: Plus) =
        Box $"{b1}, {b2}"



[<Fact>]
let ``Box.bind`` () =
   
    let m : Writer<Box, int> = Writer (42, Box "first")

    let f i = Writer (i * 2, Box "second")

    let result = bind f m |> run
    
    test <@ result = (84, Box ("first, second")) @>


[<Fact>]
let ``string list.bind`` () =
   
    let m : Writer<string list, int> = Writer (42, ["first"])

    let f i = Writer (i * 2, ["second"])

    let result = bind f m |> run
    
    test <@ result = (84, ["first"; "second"]) @>
