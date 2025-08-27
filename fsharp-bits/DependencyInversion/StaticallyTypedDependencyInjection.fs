module FSharpBits.DependencyInversion.StaticallyTypedDependencyInjection

open Xunit
open Swensen.Unquote

#nowarn 3535
type IService =
    static abstract DoWork: string -> int

type Production() =
    interface IService with
        static member DoWork(s) = s.Length

type Test() =
    interface IService with
        static member DoWork(_: string) = 42

let inline client<'T when 'T :> IService> (s: string) =
    (^T : (static member DoWork : string -> int) s)

let inline client'<'T when 'T :> IService and 'T : (static member DoWork: string -> int)> (s: string) =
    (^T : (static member DoWork : string -> int) s)

let inline client''<'T when 'T : (static member DoWork: string -> int)> (s: string) =
    (^T : (static member DoWork : string -> int) s)

[<Fact>]
let ``use production code`` () =
    let length = client<Production>("foo")
    test <@ length = 3 @>

[<Fact>]
let ``use test code`` () =
    let length = client<Test>("foo")
    test <@ length = 42 @>
