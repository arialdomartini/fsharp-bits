module FSharpBits.FreeMonads.PiccioEGian

type Product = { Name: string; Price: decimal }

module BottomUp =
    let getOfferApi' (id: int) : Product list = failwith "Not yet implemented"

    // Free Monad -> Interpreter Pattern
    // dependency rejection

    let program: decimal =
        let id = 42
        let items = getOfferApi' id // side effect

        if items |> Seq.length > 10
        then System.Console.WriteLine "Too many, I am sorry" // side effect
             0m
        else items |> Seq.sumBy _.Price

module Rappresentazione =
    let getOfferApi' (id: int) : Product list = failwith "Not yet implemented"

    type Dependency<'a> =
    | GetOfferApi of (int * (Product list -> Dependency<'a> ))
    | WriteLine of (string * (unit -> Dependency<'a>))
    | JustAValue of 'a

    let program: Dependency<decimal> =
        let id = 42
        GetOfferApi (id, fun items ->   // side effect
            if items |> Seq.length > 10
            then WriteLine ("Too many, I am sorry", fun () ->
                     JustAValue 0m)
                else JustAValue(items |> Seq.sumBy _.Price))



module SmartConstructor =
    let getOfferApi' (id: int) : Product list = failwith "Not yet implemented"

    type Dependency<'a> =
    | GetOfferApi of (int * (Product list -> Dependency<'a> ))
    | WriteLine of (string * (unit -> Dependency<'a>))
    | JustAValue of 'a

    let getOfferApi id = GetOfferApi (id, fun productList -> JustAValue productList)
    let writeLine s = WriteLine (s, fun () -> JustAValue () )

    let rec bind (dependency: Dependency<'a>) (continuation: 'a -> Dependency<'b>) =
        match dependency with
        | GetOfferApi (id, next) -> GetOfferApi (id, (fun items -> bind (next items) continuation))
        | WriteLine (s, next) -> WriteLine (s, fun () -> bind (next ()) continuation)
        | JustAValue v -> continuation v

    let (>>=) = bind

    type InjectBuilder() =
        member _.Bind(m, f) = m >>= f
        member _.Return(v) = JustAValue v
        member _.ReturnFrom(m) = m

    let inject = InjectBuilder()


    let program': Dependency<decimal> =
        let id = 42
        (getOfferApi id) >>= (fun items ->   // side effect
            if items |> Seq.length > 10
            then ((writeLine "Too many, I am sorry") >>= (fun () ->
                     JustAValue 0m))
                else JustAValue(items |> Seq.sumBy _.Price))

    let program: Dependency<decimal> = inject {
        let id = 42
        let! items = getOfferApi id
        if items |> Seq.length > 10
        then do! writeLine "Too many, I am sorry"
             return 0m
         else return (items |> Seq.sumBy _.Price)}


    let program'': decimal =
        let id = 42
        let items = getOfferApi' id // side effect

        if items |> Seq.length > 10
        then System.Console.WriteLine "Too many, I am sorry" // side effect
             0m
        else items |> Seq.sumBy _.Price
