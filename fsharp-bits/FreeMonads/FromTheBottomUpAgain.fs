module FSharpBits.FreeMonads.FromTheBottomUpAgain

type Product = { Name: string; Price: decimal }

module BottomUp =
    let getOfferApi (id: 'a) : Product list = failwith "Not yet implemented"

    let program: decimal =
        let id = 42
        let items = getOfferApi id  // side effect
        if items |> Seq.length > 10
        then System.Console.WriteLine "Too many, I am sorry" // side effect
             0m
        else items |> Seq.sumBy _.Price

// newtype Cont r a = Cont { runCont :: (a -> r) -> r }
type Dependency<'a> =
    | GetOfferApi of (int * (Product list -> Dependency<'a> ))
    | WriteLine of (string * (unit -> Dependency<'a>))  // can I remove unit?
    | JustAValue of 'a

module BottomUpMarkup =
    let program: Dependency<decimal> =
        let id = 42
        GetOfferApi (id, fun items ->   // side effect
            if items |> Seq.length > 10
            then WriteLine ("Too many, I am sorry", fun () ->
                     JustAValue 0m)
                else JustAValue(items |> Seq.sumBy _.Price))


let getOfferApi id = GetOfferApi (id, fun items -> JustAValue items)
let writeLine s = WriteLine (s, fun () -> JustAValue ())


let rec continueWith (dependency: Dependency<'a>) (continuation: 'a -> Dependency<'b>) =
    match dependency with
    | GetOfferApi (id, next) -> GetOfferApi (id, (fun items -> continueWith (next items) continuation))
    | WriteLine (s, next) -> WriteLine (s, fun () -> continueWith (next ()) continuation)
    | JustAValue v -> continuation v

let (>>=) = continueWith


type InjectBuilder() =
    member _.Bind(m, f) = m >>= f
    member _.Return(v) = JustAValue v
    member _.ReturnFrom(m) = m

let inject = InjectBuilder()

module WithDoNotation =

    // let program''' = inject {
    //     let id = 42
    //     let! items = getOfferApi id
    //     if items |> Seq.length > 10
    //     then do! writeLine "Too many, I am sorry"
    //          return 0m
    //     else
    //          let sum = (items |> Seq.sumBy _.Price)
    //          return sum }
    //

    let program: Dependency<decimal> = inject {
        let id = 42
        let! items = getOfferApi id
        if items |> Seq.length > 10
        then do! writeLine "Too many, I am sorry"
             return 0m
        else return items |> Seq.sumBy _.Price }

let rec interpreterProd (dependency: Dependency<'a>) =
    match dependency with
    | JustAValue v -> v
    | GetOfferApi (id, next) ->
        let items = BottomUp.getOfferApi id
        interpreterProd (next items)

    | WriteLine (s, next) ->
        System.Console.WriteLine(s)
        interpreterProd (next ())
