module FSharpBits.FreeMonads.FromTheBottomUp

type Product = { Name: string; Price: decimal }
let getOfferApi id : Product list = failwith "Not yet implemented"

type Dependency<'a> =
    | GetOffer of (int * (Product list -> Dependency<'a>))
    | WriteLine of (string * (unit -> Dependency<'a>))
    | JustAValue of 'a

module BottomUp =
    let program =
        let id = 42
        let items = getOfferApi id
        if items |> Seq.length > 10
        then System.Console.WriteLine "Too many, I am sorry"
             0m
        else items |> Seq.sumBy _.Price

    let program' =
        GetOffer (let id = 42
                  id , (fun items ->
            JustAValue
                (if items |> Seq.length > 10
                then WriteLine ("Too many, I am sorry", (fun () -> JustAValue 0m))
                else JustAValue (items |> Seq.sumBy _.Price))))

    let getOffer id = GetOffer (id, fun result -> JustAValue result)
    let writeLine s = WriteLine (s, fun () -> JustAValue ())
    let justAValue v = JustAValue v

    let rec continueWith (dependency: Dependency<'a>) (continuation: 'a -> Dependency<'b>) =
        match dependency with
        | GetOffer (id , next) -> GetOffer (id, fun products -> continueWith (next products) continuation)
        | WriteLine (string, next) -> WriteLine (string, fun () -> continueWith (next ()) continuation)
        | JustAValue value -> continuation value

    let (>>=) m f = continueWith m f

    let program'' =
        justAValue (
            let id = 42
            id) >>= (fun id ->
            (getOffer id) >>= (fun items ->
                    (if items |> Seq.length > 10
                     then ((writeLine "Too many, I am sorry") >>= (fun _ -> JustAValue 0m))
                     else justAValue (items |> Seq.sumBy _.Price))))



    type InjectBuilder() =
        member _.Bind(x, f) = x >>= f
        member _.Return(x) = JustAValue x
        member _.ReturnFrom(x) = x

    let inject = InjectBuilder ()



    let program''' = inject {
        let id = 42
        let! items = getOffer id
        if items |> Seq.length > 10
        then do! writeLine "Too many, I am sorry"
             return 0m
        else
             let sum = (items |> Seq.sumBy _.Price)
             return sum }

    let rec execute (dependency: Dependency<'a>) =
        match dependency with
        | JustAValue value -> value
        | GetOffer (id, continueWith) ->
            let products = getOfferApi id
            execute (continueWith products)
        | WriteLine (s, continueWith) ->
            System.Console.WriteLine s
            execute (continueWith ())
