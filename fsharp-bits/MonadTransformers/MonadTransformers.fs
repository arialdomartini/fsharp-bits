module FSharpBits.MonadTransformers.MonadTransformers

type Async with
    static member map(mapping: 'a -> 'b) : ('a Async -> 'b Async) =
        fun (a: 'a Async) ->
            async {
                let! av = a
                return (mapping av)
            }

type String = string

type TransactionId = TransactionId of String

type CreditCard =
    { Number: String
      Expiry: String
      Cvv: String }

type UserId = UserId of String

type User =
    { Id: UserId
      CreditCard: CreditCard option }

let lookupUser userId: User option Async = failwith "Not yet implemented"
let chargeCard amount card: TransactionId option Async = failwith "Not yet implemented"
// let emailReceipt (transactionId: TransactionId) : TransactionId Async = failwith "Not yet implemented"
let emailReceipt (transactionId: TransactionId) : unit Async = failwith "Not yet implemented"

let chargeUser (amount: int) (userId: UserId) : TransactionId option Async =
    async {
        match! lookupUser userId with
        | None -> return None
        | Some user ->
            match user.CreditCard with
            | None -> return None
            | Some card ->
                match! chargeCard amount card with
                | None -> return None
                | Some transactionId ->
                    do! (emailReceipt transactionId)
                    return Some transactionId
    }

// Async<Option<'a>>
type AsyncOption<'a> = Async<Option<'a>>

// AsyncOption a -> (a -> AsyncOption b) -> AsyncOption b
let bind (a: AsyncOption<'a>) (f: 'a -> AsyncOption<'b>) : AsyncOption<'b> = 
    async {
        match! a with
        | Some a -> return! f a
        | None -> return None
    }

let (>>=) a f = bind a f

let pure' (a: 'a): 'a AsyncOption =
    async {
        return Some a }

let (<!>) f a =  a >>= f

let pureAsync a = async {return a}
let mapAsync f pa = async {
    let! a = pa
    return f a }
    
type AsyncOptionBuilder() =
    member this.Bind(m, f) = (m >>= f)

    member this.Return(v) = pure' v

let asyncOption = AsyncOptionBuilder ()

let hoist option = async {return option}
let hoist' (a: Async<'a>) = async {
    let! aa = a
    let sa = Some aa
    return sa }

let lift (a: Async<'a>) = mapAsync Some a   

let chargeUser' (amount: int) (userId: UserId) : TransactionId option Async =
    asyncOption {
        let! user = lookupUser userId
        let! card = user.CreditCard |> hoist
        let! transactionId = chargeCard amount card
        do! (emailReceipt transactionId) |> lift
        return  transactionId }
