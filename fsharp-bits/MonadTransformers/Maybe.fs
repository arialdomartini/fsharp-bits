module FSharpBits.MonadTransformers.Maybe

open Xunit
open Swensen.Unquote

type Maybe<'a> =
    | Some' of 'a
    | None'

let mreturn v = Some' v

let mmap f = fun ma ->
    match ma with
    | None' -> None'
    | Some' a -> f a
    
let mbind ma f =
    match ma with
    | None' -> None'
    | Some' a ->
        f a
        
type MaybeBuilder() =
    member this.Return(v) = mreturn v
    member this.Bind(m, f) = mbind m f
    
let maybe = MaybeBuilder ()

type BankAccount = { Number: string; Balance: int Maybe }
type Client = { Name: string; BankAccount: BankAccount Maybe }

[<Fact>]
let ``Maybe.bind`` () =
    let getBankAccount (client: Client) = client.BankAccount
    
    let divideBy a b =
        if b = 0
        then None'
        else Some' (a / b)
        
    let client = {Name = "Fabian"; BankAccount = Some' {Number = "123"; Balance = Some' 42}}
    
    let n = 2
    
    let result = maybe {
        let! bankAccount = getBankAccount client
        let! balance = bankAccount.Balance
        let! r = divideBy balance n
        return r
    }
    
    test <@ result = Some' 21 @>
