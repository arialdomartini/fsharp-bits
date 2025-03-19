module FSharpBits.MonadTransformers.MaybeWriter

open FSharpBits.MonadTransformers.Maybe
open FSharpBits.MonadTransformers.Writer
open Xunit
open Swensen.Unquote

type MaybeWriter<'log, 'a> = MaybeWriter of Writer<'log, Maybe<'a>>

let runMaybeWriter (MaybeWriter m) = m

let mwreturn v : MaybeWriter<'log, 'v> = MaybeWriter (wreturn (mreturn v))
let mwbind' (m: MaybeWriter<'log, 'a>) (f: 'a -> MaybeWriter<'log, 'b>) : MaybeWriter<'log, 'b> =
    MaybeWriter <|
        let wv, wl = runWriter (runMaybeWriter m)
        match wv with
        | None' -> Writer (None', wl)
        | Some' v ->
            let rv, rl = runWriter (runMaybeWriter (f v))
            match rv with
            | None' -> Writer (None' , wl @ rl)
            | Some' v -> Writer (Some' v, wl @ rl)

let mwbind (mo: MaybeWriter<'log, 'a>) (f: 'a -> MaybeWriter<'log, 'b>) : MaybeWriter<'log, 'b> =
    MaybeWriter <|
        let mw = runMaybeWriter mo
        wbind mw (fun m ->
            match m with
            | None' -> wreturn None'
            | Some' a -> runMaybeWriter <| f a )
        

type MaybeWriterBuilder() =
    member this.Return(v) = mwreturn v
    // member this.ReturnFrom(v) = v
    member this.Bind(m, v) = mwbind m v

let maybeWriter = MaybeWriterBuilder ()

let tell' (l: 'log) : MaybeWriter<'log, unit> = MaybeWriter (Writer (Some' (), [l])) 

let mwlift<'l, 'a> (m: Maybe<'a>) : MaybeWriter<'l, 'a> =
    MaybeWriter (wreturn m)

[<Fact>]
let ``Maybe.bind`` () =
    let getBankAccount (client: Client) = client.BankAccount
    
    let divideBy a b : MaybeWriter<string, int> =
        maybeWriter {
            do! tell' "Trying to perform a division"
            let! x = if b = 0 then mwlift None' else mwlift (Some' (a / b))
            return x
        }
        
        
    let client = {Name = "Fabian"; BankAccount = Some' {Number = "123"; Balance = Some' 42}}
    
    let n = 2
    
    let resultmw = maybeWriter {
        do! tell' "Initial log" 
    
        let! bankAccount = mwlift <| getBankAccount client
        let! balance = mwlift <| bankAccount.Balance
        let! r = divideBy balance n
        return r
    }
    
    let result = runWriter <| runMaybeWriter resultmw
    
    test <@ result = (Some' 21, ["Initial log"; "Trying to perform a division"]) @>
