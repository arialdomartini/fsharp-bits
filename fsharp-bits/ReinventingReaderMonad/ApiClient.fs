module FSharpBits.ReinventingReaderMonad.ApiClient

open System
open Result

let private tryCast<'a> key (value: obj) =
        match value with
        | :? 'a as a -> Success a
        | _ -> Failure [$"Value at key {key} cannot be converted to {typeof<'a>.Name}"]

type ApiClient() =
    static let mutable data = Map.empty<string, obj>

    member this.Set (key: string) (value: obj) =
        if key = "bad" then
            Failure [ $"Bad key %s{key}"]
        else
            data <- Map.add key value data
            Success ()

    member this.Get<'a> (key: string) =
        match Map.tryFind key data with
        | Some v -> tryCast<'a> key v
        | None -> Failure [$"Key {key} is not found"]

    member this.Open() =
        printfn "[API] Opening"

    member this.Close() =
        printfn "[API] Closing"

    interface IDisposable with
        member this.Dispose() =
            printfn "[API] Disposing"