module FSharpBits.ReaderMonad

open FSharpBits.ReinventingReaderMonad
open ApiClient
open Result
open FsUnit.Xunit
open Xunit

type CustomerId = CustomerId of string
type ProductId = ProductId of string
type ProductInfo = { Name : string }




[<Fact>]
let ``api client can save and retrieve values``() =
    let api = ApiClient()
    api.Set "K1" "value 1"
    api.Get<string> "K1" |> should equal (Success "value 1")

    api.Get "K2" |> should equal (Failure ["Key K2 is not found"])


