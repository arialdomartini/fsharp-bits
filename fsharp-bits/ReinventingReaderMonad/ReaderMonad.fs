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

// Open API connection
// Get product ids purchased by customer id using the API
// For each product id:
//     get the product info for that id using the API
// Close API connection
// Return the list of product infos

let getPurchaseInfo (customerId : CustomerId) : Result<ProductInfo list> =
    use client = ApiClient()
    let (CustomerId id) = customerId
    let productIds = client.Get<ProductId list> id


    let getProductInfos (ids : ProductId list) =
        let productInfos = ResizeArray<ProductInfo>()
        let mutable anyError = false
        for ProductId id in ids do
            let productInfo = client.Get id

            match productInfo with
            | Failure _ ->
                anyError = true
                ()
            | Success info ->
                productInfos.Add info

        if anyError = true
        then Failure ["Some product info was not found"]
        else Success (productInfos.ToArray() |> Array.toList)

    let productInfos =
        match productIds with
        | Success ids -> (getProductInfos ids)
        | Failure _ as failure -> (Failure ["Could not find some id"])

    client.Close()
    productInfos



[<Fact>]
let ``get a list of products bought by a customer``() =

    let productIds = [
        ProductId "first"
        ProductId "second"
        ProductId "third"
    ]

    let product1 = { Name = "first" }
    let product2 = { Name = "second" }
    let product3 = { Name = "third" }

    let apiClient = ApiClient()

    let someCustomer = CustomerId "john doe"

    apiClient.Set "first" product1
    apiClient.Set "second" product2
    apiClient.Set "third" product3

    apiClient.Set "john doe" productIds


    let purchasedProducts = getPurchaseInfo someCustomer

    purchasedProducts |> should equal (Success [product1; product2; product3])