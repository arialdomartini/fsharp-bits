module FSharpBits.ReaderMonad

open FSharpBits.ReinventingReaderMonad
open ApiClient
open Result
open FsUnit.Xunit
open Xunit

type CustomerId = CustomerId of string
type ProductId = ProductId of string
type ProductInfo = { Name : string }


let map (f : 'a -> 'b) (r : Result<'a>) : Result<'b> =
    match r with
    | Failure f -> Failure f
    | Success v -> Success (f v)

let bind (f : 'a -> Result<'b>) (r : Result<'a>) : Result<'b> =
    match r with
    | Failure f -> Failure f
    | Success v -> f v



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

let executeApiAction action =
    let client = ApiClient()
    let result = action client
    client.Close()
    result

let isFailure r =
    match r with
    | Success _ -> false
    | Failure _ -> true

let onlySuccess r =
    match r with
    | Success s -> Some s
    | Failure _ -> None

let getPurchaseInfo (customerId : CustomerId) : Result<ProductInfo list> =

    let (CustomerId id) = customerId

    let toProductInfo (ids: ProductId list) (client : ApiClient) =
        let infos =
            ids
            |> List.map (fun (ProductId id) -> id)
            |> List.map client.Get<ProductInfo>

        if infos |> Seq.exists isFailure
        then Failure []
        else Success (infos |> List.choose onlySuccess)


    let action customerId (client : ApiClient)  =
        client.Get<ProductId list> customerId
        |> bind (fun ids -> toProductInfo ids client)

    let action = action id

    executeApiAction action



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