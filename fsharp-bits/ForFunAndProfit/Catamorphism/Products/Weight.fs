module FSharpBits.ForFunAndProfit.Catamorphism.Products.Weight

open FSharpBits.ForFunAndProfit.Catamorphism.Products.SampleValues
open MutuallyRecursiveType
open Cata

let weight product =
    let fBought (boughtProduct: BoughtProduct) = boughtProduct.weight
    
    let fMade (_, weight, otherWeights) =
        weight + (otherWeights |> List.map (fun (a, b) -> a * b) |> List.sum)

    cataProduct fBought fMade product
    
open Xunit
open Swensen.Unquote

[<Fact>]
let ``calculate weight`` () =
    test <@ weight shampoo = 17 @>
    test <@ weight twoPack = 39 @>
