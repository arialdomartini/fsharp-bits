module FSharpBits.ForFunAndProfit.Catamorphism.Products.MostUsedVendor

open MutuallyRecursiveType
open Cata

type Vendors = string list

let mostUsedVendor (product: Product): (string * int) option =
    let fBought (boughtProduct: BoughtProduct) : string list =
        match boughtProduct.vendor with
        | None -> [ ]
        | Some vendor -> [vendor]

    let fMade (_: string, _: int, vendors: (Vendors * int) list): Vendors =
        List.map fst vendors |> List.concat

    cataProduct fBought fMade product
    |> List.groupBy id
    |> List.sortByDescending (fun e -> e |> snd |> List.length)
    |> List.map (fun e -> (fst e, snd e |> List.length))
    |> List.tryHead

open Xunit
open Swensen.Unquote
open SampleValues

[<Fact>]
let ``calculate most used vendor`` () =
    test <@ mostUsedVendor label = Some ("ACME", 1) @>
    test <@ mostUsedVendor formulation = None @>
    test <@ mostUsedVendor shampoo = Some ("ACME", 2) @>
    test <@ mostUsedVendor twoPack = Some ("ACME", 2) @>
