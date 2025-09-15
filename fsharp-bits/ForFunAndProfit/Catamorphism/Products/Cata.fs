module FSharpBits.ForFunAndProfit.Catamorphism.Products.Cata

open MutuallyRecursiveType

let rec cataProduct
    (fBought: BoughtProduct -> 'r)
    (fMade: string * int * ('r * int) list -> 'r) (product: Product) : 'r =

    let recurse = cataProduct fBought fMade

    match product with
    | Bought boughtProduct -> fBought boughtProduct
    | Made madeProduct ->
        fMade
            ( madeProduct.name,
              madeProduct.weight,
              madeProduct.components
              |> List.map (fun c -> (recurse c.product, c.quantity)))
