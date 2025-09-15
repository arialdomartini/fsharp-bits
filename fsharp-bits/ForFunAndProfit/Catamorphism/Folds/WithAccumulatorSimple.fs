module FSharpBits.ForFunAndProfit.Catamorphism.Folds.WithAccumulatorSimple

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType

type Book = { title: string; price: decimal }

type Gift =
    | Book of Book
    | Wrapped of Gift * string

let deeplyNestedBox gift depth =

    let rec loop v =
        function
        | 0 -> v
        | n -> (loop (Wrapped (v, "type")) (n - 1))

    loop gift depth



open Xunit
open Swensen.Unquote

let rec innerCost costSoFar (gift: Gift): decimal =

    match gift with
    | Book book -> costSoFar + book.price
    | Wrapped(gift, _wrapping) ->
        innerCost (costSoFar + wrappedPrice) gift

[<Fact>]
let ``does not raise a stackoverflow exception!`` () =
    let gift = (deeplyNestedBox (Book { title= "title"; price = 42m }) 10000)

    let totalCost = innerCost 0m gift

    test <@ totalCost = 42m + wrappedPrice * 10000m @>
