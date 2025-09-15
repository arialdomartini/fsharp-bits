module FSharpBits.ForFunAndProfit.Catamorphism.Folds.TotalCostWithAccumulator

open FSharpBits.ForFunAndProfit.Catamorphism.Folds.NestedBoxes
open FSharpBits.ForFunAndProfit.Catamorphism.Folds.SampleValues
open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType


open Xunit
open Swensen.Unquote

let rec totalCost acc (gift: Gift) : decimal =
    match gift with
    | Book book -> acc + book.price
    | Chocolate chocolate -> acc + chocolate.price
    | Wrapped(gift, _) ->
        totalCost (acc + wrappedPrice) gift
    | Boxed gift ->
        totalCost (acc + boxedPrice) gift
    | WithACard(gift, _) ->
        totalCost (acc + withACardPrice) gift


[<Fact(Skip = "This fails in Debug for missing tail call optimization")>]
let ``raises a stackoverflow exception!'`` () =

    let gift = deeplyNestedBox (Book wolfHall) 10000

    let tot = totalCost 0m gift

    test <@ tot = 42m + boxedPrice * 10000m @>


[<Fact>]
let ``calculates cost'`` () =

    let gift = deeplyNestedBox (Book wolfHall) 100

    let tot = totalCost 0m gift

    test <@ tot = 20m + 100m * boxedPrice @>
