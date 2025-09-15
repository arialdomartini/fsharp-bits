module FSharpBits.ForFunAndProfit.Catamorphism.Folds.Raises

open FSharpBits.ForFunAndProfit.Catamorphism.Cata
open FSharpBits.ForFunAndProfit.Catamorphism.Folds.NestedBoxes
open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType
open SampleValues
open Xunit

[<Fact(Skip = "Throws")>]
let ``raises a stackoverflow exception`` () =
    whatsInside (deeplyNestedBox (Book wolfHall) 10000) |> ignore
