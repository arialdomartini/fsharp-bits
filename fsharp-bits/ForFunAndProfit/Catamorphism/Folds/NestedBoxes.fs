module FSharpBits.ForFunAndProfit.Catamorphism.Folds.NestedBoxes

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType

let deeplyNestedBox' gift depth =

    let rec loop v =
        function
        | 0 -> v
        | n -> (loop (Boxed v) (n - 1))

    loop gift depth

let deeplyNestedBox gift depth =
    let rec loop depth boxSoFar =
        match depth with
        | 0 -> boxSoFar
        | n -> loop (n-1) (Boxed boxSoFar)
    loop depth gift

open Xunit
open Swensen.Unquote
open SampleValues

[<Fact>]
let ``a book in 10 boxes`` () =
    let book = wolfHall
    test <@ deeplyNestedBox (Book book) 10 =
        Boxed(
            Boxed(
                Boxed(
                    Boxed(
                        Boxed(
                            Boxed(
                                Boxed(
                                    Boxed(
                                        Boxed(
                                            Boxed(
                                                Book book)))))))))) @>

[<Fact>]
let ``a book in 10000 boxes does not throw`` () =
    Assert.NotNull(deeplyNestedBox (Book wolfHall) 10000)

open FSharpBits.ForFunAndProfit.Catamorphism.Cata

[<Fact(Skip = "Throws")>]
let ``raises a stackoverflow exception`` () =
    whatsInside (deeplyNestedBox (Book wolfHall) 10000) |> ignore
