module FSharpBits.ForFunAndProfit.Catamorphism.Folds.NestedBoxes

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType

let deeplyNestedBox depth gift =

    let rec loop v =
        function
        | 0 -> v
        | n -> (loop (Boxed v) (n - 1))

    loop depth gift


open Xunit
open Swensen.Unquote
open SampleValues

[<Fact>]
let ``a book in nested boxes`` () =
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
