module FSharpBits.PropertyBasedTesting.WithHedgehog.PlayingWithGenerators

open Hedgehog
open Expecto


let someProperty xs = xs |> List.rev |> List.rev = xs
let someProp x = x + 1 = x

let gen = Gen.int32 (Range.linear 0 100)


let lift f = fun x -> f x |> Property.ofBool

[<Expecto.Tests>]
let treeTests =
    testList
        "Simple Properties while playing with Hedgehog Generators"

        [ test "Square of any number is not negative" {
              let numbers = Gen.int32 (Range.linear 0 100)
              
              let square n = n * n

              let squareIsNotNegative n = (square n >= 0) |> Property.ofBool

              numbers |> Property.forAll squareIsNotNegative |> Property.check
          } ]
