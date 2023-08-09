module FSharpBits.PropertyBasedTesting.BasicHedgehogTest

open Hedgehog
open Expecto

let someProperty xs = xs |> List.rev |> List.rev = xs

[<Expecto.Tests>]
let treeTests =
    testList
        "Tree tests"
        [
          testCase "classic rev.rev.list"
          <| fun _ ->
              property {
                  let! xs = Gen.list (Range.linear 0 100) Gen.alpha
                  return someProperty xs
              }
              |> Property.checkBool ]
