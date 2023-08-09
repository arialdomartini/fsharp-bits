module FSharpBits.PropertyBasedTesting.BasicHedgehogTest

open Hedgehog
open Expecto

[<Expecto.Tests>]
let treeTests =
    testList
        "Tree tests"
        [
          testCase "classic rev.rev.list"
          <| fun _ ->
              property {
                  let! xs = Gen.list (Range.linear 0 100) Gen.alpha
                  return xs |> List.rev |> List.rev = xs
              }
              |> Property.checkBool ]
