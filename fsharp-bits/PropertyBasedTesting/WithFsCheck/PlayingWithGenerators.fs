module FSharpBits.PropertyBasedTesting.WithFsCheck.PlayingWithGenerators

open Expecto
open FsCheck


[<Tests>]
let treeTests =
    testList
        "Simple Properties while playing with Generators"

        [ test "Square of any number is not negative test" {
              let numbers = Arb.from<int>
              let square n = n * n
              let squareIsNotNegative (n: int) : bool = square n >= 0

              Check.QuickThrowOnFailure(Prop.forAll numbers squareIsNotNegative)
          }

          testProperty
              "Square of any number is not negative"
              (let numbers = Arb.from<int>
               let square n = n * n
               let squareIsNotNegative (n: int) : bool = square n >= 0

               Prop.forAll numbers squareIsNotNegative) ]
