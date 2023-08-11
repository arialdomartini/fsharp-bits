module FSharpBits.PropertyBasedTesting.WithFsCheck.FizzBuzzTests

open Expecto
open FsCheck

let positiveOnly n = n > 0
let positiveNumbers = Arb.generate<int> |> Gen.where positiveOnly
let multipleOf m n = n % m = 0
let notMultipleOf m = fun n -> not (n |> multipleOf m)

let fizzBuzz n =
    if n % 15 = 0 then "fizzbuzz"
    else if n % 3 = 0 then "fizz"
    else if n % 5 = 0 then "buzz"
    else n.ToString()

[<Tests>]
let fizzBuzzTests =
    testList
        "FizzBuzz tests"
        [ testProperty
              "multiple of 3 return 'fizz'"
              (let ``multiples of 3`` =
                  positiveNumbers
                  |> Gen.map (fun n -> n * 3)
                  |> Gen.where (notMultipleOf 5)
                  |> Arb.fromGen

               Prop.forAll ``multiples of 3`` (fun n -> fizzBuzz n = "fizz"))

          testProperty
              "multiple of 5 return 'buzz'"
              (let ``multiples of 5`` =
                  positiveNumbers
                  |> Gen.map (fun n -> n * 5)
                  |> Gen.where (notMultipleOf 3)
                  |> Arb.fromGen

               Prop.forAll ``multiples of 5`` (fun n -> fizzBuzz n = "buzz"))

          testProperty
              "multiple of both 3 and 5 return 'fizzbuzz'"
              (let ``multiples of both 3 and 5`` =
                  positiveNumbers
                  |> Gen.map (fun n -> n * 3 * 5)
                  |> Arb.fromGen

               Prop.forAll ``multiples of both 3 and 5`` (fun n -> fizzBuzz n = "fizzbuzz"))

          testProperty
              "all the other numbers return themselves'"
              (let rest = Arb.from<int> |> Arb.mapFilter id (fun n -> (n % 3 <> 0) && (n % 5 <> 0))

               Prop.forAll rest (fun n -> fizzBuzz n = n.ToString())) ]
