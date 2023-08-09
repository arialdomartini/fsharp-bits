﻿module FSharpBits.PropertyBasedTesting.BasicHedgehogTest

open Hedgehog
open Expecto

let someProperty xs = xs |> List.rev |> List.rev = xs
let someProp x = x + 1 = x

let gen = Gen.int32 (Range.linear 0 100)

let lift f = fun x -> property.Return (f x)

[<Expecto.Tests>]
let treeTests =
    testList
        "Tree tests"
        [
          testCase "x != x+1"
          <| fun _ ->
              property {
                  let! xs = gen
                  return someProp xs
              }
              |> Property.checkBool
              
          testCase "x != x+1 without computation expression"
          <| fun _ ->
              Property.forAll (lift someProp) gen  
              |> Property.check
         ]
