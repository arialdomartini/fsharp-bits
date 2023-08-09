module FSharpBits.PropertyBasedTesting.BasicFsCheckTest

open FsCheck
open Expecto

let gen = Gen.oneof [ gen { return true }; gen { return false } ] |> Arb.fromGen

let anyBoolIsAlwaysTrue b = b = true


[<Tests>]
let treeTests =
    testList
        "True | False"
        [ test "fail please" { Tests.failtest "OMG I failed!" }
          testProperty "true|false is always true" (Prop.forAll gen anyBoolIsAlwaysTrue)

          test "is really true|false is always true"
              { Check.QuickThrowOnFailure(Prop.forAll gen anyBoolIsAlwaysTrue) } ]
