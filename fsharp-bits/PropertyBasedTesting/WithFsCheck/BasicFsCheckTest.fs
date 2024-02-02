module FSharpBits.PropertyBasedTesting.WithFsCheck.BasicFsCheckTest

open FsCheck
open Expecto

let gen = Gen.oneof [ gen { return true }; gen { return false } ] |> Arb.fromGen

let anyBoolIsEitherTrueOrFalse b = b = true or b = false

[<Tests>]
let treeTests =
    testList
        "True | False"
        [ // test "fail please" { Tests.failtest "OMG I failed!"
          testProperty "true|false is always true\"" (Prop.forAll gen anyBoolIsEitherTrueOrFalse)

          test "is really true|false is always true"
              { Check.QuickThrowOnFailure(Prop.forAll gen anyBoolIsEitherTrueOrFalse) } ]
