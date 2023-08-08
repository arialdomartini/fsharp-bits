module FSharpBits.PropertyBasedTesting.Program

open Expecto
open FSharpBits.PropertyBasedTesting.BasicHedgehogTest

let allTests = Tests.testList "All tests" [
    treeTests
]

[<EntryPoint>]
let main (args: string[]) =
    //Tests.runTestsWithCLIArgs [] args allTests
    Expecto.Tests.runTestsInAssemblyWithCLIArgs [] args
