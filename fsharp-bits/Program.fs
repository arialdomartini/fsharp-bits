module FSharpBits.PropertyBasedTesting.Program

[<EntryPoint>]
let main (args: string[]) =
    //Tests.runTestsWithCLIArgs [] args allTests
    Expecto.Tests.runTestsInAssemblyWithCLIArgs [] args
