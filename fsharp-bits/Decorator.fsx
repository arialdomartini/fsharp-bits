let addingCalculator input = input + 1

let loggingCalculator innerCalculator input = 
    printfn "logging calculator received input %d" input
    let result = innerCalculator input
    printfn "And result is %d" result
    result

printfn "Using addingCalculator"
printfn "Result: %d" (addingCalculator 10)

printfn "Using loggingCalculator"
printfn "Result:"
loggingCalculator addingCalculator 10
