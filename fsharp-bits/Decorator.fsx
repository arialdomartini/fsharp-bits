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

let genericLoggerWrapper (funct:int->int) : int->int =
    let loggingDecoratedFunction(input:int) : int =
        printfn "input is %d" input
        let result = funct input
        printfn "result is %d" result
        result
    loggingDecoratedFunction

let loggingCalculatorGenerated = (genericLoggerWrapper addingCalculator)

printfn "Using loggingCalculatorGenerated"
printfn "%d" (loggingCalculatorGenerated 10)

