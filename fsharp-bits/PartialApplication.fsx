let add x y = x + y
let result = add 1 2

let add2 = add 2

printfn "add2 10 should be 12: %d" (add2 10)

let makeAdder n = 
    let addn y = 
        n + y
    addn

let add2_2 = makeAdder 2
printfn "add2_2 10 should be 12: %d" (add2_2 10)

let genericLoggerDecorator before after func input = 
    before input
    let result = func input
    after result
    result

let notDecorated value = 
    printfn "I got the value %A" value
    value + " yeah!"
let logging value = 
    printfn "logging %A" value
let decorated = genericLoggerDecorator logging logging notDecorated

decorated "foo"
