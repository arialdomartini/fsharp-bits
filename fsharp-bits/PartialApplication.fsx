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

