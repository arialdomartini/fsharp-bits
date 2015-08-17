let makeAdd n = 
    let add x = x + n
    add

let makeMult n =
    let mult x = x * n
    mult

let add2 = makeAdd 2
let multBy3 = makeMult 3
let square x = x * x

[1..10] |> List.map  add2 |> printfn "%A"
[1..10] |> List.map  multBy3 |> printfn "%A"
[1..10] |> List.map  square |> printfn "%A"


let add2ThenMultBy3 = add2 >> multBy3
add2ThenMultBy3 8 |> printfn "8 2 + 3 * ->%A"


let makeLog (funct:Printf.TextWriterFormat<'T> -> 'T) =
    let log message value =
        funct "%s%i; " message value
        value
    log

let logMsg = makeLog printf
let logMsgn = makeLog printfn

let a = (logMsgn "ciao" 12)
printfn "a=%A" a


let mult3ThenSquareLogged =
    logMsg "Input value = "
    >> multBy3
    >> logMsg "Multiplied by 3 ="
    >> square
    >> logMsgn "After the addiction ="

printfn "2 * 3 squared = %d" (mult3ThenSquareLogged 2)