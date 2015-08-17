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


let add2ThenMultBy3 x = multBy3(add2 x)
add2ThenMultBy3 8 |> printfn "8 2 + 3 * ->%A"