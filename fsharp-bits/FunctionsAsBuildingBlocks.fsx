let add2 x = x + 2
let multBy3 x = x * 3
let square x = x * x

[1..10] |> List.map  add2 |> printfn "%A"
[1..10] |> List.map  multBy3 |> printfn "%A"
[1..10] |> List.map  square |> printfn "%A"

let add2ThenMultBy3 x = x |> add2 |> multBy3

[1..10] |> List.map add2ThenMultBy3 |> printfn "%A"