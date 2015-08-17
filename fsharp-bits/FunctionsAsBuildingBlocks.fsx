let add2 x = x + 2
let multiplicateBy2 x = x * 2
let square x = x * x

[1..10] |> List.map  add2 |> printfn "%A"
[1..10] |> List.map  multiplicateBy2 |> printfn "%A"
[1..10] |> List.map  square |> printfn "%A"
