let square x = x * x

[<EntryPoint>]
let main argv = 
    [1..100] |> List.sum |> printfn "The sum is=%d"
    square 100 |> printfn "The square or 100 is %d"
    0