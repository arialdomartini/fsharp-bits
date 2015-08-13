let square x = x * x

type Person = { First:string; Last:string; }

[<EntryPoint>]
let main argv = 
    [1..100] |> List.sum |> printfn "The sum is=%d"
    square 100 |> printfn "The square or 100 is %d"

    let mario = {First="Mario"; Last="Rossi"}
    mario.Last |> printfn "His lasst name is %s"
    0