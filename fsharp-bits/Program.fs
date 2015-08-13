let square x = x * x

type Person = { First:string; Last:string; }

[<EntryPoint>]
let main argv = 
    [1..100] |> List.sum |> printfn "The sum is=%d"
    square 100 |> printfn "The square or 100 is %d"

    let mario = {First="Mario"; Last="Rossi"}
    mario.Last |> printfn "His lasst name is %s"

    let person1 = {First = "Foo"; Last = "Bar"}
    let person2 = {First = "Foo"; Last = "Bar"}

    printfn "is person1 == person2 ? %A" ( person1 = person2 )
    printfn "does person1.Equals(person2) ? %A" ( person1.Equals(person2) )
    
    0