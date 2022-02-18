let first, second, _ = (1, 2, 3)

printfn "first =%A " first
printfn "second =%A " second



let list = [1; 2; 3; 4]

printfn "%A" (5 :: list)

let e1::e2::rest = [1..10]

printfn "e1=%A e2=%A" e1 e2
printfn "rest=%A" rest


let listMatcher list = 
    match list with
    | [] -> printfn "Lista vuota"
    | [first] -> printfn "un solo elemento = %d" first
    | [first;second] -> printfn "due elementi %d %d" first second
    | first::second::rest -> printfn "altro rest=%A" rest


listMatcher []
listMatcher [1]
listMatcher [1;2]
listMatcher [1;2;3;5;6]


type Address = {Street: string; City: string}
type Customer = {Id: int; Name: string; Address: Address; Gender: string}

let bob = {Id=1; Name="Bob"; Gender = "m"; Address = {Street= "Via Montesecco"; City = "Roma"}}
let carl = {Id=1; Name="Carl"; Gender = "m"; Address = {Street= "Via Montesecco"; City = "Roma"}}
let mary = {Id=1; Name="Mary"; Gender = "f"; Address = {Street= "Via Montesecco"; City = "Roma"}}

let {Name = nome; Address = {City = city}} = bob

printfn "Bob's name is %s, and he lives in %s" nome city



let selectMales persona =
    match persona with
    | {Name = name; Gender = "m"} -> printfn "%s is a male" name
    | _ -> printfn "Not a male"


[bob; carl; mary] |> List.iter selectMales