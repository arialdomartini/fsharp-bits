type NameAndSize = {Name:string; Size:int}

let items = [
    {Name="foo1"; Size=100};
    {Name="foo2"; Size=100};
    {Name="max"; Size=200};
    {Name="foo3"; Size=150};
    {Name="foo4"; Size=10};
    {Name="foo5"; Size=90};
]


let maxNameAndSize list =
    let rec maxNameAndSizeAcc list acc =
        match list with
        | [] -> acc
        | cad :: cdr -> maxNameAndSizeAcc cdr (max cad.Size acc)
            

    maxNameAndSizeAcc list 0

printfn "Lisst of NameAndSizes: %A" items
printfn "The biggest MaxAndSize is %A" (maxNameAndSize items)