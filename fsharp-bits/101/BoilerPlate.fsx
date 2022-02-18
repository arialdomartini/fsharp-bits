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

printfn "List of NameAndSizes: %A" items
printfn "The biggest size in the list of MaxAndSizes is %A" (maxNameAndSize items)


let maxSize list initial =
    let getMax item maxSoFar = if maxSoFar.Size < item.Size then item else maxSoFar
    list |> List.fold getMax initial

printfn "The biggest MaxAndSize is %A" (maxSize (List.tail(items)) (List.head(items)))

printfn "The biggest MaxAndSize is %A" (items |> List.maxBy (fun item -> item.Size))
