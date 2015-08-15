let incrementBy1 i = i + 1

let rec visitList list operation =
    match list with
        | [] -> []
        | first::rest -> (first + 1) :: (visitList rest operation)

printfn "result: %A" (visitList [1..10] incrementBy1)