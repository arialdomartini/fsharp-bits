// Calculating the product of all the numbers up to N

let product a b = a * b

let productOfNumbersUpTo n =
    [1..n] |> List.fold product 1

printfn "%d" (productOfNumbersUpTo 10)