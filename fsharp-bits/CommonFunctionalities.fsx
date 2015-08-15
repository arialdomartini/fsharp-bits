// Calculating the product of all the numbers up to N

let product a b = a * b

let productOfNumbersUpTo n =
    [1..n] |> List.fold product 1

printfn "%d" (productOfNumbersUpTo 10)


// Counting the sum of odd numbers up to N

let sum a b = a + b
let isOdd n = not (n % 2 = 0)

let sumOfNumbersUpTo n =
    [1..n] |> List.filter isOdd |> List.fold sum 0

printfn "%d" (sumOfNumbersUpTo 10)