// Calculating the product of all the numbers up to N

type Fold = {funct:int-> int -> int ; initialValue:int }

let filterAndFold n filter foldFunction =
    [1..n] 
    |> List.filter filter 
    |> List.fold foldFunction.funct foldFunction.initialValue

let product a b = a * b
let productFold = { funct=product; initialValue= 1}
let all n = true
let productOfNumbersUpTo n =
    filterAndFold n all productFold

printfn "%d" (productOfNumbersUpTo 10)


// Counting the sum of odd numbers up to N

let sum a b = a + b
let sumFold = {funct=sum; initialValue=0}
let isOdd n = not (n % 2 = 0)

let sumOfNumbersUpTo n =
    filterAndFold n isOdd sumFold

printfn "%d" (sumOfNumbersUpTo 10)
