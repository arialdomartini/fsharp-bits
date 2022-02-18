// Shared functionalities
type Fold = {funct:int-> int -> int ; initialValue:int }

let manipulateFilterAndFold n manipulateFunction filter foldFunction =
    [1..n] 
    |> manipulateFunction
    |> List.filter filter 
    |> List.fold foldFunction.funct foldFunction.initialValue


// Calculating the product of all the numbers up to N
let leaveAsIs list = list
let productFold = { funct= (fun a b -> a * b); initialValue= 1}
let takeAll n = true

let productOfNumbersUpTo n =
    manipulateFilterAndFold n leaveAsIs takeAll productFold

printfn "%d" (productOfNumbersUpTo 10)

let sum a b = a + b

// Calculating the sum of odd numbers up to N
let sumFold = {funct= sum; initialValue=0}
let isOdd n = not (n % 2 = 0)

let sumOfNumbersUpTo n =
    manipulateFilterAndFold n leaveAsIs isOdd sumFold

printfn "%d" (sumOfNumbersUpTo 10)


// The alternating sum of the numbers up to N

let rec withPositionAcc list position =
    match list with
        | [] -> []
        | element :: rest -> (element, position) :: (withPositionAcc rest (position + 1))

let withPosition list= withPositionAcc list 0

printfn "%A" (withPosition [1; 2; 3; 4; 5; 6; 7; 8; 9; 10])

let invertIfOddPosition element position =
    if isOdd position then -element else element

let rec alternateIfOdd listWithPosition=
    match listWithPosition with
    | [] -> []
    | (item, position) :: rest -> (invertIfOddPosition item position) :: (alternateIfOdd rest)

let alternate list =
     withPosition list |> alternateIfOdd

let sumAlternatingUpToNUsing n =
    manipulateFilterAndFold n alternate takeAll sumFold

printfn "%A" (sumAlternatingUpToNUsing 16)


// Old solutions

let sumAlternatingUpToN n =
    [1..n]
    |> alternate
    |> List.filter takeAll
    |> List.fold sum 0

printfn "%A" (sumAlternatingUpToN 16)


let rec sumAlternating list =
    match list with
        | []    ->     0
        | [a]   ->     a 
        | a :: [b] ->    a + b
        | a :: b :: rest -> a - b + sumAlternating rest

let alternatingSumUpToN n =
    [1..n]
    |> sumAlternating

printf "%d" (alternatingSumUpToN 4)
