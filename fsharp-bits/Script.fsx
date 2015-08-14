// simple type inference

let myInt = 5
let myFloaat = 3.14
let myString = "hello"

// lists

let twoToFive = [2;3;4;5]

// cons
let oneToFive = 1 :: twoToFive

let zeroToFive = [0;1] @ twoToFive

printfn "%A and %A are equal" ([0;1] @ twoToFive)  (0 :: 1 :: twoToFive)


let evens list =
    let isEven value = value % 2 = 0
    List.filter isEven list

let numbers = [1;2;3;4;5;6;7;8;9;10]
printfn "Extracting even numbers from %A gets %A" numbers (evens numbers)

let sumOfSquaresToN n =
    [1..n] |> List.map (fun x -> x * x) |> List.sum

let sumOfSquaresToNUsingArgs n =
    List.sum ( List.map (fun x -> x*x) [1..n])

printfn "The sum of the squares of the first 100 numbers is %d" (sumOfSquaresToN(100))
printfn "The sum of the squares of the first 100 numbers is %d" (sumOfSquaresToNUsingArgs(100))

