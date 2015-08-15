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



let doYouWantIt value =
    match value with
        | "yes"  -> "you said you want it"
        | "no"   -> "oh, you don't want it"
        | "jain" -> "undecided?"
        | _      -> "whatever"  // this is mandatory, or a warning is emitted

printfn "Do you want it? %s" (doYouWantIt "yes")
printfn "Do you want it? %s" (doYouWantIt "no")
printfn "Do you want it? %s" (doYouWantIt "jain")
printfn "Do you want it? %s" (doYouWantIt "foobar")

let optionPatternMatching value =
    match value with
        | Some number -> (sprintf "number is %d" number)
        | None        -> "value is None"
        | _           -> "otherwise"

//printfn "11 => %A" (optionPatternMatching (11)) // <=== this produces a compile error: value must be an option
printfn "Some(11) => %A" (optionPatternMatching (Some(11)))
printfn "None => %A" (optionPatternMatching (None))


type Person = {FirstName:string; SecondName:string}
let john = {FirstName="John"; SecondName="Carioca"}
printfn "%s's second name is %s" john.FirstName john.SecondName

type AnotherType = {FirstName:string; SecondName:string}
let maria:Person = {FirstName="Maria";SecondName="Impagliazzo"}
let mario = {FirstName="Mario";SecondName="Morini"}


printfn "john is of type %s" (john.GetType().Name)
printfn "maria is of type %s" (maria.GetType().Name)
printfn "mario is of type %s" (mario.GetType().Name)


// Playing with type inference

let foo (s:string) = 
    s.Length
 
let bar(x, y) = foo(x) + y
let baz(a, b) = a(b) :: b


let lengths (elements: _ [] []) = 
    elements |> Array.map (fun element -> element.Length)

//let lengths_fails (elements: _ [] []) = 
//    Array.map (fun xs -> xs.Length) elements    // this breaks the type inference

let values = [|[|1;2;3|];[|1;2;3|]|]
let result = lengths values
printfn "%A"  result


// tuples

let twoTuple = 1,2
let threeTuples = "a", 2, true

let _, singleValue, _ = threeTuples
printfn "I'm accessing one element of a tuple %d" singleValue