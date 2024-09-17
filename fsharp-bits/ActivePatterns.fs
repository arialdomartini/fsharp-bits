module FSharpBits.ActivePatterns

open Xunit
open Swensen.Unquote
open System

[<Fact>]
let ``active pattern with multiple choices`` () =

    let (|High|Medium|Low|) value =
        if value > 1000 then High
        else if value > 500 then Medium
        else Low

    let (|Blue|Red|) value =
        if value > 1000 then Blue
        else Red

    let v = 600
    let r1 =
        match v with
        | High -> "high"
        | Medium -> "medium"
        | Low -> "low"
        
    let r2 =
        match v with
        | Blue -> "large"
        | _ -> "other"
        
    let r3 =
        match v with
        | Red -> "red3"
        | High -> "high3"
        | Medium -> "medium3"
        | Blue -> "blue3"
        | Low -> "low3"
        
    test <@ (r1, r2, r3) = ("medium", "other", "red3") @>

[<Fact>]
let ``active pattern with 1 choice`` () =
    let (|Palindrome|) s =
        let cleaned = s |> String.filter Char.IsLetterOrDigit
        let reversed = new string (cleaned |> Seq.toArray |> Array.rev)
        Palindrome (cleaned = reversed)
        
    let check s =
        match s with
        | Palindrome true -> "it is"
        | Palindrome false -> "it is not"
        
    let trueCase = check "i topi non avevano nipoti"
    let falseCase = check "i topi"
    
    test <@ trueCase = "it is" @>
    test <@ falseCase = "it is not" @>
