module FSharpBits.ParserCombinators.Satisfies

open FSharpBits.ParserCombinators.Any
open FSharpBits.ParserCombinators.ParserCombinators
open Xunit
open Swensen.Unquote

let satisfies condition error =
    Parser (fun input ->
        let rest, r = runParser any input
        match r with
        | Ok resultValue ->
            if condition resultValue
            then (rest, Ok resultValue)
            else (rest, Error ({ Expected = error; Encountered = resultValue.ToString() }))
        | Error error -> (rest, Error error))


[<Fact>]
let ``parses a character satisfying a condition`` () =
    let condition c = c >= 'a' && c <= 'c'

    let parsesABC = satisfies condition "a character between a and c"

    let _, result = runParser parsesABC "a-passes"
    test <@ result = Ok 'a' @>

    let _, result = runParser parsesABC "b-passes"
    test <@ result = Ok 'b' @>

    let _, result = runParser parsesABC "c-passes"
    test <@ result = Ok 'c' @>

    let _, result = runParser parsesABC "d-fails"
    test <@ result = Error { Expected = "a character between a and c"; Encountered = "d" } @>
