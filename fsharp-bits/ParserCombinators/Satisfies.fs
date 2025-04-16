module FSharpBits.ParserCombinators.Satisfies

open FSharpBits.ParserCombinators.Any
open FSharpBits.ParserCombinators.Monad
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
            else (input, Error ({ Expected = error; Encountered = resultValue.ToString() }))
        | Error error -> (input, Error error))

let satisfies' condition errorMessage =
    parser {
        let! resultValue = any
        if condition resultValue
        then return resultValue
        else return! parseError errorMessage (resultValue.ToString())
    }

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

[<Fact>]
let ``does not consume the input in case of failure`` () =
    let condition c = c >= 'a' && c <= 'c'

    let parsesABC = satisfies condition "a character between a and c"

    let input = "d-fails"

    let rest, _ = runParser parsesABC input
    test <@ rest = input @>

[<Fact>]
let ``does not consume the input in case of failure, using Computation Expression`` () =
    let condition c = c >= 'a' && c <= 'c'

    let parsesABC = satisfies' condition "a character between a and c"

    let input = "d-fails"

    let rest, _ = runParser parsesABC input
    test <@ rest = input @>

[<Fact>]
let ``parses a character satisfying a condition, using Computation Expression`` () =
    let condition c = c >= 'a' && c <= 'c'

    let parsesABC = satisfies' condition "a character between a and c"

    let _, result = runParser parsesABC "a-passes"
    test <@ result = Ok 'a' @>

    let _, result = runParser parsesABC "b-passes"
    test <@ result = Ok 'b' @>

    let _, result = runParser parsesABC "c-passes"
    test <@ result = Ok 'c' @>

    let _, result = runParser parsesABC "d-fails"
    test <@ result = Error { Expected = "a character between a and c"; Encountered = "d" } @>
