module FSharpBits.ParserCombinators.Monad

open FSharpBits.ParserCombinators.ParserCombinators

// let andThen : 'a Parser -> ('a -> 'b Parser) -> 'b Parser =
let andThen (a: 'a Parser) (f: 'a -> 'b Parser) =
    Parser(fun input ->
        let rest, result = runParser a input

        match result with
        | Error errorValue -> (rest, Error errorValue)
        | Ok av -> runParser (f av) input)
