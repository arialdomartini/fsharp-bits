module FSharpBits.ParserCombinators.Monad

open FSharpBits.ParserCombinators.ParserCombinators

// let andThen : 'a Parser -> ('a -> 'b Parser) -> 'b Parser =
let andThen (a: 'a Parser) (f: 'a -> 'b Parser) =
    Parser(fun input ->
        let rest, result = runParser a input

        match result with
        | Error errorValue -> (rest, Error errorValue)
        | Ok av -> runParser (f av) input)

let (>>=) = andThen

let ``return`` a = Parser (fun input -> (input, Ok a))


type ParserBuilder() =
    member this.Return(a) = ``return`` a
    member this.Bind(m, f) = andThen m f

let parser = ParserBuilder()
