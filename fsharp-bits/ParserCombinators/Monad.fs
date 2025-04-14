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

let (.*>) (a: Parser<'a>) (b: Parser<'b>) : Parser<'b> =
    parser {
        let! _ = a
        let! b' = b
        return b'
    }

let (<*.) (a: Parser<'a>) (b: Parser<'b>) : Parser<'a> =
    parser {
        let! a' = a
        let! _ = b
        return a'
    }

// (<$) :: a -> Parser b -> Parser a
let (.%>) a b =
    parser {
        let! _ = b
        return a
    }
