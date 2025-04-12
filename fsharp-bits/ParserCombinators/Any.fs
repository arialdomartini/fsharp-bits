module FSharpBits.ParserCombinators.Any

open FSharpBits.ParserCombinators.ParserCombinators

let private unexpectedEndOfFile =
    { Expected = "a char"
      Encountered = "the end of input" }

let any: Parser<Char> =
    Parser(fun input ->
        match Seq.toList input with
        | [] -> ("", Error unexpectedEndOfFile)
        | head :: tail -> (string tail, Ok(head)))
