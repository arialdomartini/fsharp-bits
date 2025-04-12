module FSharpBits.ParserCombinators.EOF

open FSharpBits.ParserCombinators.ParserCombinators

let private extraChars =
    { Expected = "the end of input"
      Encountered = "other chars" }

let eof: Unit Parser =
    Parser(fun input ->
        match Seq.toList input with
        | [] -> ("", Ok ())
        | xs -> (string xs, Error extraChars))
