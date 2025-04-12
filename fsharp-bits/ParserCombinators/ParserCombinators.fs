module FSharpBits.ParserCombinators.ParserCombinators

type String = string
type Char = char
type Unit = unit

type ParseError =
    { Expected: String
      Encountered: String }

type Parser<'a> = Parser of (string -> string * Result<'a, ParseError>)

let runParser (Parser p) input = p input
