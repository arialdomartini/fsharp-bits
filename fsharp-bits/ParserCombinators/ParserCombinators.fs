module FSharpBits.ParserCombinators.ParserCombinators


// newtype Parser a = Parser {
//   runParser :: String -> (String, Either ParseError a)
// }

type String = string
type Char = char
type Unit = unit

type ParseError =
    { Expected: String
      Encountered: String }

type Parser<'a> = Parser of (string -> string * Result<'a, ParseError>)

let runParser (Parser p) input = p input

let private unexpectedEndOfFile =
    { Expected = "a char"
      Encountered = "the end of input" }

let any: Parser<Char> =
    Parser(fun input ->
        match Seq.toList input with
        | [] -> ("", Error unexpectedEndOfFile)
        | head :: tail -> (string tail, Ok(head)))

let private extraChars =
    { Expected = "the end of input"
      Encountered = "other chars" }

let eof: Parser<Unit> =
    Parser(fun input ->
        match Seq.toList input with
        | [] -> ("", Ok ())
        | xs -> (string xs, Error extraChars))
