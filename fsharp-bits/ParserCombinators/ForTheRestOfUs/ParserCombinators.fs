module FSharpBits.ParserCombinators.ForTheRestOfUs.ParserCombinators

type String = string
type Char = char
type Unit = unit

type ParseError =
    { Expected: String
      Encountered: String }

type Parser<'a> = Parser of (string -> string * Result<'a, ParseError>)

let runParser (Parser p) input = p input


open Xunit
open Swensen.Unquote

type MyLanguage =
    | Null
    | Class

let classParser : Parser<MyLanguage> =
    Parser (fun _ -> ("", Ok Class))

[<Fact>]
let ``successful parsing`` () =
    let _, result = runParser classParser "some input"
    test <@ result = Ok Class @>
