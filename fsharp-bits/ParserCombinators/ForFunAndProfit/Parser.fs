module FSharpBits.ParserCombinators.ForFunAndProfit.Parser

open FSharpBits.ParserCombinators.ForFunAndProfit
open ParseResult

type Parser<'t> = Parser of (string -> ParseResult<'t * string>)

let run (Parser p) input = p input
