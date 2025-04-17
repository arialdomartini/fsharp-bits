module FSharpBits.ParserCombinators.ForFunAndProfit.ParseResult

type ParseResult<'a> =
    | Success of 'a
    | Failure of string
