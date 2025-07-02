module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.WithResult.Parser

type ParseError = string
type Input = string
type Rest = string

type Parser'<'a> = Input -> Rest * Result<'a, ParseError>
type Parser<'a> = Input -> Result<Rest * 'a, ParseError>
