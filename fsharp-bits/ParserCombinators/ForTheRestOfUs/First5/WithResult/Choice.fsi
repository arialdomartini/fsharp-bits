module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.Chapter3.WithResult.Choice

open FSharpBits.ParserCombinators.ForTheRestOfUs.First5.WithResult.Parser

val choice<'a>: 'a Parser list -> 'a Parser
