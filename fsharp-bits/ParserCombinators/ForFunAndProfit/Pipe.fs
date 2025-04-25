module FSharpBits.ParserCombinators.ForFunAndProfit.Pipe

open Functor

let (|>> ) x f = mapP f x
