module FSharpBits.ReinventingReaderMonad.Result

type Result<'a> = Success of 'a | Failure of string list
