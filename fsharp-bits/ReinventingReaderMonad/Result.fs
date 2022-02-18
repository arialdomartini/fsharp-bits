module FSharpBits.ReinventingReaderMonad.Result

type Result = Success of obj | Failure of string list
