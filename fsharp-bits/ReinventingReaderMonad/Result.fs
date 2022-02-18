module FSharpBits.ReinventingReaderMonad.Result

type Result<'a> = Success of 'a | Failure of string list


let map (f : 'a -> 'b) (r : Result<'a>) : Result<'b> =
    match r with
    | Failure f -> Failure f
    | Success v -> Success (f v)

let bind (f : 'a -> Result<'b>) (r : Result<'a>) : Result<'b> =
    match r with
    | Failure f -> Failure f
    | Success v -> f v
