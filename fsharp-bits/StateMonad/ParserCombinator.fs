module FSharpBits.StateMonad.ParserCombinator

open Swensen.Unquote
open Xunit

type State<'s, 'v> = State of ('s -> 's * 'v)

let runState (State f) s = f s
let execState m = snd << (runState m)

//type Parser<'a> = State<string list, 'a>

let sm = State (fun s -> (s+1, s * 10))

[<Fact>]
let ``runState State`` () =
  let v, ns = runState sm 42
  (v, ns) =! (43, 420)

[<Fact>]
let ``execState State`` () =
  let ns = execState sm 42
  ns =! 420
