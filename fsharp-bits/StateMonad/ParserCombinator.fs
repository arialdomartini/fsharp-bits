module FSharpBits.StateMonad.ParserCombinator

open Swensen.Unquote
open Xunit

let twice n = n * 2
let flip f a b = f b a

type State<'s, 'v> = State of ('s -> 's * 'v)

let runState (State f) s = f s
let execState m = snd << (runState m)

let map f m  =
    State(fun s ->
        let ns, v = runState m s
        (ns, f v))

let (<*>) m1 m2 =
    State (fun s ->
        let s1, f = runState m1 s
        let s2, v2 = runState m2 s1
        (s2, f v2))

//type Parser<'a> = State<string list, 'a>

let sm = State(fun s -> (s + 1, s * 10))

[<Fact>]
let ``runState State`` () =
    let v, ns = runState sm 42
    (v, ns) =! (42 + 1, 42 * 10)

[<Fact>]
let ``execState State`` () =
    let ns = execState sm 42
    ns =! 42 * 10

[<Fact>]
let ``State has an instance of functor`` () =
    let v, ns =
        map twice sm
        |> (flip runState) 42
    (v, ns) =! (42 + 1, 42 * 2 * 10)

[<Fact>]
let ``State has an instance of applicative`` () =
    let sf = State (fun s -> s+1, twice)
    let v, ns = runState (sf <*> sm) 42
    (v, ns) =! (42 + 1 + 1, (42 + 1) * 2 * 10)
