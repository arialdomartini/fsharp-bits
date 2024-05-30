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


let (>>=) m1 fm =
    State (fun s ->
        let s1, v1 = runState m1 s
        let s2, v2 = runState (fm v1) s1
        (s2, v2))


type StateComputation() =
    member this.Return(v) = State (fun s ->(s, v))
    member this.Bind(m, v) = m >>= v

let state = StateComputation()

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

[<Fact>]
let ``State has an instance of monad`` () =
    let f i = State (fun s -> s, s)
    let v, ns = runState (sm >>= f) 42
    (v, ns) =! (42 + 1, (42 + 1))

[<Fact>]
let ``computation expression for State`` () =
    let sm = state { return 42 }
    let f x = state { return x * 2 }
    
    let m = state {
        let! m = sm
        let! r = f m
        return r
    }
    
    runState m 42 =! (42,84)
