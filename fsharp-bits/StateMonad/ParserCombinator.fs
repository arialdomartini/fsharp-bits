module FSharpBits.StateMonad.ParserCombinator

open FSharpBits.StateMonad.StateMonadForParserCombinator
open Xunit
open Swensen.Unquote


let state = StateComputation()

type Parser<'a> = State<string, 'a list>
let pur v = State (fun s -> s, [v])

let runParser p v =
    runState p v

let mapParser f p =
    let flist = List.map f
    map flist p

let (<**>) (fs : ('a -> 'b) list) (vs: 'a list) : 'b list =
    seq {
        for f in fs do
        for v in vs do
        yield f v
    } |> Seq.toList
    
let (<**>) (f: Parser<'a -> 'b>) (m: Parser<'a>) : Parser<'b> =
    State (fun (s : string) ->
        let sf, vf = runParser f s
        let sm, vm = runParser m sf
        let vvf = vf <**> vm
        (sm, vvf))
    
let folder acc st =
    let stateToApply = fst acc
    let elementsSoFar = snd acc
    let ns, nr = runParser st stateToApply
    (ns, List.append elementsSoFar nr)
    
let (>>>=) (m: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    State (fun s ->
        let sm, vm = runParser m s
        let parsers: Parser<'b> list = List.map f vm
        let initialState = (sm, [])
        let s, objects = List.fold folder initialState parsers
        s, objects)
    
[<Fact>]
let ``run parser`` () =
    let parser = pur 42
    let s, v = runParser parser "foobar"
    (s, v) =! ("foobar", [42])

[<Fact>]
let ``map parser`` () =
    let parser = State (fun s -> (s + "parsed", [1;2;3]))
    
    let mapped = mapParser twice parser
    let s, v = runParser mapped "original-"
    (s, v) =! ("original-parsed", [2;4;6])

[<Fact>]
let ``applicative parser`` () =
    let f: Parser<int -> int> = State (fun s -> (s + "func-", [twice; (fun i -> i*3) ]))
    let parser: State<string, int list> = State (fun s -> (s + "parsed", [1;2;3]))
    
    let mapped = f <**> parser
    let s, v = runParser mapped "original-"
    (s, v) =! ("original-func-parsed", [2; 4; 6; 3; 6; 9])

// Parser a -> (a -> Parser b) -> Parser b
// m >>= f
[<Fact>]
let ``parser bind`` () =
    let parser: Parser<string> = State (fun s -> (s + "parsed-", ["one";"two";"three"]))
    let f n = State (fun s -> (s + "bound-" + n, [String.length s]))
    let bound = parser >>>= f
    let s, v = runParser bound "original-"
    (s, v) =! ("original-parsed-bound-onebound-twobound-three", [16; 25; 34])) 
