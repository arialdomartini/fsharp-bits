module FSharpBits.StateMonad.ParserCombinator

open FSharpBits.StateMonad.StateMonadForParserCombinator
open Xunit
open Swensen.Unquote

module Li =
    let (<**>) (fs : ('a -> 'b) list) (vs: 'a list) : 'b list =
        seq {
            for f in fs do
            for v in vs do
            yield f v
        } |> Seq.toList
    let ap = (<**>)

open Li

let state = StateComputation()

type Parser<'a> = State<string, 'a list>
let pur v = State (fun s -> s, [v])

let runParser p v =
    runState p v

let mapParser f p =
    let flist = List.map f
    map flist p
    
let (<**>) (f: Parser<'a -> 'b>) (m: Parser<'a>) : Parser<'b> =
    State (fun (s : string) ->
        let sf, vf = runParser f s
        let sm, vm = runParser m sf
        let vvf = ap vf vm
        (sm, vvf))
    
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
