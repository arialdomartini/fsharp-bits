module FSharpBits.ParserCombinators.ForFunAndProfit.Monad

open Xunit
open Swensen.Unquote
open Parser
open ParseResult

let returnP v =
    Parser (fun input -> Success (v, input))

let bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser = fun p f ->
    Parser(fun input ->
        let result = run p input
        match result with
        | Failure s -> Failure s
        | Success (a, rest) -> run (f a) rest)

let (>>=) = bind

[<Fact>]
let ``Monad's return`` () =

    let parseX = returnP "X"

    test <@ run parseX "" =  Success ("X", "")@>
    test <@ run parseX "whatever" =  Success ("X", "whatever")@>


type ParserBuilder() =
    member this.Bind(p, f) = p >>= f
    member this.Return(v) = returnP v
    member this.ReturnFrom(v) = v

let parse = ParserBuilder()
