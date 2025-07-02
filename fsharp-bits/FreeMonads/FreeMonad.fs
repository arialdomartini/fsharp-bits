module FSharpBits.ParserCombinators.FreeMonad

open Xunit
open Swensen.Unquote

type User = { Id: string; Name: string }
type Result = string

type ExternalServiceInstruction<'a> =
    | GetBalance of int * (decimal -> 'a)
    | GetUser of string * (User -> 'a)
    | PostData of string * decimal * (Result -> 'a)

type Program<'a> =
    | Pure of 'a
    | Free of ExternalServiceInstruction<Program<'a>>

let rec bind f prog =
    match prog with
    | Pure x -> f x
    | Free instr ->
        match instr with
        | GetBalance (x, next) -> Free (GetBalance (x, next >> bind f))
        | GetUser (id, next) -> Free (GetUser (id, next >> bind f))
        | PostData (name, amount, next) -> Free (PostData (name, amount, next >> bind f))

type ProgramBuilder() =
    member _.Return(x) = Pure x
    member _.Bind(p, f) = bind f p

let program = ProgramBuilder()


let getFund x = Free (GetBalance (x, Pure))
let getUser id = Free (GetUser (id, Pure))
let sendMoney name amount = Free (PostData (name, amount, Pure))

let pureCalculation1 () = 42
let pure_calculation_true () = "It was true!"
let pureCalculation2 () = "It was false!"


let balanceService id =
    let balance = 5000M
    balance

let userService id =
    let user = { Id= id; Name = "Alice" }
    user

let postService name amount =
    $"sent {amount} to {name}"

let rec interpret prog =
    match prog with
    | Pure x -> x
    | Free instr ->
        match instr with
        | GetBalance (id, next) ->
            interpret (next (balanceService id))
        | GetUser (id, next) ->
            interpret (next (userService id))
        | PostData (name, amount, next) ->
            let result = postService name amount
            interpret (next result)


let myProgram: Program<string> =
    program {
        let balanceId = pureCalculation1 ()
        let! fund = getFund balanceId
        if fund > 100M then
            let! user = getUser "abc123"
            let! result = sendMoney user.Name fund
            return $"balance of {user.Name} increased by {fund}. Result: {result}"
        else
            return pureCalculation2 ()
    }

[<Fact>]
let ``pure calculation`` () =

    let result = interpret myProgram

    test <@ result = "balance of Alice increased by 5000. Result: sent 5000 to Alice" @>
