module FSharpBits.FreeMonads.SimpleFreeMonad

// Set of instructions
type Command<'nextStep> =
    | ReadLine of (string -> 'nextStep)
    | WriteLine of string * (unit -> 'nextStep)
    | Nope of (unit -> 'nextStep)


type Step<'a> =
    | Execute of Command<Step<'a>>    // execute
    | Finally of 'a                   // a value


// Use Pure to define a program just returning a value
// Use Free to define a program containing a Command


module ViaConstructors =
    let finallyReturnLength (s: string) = Finally s.Length

    let c1: Command<Step<int>> = ReadLine finallyReturnLength
    let p1: Step<int> = Execute c1

    let c2: Command<Step<int>> = WriteLine ("Hello, World!", (fun () -> p1))
    let p2: Step<int> = Execute c2

    let c3: Command<Step<int>> = Nope (fun () -> p2)
    let p3: Step<int> = Execute c3

module Inline =
    let p3: Step<int> =
        Execute (Nope (fun () ->
                  (Execute (WriteLine ("Hello, World!", (fun () ->
                                                         Execute (ReadLine (fun s ->
                                                             Finally s.Length))))))))

let rec bind continuation step =
    match step with
    | Finally value -> continuation value
    | Execute command ->
        match command with
        | ReadLine nextStep -> Execute (ReadLine (nextStep >> bind continuation))
        | WriteLine (s, nextStep) -> Execute (WriteLine (s, nextStep >> bind continuation))
        | Nope nextStep -> Execute (Nope (nextStep >> bind continuation))

let rec bind' continuation step =
    match step with
    | Finally value -> continuation value
    | Execute command ->
        match command with
        | ReadLine (nextStep: string -> Step<'a>) ->
            Execute (ReadLine (fun line ->
                let newStep = (nextStep line)
                bind' continuation newStep ))

        | WriteLine (s, next) -> Execute (WriteLine (s, next >> bind' continuation))
        | Nope next -> Execute (Nope (next >> bind' continuation))

let readLine': Step<string> = Execute (ReadLine (fun s -> Finally s))
let readLine: Step<string> = Execute (ReadLine Finally)
let writeLine' s: Step<unit> = Execute (WriteLine (s, fun () -> Finally ()))
let writeLine s = Execute (WriteLine (s, Finally))
let returnValue v = Finally v
let nope = Execute (Nope (fun () -> Finally ()))

let c1': Step<string> = readLine
let c2': Step<unit> = writeLine "Hello, World!"

let (>>=) a b = bind b a

module WithSmartConstructors =
    let finallyReturnLength (s: string) = returnValue s.Length

    let p3': Step<int> =
        Execute (Nope (fun () ->
                  (Execute (WriteLine ("Hello, World!", (fun () ->
                                                         Execute (ReadLine (fun s ->
                                                             Finally s.Length))))))))

    let p =
        nope
        >>= (fun () -> writeLine "Hello, World!")
        >>= (fun () -> readLine)
        >>= (fun s -> returnValue s.Length)



type ConsoleBuilder() =
    member _.Bind(x, f) = bind f x
    member _.Return(x) = Finally x
    member _.ReturnFrom(x) = x

let console = ConsoleBuilder()

module WithComputationExpression =
    let p' = console {
        do! nope
        do! writeLine "Hello, World!"
        let! line = readLine
        let l = returnValue line.Length
        return l
    }

    let p = console {
        do! nope
        do! writeLine "Hello, World!"
        let! line = readLine
        return line.Length
    }


let rec interpreter (step: Step<'a>) =
    match step with
    | Finally value -> value
    | Execute command -> match command with
                         | ReadLine continuation ->
                             let line = System.Console.ReadLine()
                             let nextStep = continuation line
                             interpreter nextStep

                         | WriteLine(s, continuation) ->
                             System.Console.WriteLine(s)
                             interpreter (continuation ())

                         | Nope continuation ->
                             interpreter (continuation ())
