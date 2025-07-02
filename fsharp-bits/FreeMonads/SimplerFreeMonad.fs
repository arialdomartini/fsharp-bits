module FSharpBits.FreeMonads.SimplerFreeMonad

// Set of instructions
type Command<'a> =
    | ReadLine of (string -> 'a Command)
    | WriteLine of string * (unit -> 'a Command)
    | Nope of (unit -> 'a Command)
    | Finally of 'a


module ViaConstructors =
    let finallyReturnLength (s: string) = Finally s.Length
    let p1 = ReadLine finallyReturnLength
    let p2 = WriteLine ("Hello, World!", (fun () -> p1))
    let p3 = Nope (fun () -> p2)

module Inline =
    let p3 =
        Nope (fun () ->
            WriteLine ("Hello, World!", (fun () ->
                ReadLine (fun s -> Finally s.Length))))

let rec bind continuation command =
    match command with
    | Finally value -> continuation value
    | ReadLine nextStep -> (ReadLine (nextStep >> bind continuation))
    | WriteLine (s, nextStep) -> WriteLine (s, nextStep >> bind continuation)
    | Nope nextStep -> Nope (nextStep >> bind continuation)

let rec bind' continuation command =
    match command with
    | Finally value -> continuation value
    | ReadLine (nextCommand: string -> Command<'a>) ->
        (ReadLine (fun line ->
            let newCommand = (nextCommand line)
            bind' continuation newCommand))

    | WriteLine (s, next) -> (WriteLine (s, next >> bind' continuation))
    | Nope next -> (Nope (next >> bind' continuation))

let readLine': Command<string> = (ReadLine (fun s -> Finally s))
let readLine: Command<string> = (ReadLine Finally)
let writeLine' s: Command<unit> = (WriteLine (s, fun () -> Finally ()))
let writeLine s = (WriteLine (s, Finally))
let returnValue v = Finally v
let nope = (Nope (fun () -> Finally ()))

let c1': Command<string> = readLine
let c2': Command<unit> = writeLine "Hello, World!"

let (>>=) a b = bind b a

module WithSmartConstructors =
    let finallyReturnLength (s: string) = returnValue s.Length

    let p3': Command<int> =
        (Nope (fun () ->
                  (WriteLine ("Hello, World!", (fun () ->
                                                        (ReadLine (fun s ->
                                                            Finally s.Length)))))))


    let c1 = nope
    let c2 = writeLine "Hello, World!"
    let c3 = readLine
    let c4 (s: string) = returnValue s.Length

    let p' =
        c1
        >>= (fun () -> c2)
        >>= (fun () -> c3)
        >>= (fun s -> c4 s)

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


let rec execute (step: Command<'a>) =
    match step with
    | Finally value -> value
    | ReadLine continuation ->
     let line = System.Console.ReadLine()
     let nextStep = continuation line
     execute nextStep

    | WriteLine(s, continuation) ->
     System.Console.WriteLine(s)
     execute (continuation ())

    | Nope continuation ->
     execute (continuation ())
