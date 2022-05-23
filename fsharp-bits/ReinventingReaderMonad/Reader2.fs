module FSharpBits.ReinventingReaderMonad.Reader2

open Xunit


type Reader<'env, 'out> = Reader of action: ('env -> 'out)

let run env (Reader action) = action env

// Creates a Reader tht returns the environment itself
let ask = Reader id


//let map f reader =
//    fun env ->
//        let result = run env reader
//        f result
//    |> Reader

let map f reader = Reader(fun env -> f (run env reader))

//let bind f reader =
//    let newAction env =
//        let x = run env reader
//        run env (f x)
//
//    Reader newAction

let bind f reader =
    fun env ->
        let result = run env reader
        let newReader = f result
        run env newReader
    |> Reader

let usingEnv env = $"Env is {env}"

let reader = Reader usingEnv

[<Fact>]
let ``running a reader monad`` () =
    let result = run "foo" reader

    Assert.Equal("Env is foo", result)

[<Fact>]
let ``mapping a reader monad`` () =
    let toUpper (s: string) = s.ToUpper()

    let mapped = map toUpper reader
    let result = run "foo" mapped

    Assert.Equal("ENV IS FOO", result)


[<Fact>]
let ``binding a reader monad`` () =
    let toUpper (s: string) =
        fun env ->
            if env = "to upper please" then
                s.ToUpper()
            else
                s
        |> Reader
     
    let bound = bind toUpper reader
     
    Assert.Equal("Env is foo", run "foo" bound) 
    Assert.Equal("ENV IS TO UPPER PLEASE", run "to upper please" bound)
