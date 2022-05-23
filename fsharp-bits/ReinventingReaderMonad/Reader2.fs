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

  
[<Fact>]
let ``running a reader monad``() =
    let usingEnv env = $"Env is {env}"
    
    let result = run "foo" (Reader usingEnv)

    Assert.Equal("Env is foo", result)
