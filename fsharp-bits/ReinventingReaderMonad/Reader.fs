module FSharpBits.ReinventingReaderMonad.Reader

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


type ReaderBuilder() =
    member _.Return(x) = Reader (fun _ -> x)
    member this.Zero =  this.Return ()
    member _.Bind(x, f) = bind f x




let usingEnv env = $"Env is {env}"

let usingEnvReader = Reader usingEnv

[<Fact>]
let ``running a reader monad`` () =
    let result = run "foo" usingEnvReader

    Assert.Equal("Env is foo", result)

[<Fact>]
let ``mapping a reader monad`` () =
    let toUpper (s: string) = s.ToUpper()

    let mapped = map toUpper usingEnvReader
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
     
    let bound = bind toUpper usingEnvReader
     
    Assert.Equal("Env is foo", run "foo" bound) 
    Assert.Equal("ENV IS TO UPPER PLEASE", run "to upper please" bound)

let reader = ReaderBuilder()

let usingEnvWithComputation (s: string) : Reader<string, string>=
    reader {
        let! env = ask 
        return $"Input is {s}, Env is {env}"    
    }
    
[<Fact>]
let ``running, with Computation Expression``() =
    let result = usingEnvWithComputation "bar"
    Assert.Equal("Input is bar, Env is foo", run "foo" result)


type Service = int -> string -> string
let withSideEffects (n: int) (s: string)=
    System.Console.WriteLine s
    s + n.ToString()

let x : Service = withSideEffects

let functionUsingService (service: Service) =
    service 120 "ciao"

let usingService (s: string) =
    fun (service: Service) ->
        let result = functionUsingService service
        
        result + s
    |> Reader
