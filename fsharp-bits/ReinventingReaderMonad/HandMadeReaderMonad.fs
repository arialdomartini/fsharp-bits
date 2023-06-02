module FSharpBits.ReinventingReaderMonad.HandMadeReaderMonad

open Xunit

type Reader<'env, 'out> = Reader of action: ('env -> 'out)

module Reader =
    let return' v = Reader(fun _ -> v)
    let run env (Reader action) = action env
    let map f reader = Reader(fun env -> f (reader env))

    let bind (f: 'a -> Reader<'env, 'b>) (reader: Reader<'env, 'a>) : Reader<'env, 'b> =
        Reader(fun env ->
            let r: 'a = run env reader
            let r' = f r
            run env r')

    let ask = Reader id


type ILogger =
    abstract Information: string -> unit

let mutable logs = ""

type Logger() =
    interface ILogger with
        member _.Information s = logs <- logs + "/n" + s

let logger: ILogger = Logger()


let sum x y (logger: ILogger) =
    logger.Information("invoked")
    x + y

[<Fact>]
let ``dependency retention`` () =
    let result = sum 2 3 logger
    Assert.Equal(5, result)



let sumCurry x y =
    fun (logger: ILogger) ->
        logger.Information("invoked")
        x + y

[<Fact>]
let ``last parameter`` () =
    let func = sumCurry 2 3
    Assert.Equal(5, func logger)


let sumReader (x: int) (y: int) : Reader<ILogger, int> =
    Reader(fun (logger: ILogger) ->
        logger.Information("invoked")
        x + y)

[<Fact>]
let ``reader monad`` () =
    let reader = sumReader 2 3
    let result = Reader.run logger reader
    Assert.Equal(5, result)

[<Fact>]
let ``reader monad inside a monad`` () =
    Reader(fun (logger: ILogger) ->
        let reader = sumReader 2 3
        let result = Reader.run logger reader
        Assert.Equal(5, result))
    |> Reader.run logger


type ReaderBuilder() =
    member _.Return(v) = Reader.return' v
    member _.Bind(r, f) = Reader.bind f r

let reader = ReaderBuilder()

let sumComputation x y : Reader<ILogger, int> =
    reader {
        let! l = Reader.ask
        l.Information("invoked")
        return x + y
    }

[<Fact>]
let ``with computation expression`` () =
    let reader = sumComputation 2 3
    let result = Reader.run logger reader
    Assert.Equal(5, result)


let len (s: string) =
    reader {
        let! (l: ILogger) = Reader.ask
        l.Information("Calculating length")
        return s.Length
    }

let concat (s: string) (z: string) =
    reader {
        let! (l: ILogger) = Reader.ask
        l.Information("Concatenating strings")
        return s + z
    }

[<Fact>]
let ``function bind`` () =
    let reader = Reader.bind len (concat "foo" "bar")
    let result = Reader.run logger reader
    Assert.Equal(6, result)


let len' (s: string) =
    Reader(fun logger ->
        let (l: ILogger) = logger
        l.Information("Calculating length")
        s.Length)

let concat' (s: string) (z: string) =
    Reader(fun logger ->
        let (l: ILogger) = logger
        l.Information("Concatenating strings")
        s + z)

[<Fact>]
let ``function bind without computation expression`` () =
    let reader = Reader.bind len' (concat' "foo" "bar")
    let result = Reader.run logger reader
    Assert.Equal(6, result)


type IOther =
    abstract Function: int -> int

type Other() =
    interface IOther with
        member _.Function x = x

let other: IOther = Other()

let usesLogger (s: string) =
    reader {
        let! (l: ILogger) = Reader.ask
        return s.Length
    }

let usesOther (i: int) =
    reader {
        let! (o: IOther) = Reader.ask
        return i
    }

// [<Fact>]
// let ``cannot combine incompatible readers``() =
//     let reader = Reader.bind usesOther (usesLogger "foo")
//     let result = Reader.run logger reader
//     Assert.Equal(3, result)

let usesLogger' (s: string) =
    reader {
        let! (l: #ILogger) = Reader.ask
        l.Information("uses logger")
        return s.Length
    }

let usesOther' (i: int) =
    reader {
        let! (o: #IOther) = Reader.ask
        o.Function(42)
        return i
    }

type Both() =
    interface ILogger with
        member _.Information s = logger.Information s

    interface IOther with
        member _.Function i = i

[<Fact>]
let ``trick with inferred inheritance`` () =
    let reader = Reader.bind usesOther' (usesLogger' "foo")
    let result = Reader.run (Both()) reader
    Assert.Equal(3, result)


type ILoggerW =
    abstract Logger: ILogger

type IOtherW =
    abstract Other: IOther

type Both'() =
    interface ILoggerW with
        member _.Logger = logger

    interface IOtherW with
        member _.Other = Other()

type Both'' =
    inherit ILoggerW
    inherit IOtherW

let usesLogger'' (s: string) =
    reader {
        let! (l: #ILoggerW) = Reader.ask
        l.Logger.Information("uses logger")
        return s.Length
    }

let usesOther'' (i: int) =
    reader {
        let! (o: #IOtherW) = Reader.ask
        o.Other.Function(42)
        return i
    }

[<Fact>]
let ``trick with inferred inheritance no proliferation of methods`` () =
    let reader = Reader.bind usesOther'' (usesLogger'' "foo")
    let result = Reader.run (Both'()) reader
    Assert.Equal(3, result)

[<Fact>]
let ``trick with inferred inheritance no proliferation of methods, direct instantiation of env`` () =
    let reader = Reader.bind usesOther'' (usesLogger'' "foo")
    
    let both =
        { new Both''
          interface ILoggerW with
              member _.Logger = logger
          interface IOtherW with
              member _.Other = Other() }

    let result' = Reader.run both reader
    Assert.Equal(3, result')

let usesBothAsATuple x y =
    reader {
        let! (l: ILogger), (o: IOther) = Reader.ask
        l.Information("this is the logger")
        o.Function(42)
        return x + y
    }

[<Fact>]
let ``resolving a tuple``() =
    let reader = usesBothAsATuple 2 3
    let result = Reader.run (logger, other) reader
    Assert.Equal(5, result)


// Still they don't compose


let usingBothAsATuple x y =
    reader {
        let! (l: ILogger), (o: IOther) = Reader.ask
        l.Information("this is the logger")
        o.Function(42)
        return x + y
    }

let usingOne x =
    reader {
        let! (l: ILogger) = Reader.ask
        l.Information("this is the logger")
        return 2 * x
    }
     
// [<Fact>]
// let ``they don't compose``() =
//     let reader = Reader.bind usingOne (usingBothAsATuple 2 3)
//     let result = Reader.run (logger, other) reader
//     Assert.Equal(5, result)

// [<Fact>]
// let ``they don't compose``() =
//     reader {
//         let! sum = usingBothAsATuple 2 3
//         let! double = usingOne sum
//         return double
//     }

type Dependencies = {
    Logger: ILogger
    Other: IOther
}

let mapBoth (dep: Dependencies) = (dep.Logger, dep.Other)
let usingBothWithContraMap (x: int) (y: int): Reader<(ILogger * IOther),int> =
    reader {
        let! (l: ILogger), (o: IOther) = Reader.ask
        l.Information("this is the logger")
        o.Function(42)
        return x + y
    }

let mapOne (dep : Dependencies): ILogger = dep.Logger 
let usingOneWithContraMap (x: int): Reader<ILogger,int> =
    reader {
        let! (l: ILogger) = Reader.ask
        l.Information("this is the logger")
        return 2 * x
    }

let withEnv (map: 'superenv -> 'subenv) (reader: Reader<'subenv, 'out>)  : (Reader<'superenv, 'out>) =
    Reader (fun (superEnv': 'superenv) ->
        let subEnv': 'subenv = map superEnv'
        let out = Reader.run subEnv' reader
        out)

[<Fact>]
let ``combine using contra-map``() =
    let readerBoth' = usingBothWithContraMap 2 3 |> withEnv mapBoth
    
    let readerOne' = fun x -> withEnv mapOne (usingOneWithContraMap x) 
    
    let reader = Reader.bind readerOne' readerBoth'
    let result = Reader.run { Logger = logger; Other = other} reader
    
    Assert.Equal(10, result)


[<Fact>]
let ``combine using contra-map in a reader map``() =
    reader {
        let! sum = (usingBothWithContraMap 2 3) |> withEnv mapBoth
        let! double = (usingOneWithContraMap sum) |> withEnv mapOne 
        
        Assert.Equal(10, double)
        return ()
    } |> Reader.run { Logger = logger; Other = other}
