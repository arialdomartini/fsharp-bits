module FSharpBits.MonadTransformers.Writer

open Xunit
open Swensen.Unquote

type Writer<'log, 'a> = Writer of ('a * 'log list)

let runWriter (Writer w) = w

let wreturn v = Writer (v, [])

let wmap f = fun w ->
    Writer <|
        let wv, wlog = runWriter w
        (f wv, wlog)

let wbind w f =
    Writer <|
        let wv, wlog = runWriter w
        let rv, rlog = runWriter (f wv)
        (rv, wlog @ rlog)

let tell l = Writer ((), [l])
        
type WriterBuilder () =
    member this.Return(v) = wreturn v
    member this.Bind(w, f) = wbind w f
    
let writer = WriterBuilder ()


[<Fact>]
let ``Writer.bind`` () =
    
    let twice n = writer {
        do! tell "running twice"
        return n * 2
    }
    
    let w = writer {
        do! (tell "initial log")
        let! r = twice 42
        return r
    }
    
    let result = runWriter w
    
    test <@ result = (84, ["initial log"; "running twice"]) @>
