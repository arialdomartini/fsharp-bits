module FSharpBits.ReaderMonad

open FsUnit.Xunit
open Xunit

[<Fact>]
let ``should pass``() =
    true |> should be True
