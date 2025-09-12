module FSharpBits.ForFunAndProfit.Catamorphism.FileSystem.CataUsage

open Cata
open FileSystem
open SampleValues

open Xunit
open Swensen.Unquote

let totalSize (item: FileSystemItem) =
    let fFile (file: File) = file.fileSize
    let fDir (_, size, innerSizes) = size + (List.sum innerSizes)

    cataFS fFile fDir item

[<Fact>]
let ``total size`` () =
    test <@ totalSize root = 31 @>
