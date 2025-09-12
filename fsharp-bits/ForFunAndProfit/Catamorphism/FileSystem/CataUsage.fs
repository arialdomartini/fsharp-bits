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
let ``total size`` () = test <@ totalSize root = 31 @>


type Microsoft.FSharp.Collections.List<'a> with
    static member optionMaxBy(f: 'a -> 'b) xs =
        match xs with
        | [] -> None
        | _ -> Some (List.maxBy f xs)

let largestFile (item: FileSystemItem) =
    let ignoreEmptyDirectories = List.choose id

    let fFile = Some

    let fDir (_: string, _: int, largestFiles: File option list) =
        match largestFiles with
        | [] -> None
        | largestFiles ->
            largestFiles
            |> ignoreEmptyDirectories
            |> List.optionMaxBy _.fileSize

    cataFS fFile fDir item

[<Fact>]
let ``largestFile `` () =

    let empty = Directory { Directory.dirSize =0; name = "empty"; subItems = [] }
    let e = Directory { Directory.dirSize =0; name = "empty"; subItems = [ empty ] }

    test <@ largestFile src = Some { name = "build.bat"; fileSize = 3 } @>
    test <@ largestFile e = None @>


let largestFile' (item: FileSystemItem) =
    let fFile = Some

    let ifNone defaultValue fileOpt =
        match fileOpt with
        | Some size -> size
        | None -> defaultValue

    let fileSize fileOpt =
        fileOpt
        |> Option.map _.fileSize
        |> ifNone 0

    let fDir (_: string, _: int, largestFiles: File option list) =
        match largestFiles with
        | [] -> None
        | largestFiles -> largestFiles |> List.maxBy fileSize

    cataFS fFile fDir item

[<Fact>]
let ``largestFile size'`` () =
    let empty = Directory { Directory.dirSize =0; name = "empty"; subItems = [] }
    let e = Directory { Directory.dirSize =0; name = "empty"; subItems = [ empty ] }
    test <@ largestFile' root = Some { name = "build.bat"; fileSize = 3 } @>
    test <@ largestFile e = None @>
