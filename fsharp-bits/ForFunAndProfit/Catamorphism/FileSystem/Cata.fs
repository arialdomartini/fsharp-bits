module FSharpBits.ForFunAndProfit.Catamorphism.FileSystem.Cata


open FileSystem

let rec cataFS fFile fDirectory (fsItem: FileSystemItem) : 'r =
    let recurse = cataFS fFile fDirectory

    match fsItem with
    | File file -> fFile file
    | Directory directory -> fDirectory (directory.name, directory.dirSize, (List.map recurse directory.subItems))
