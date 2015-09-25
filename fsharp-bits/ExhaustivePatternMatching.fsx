let getFileInfo filePath = 
    let fi = System.IO.FileInfo filePath
    if fi.Exists then Some(fi) else None


let goodFilename = "existingFile.txt"
let badFilename = "notExistingFile.txt"


let doSomethingWithOption(fileInfo:System.IO.FileInfo option) =
    match fileInfo with
    | Some(info) -> printfn "%s" info.DirectoryName
    | None       -> printfn "Missing fileinfo"

doSomethingWithOption(getFileInfo "somefile")


let rec movingAverages list =
    match list with
    | [] -> []
    | x :: y :: rest -> [ (x+y)/2.0] :: movingAverages(y::rest)
    | [_] -> []  // this can't be skipped: pattern matching is required to be exhaustive
    