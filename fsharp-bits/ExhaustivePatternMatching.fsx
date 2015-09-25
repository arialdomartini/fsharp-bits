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
