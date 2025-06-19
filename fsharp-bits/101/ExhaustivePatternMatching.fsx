
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


 

type Result<'a, 'b> =
    | Success of 'a
    | Failure of 'b

type FileErrorReason =
    | FileNotFound of string
    | UnauthorizedAccess of string * System.Exception


let performActionOnFile action filePath =
    try
        use streamReader = new System.IO.StreamReader(filePath:string)
        let result = action streamReader
        streamReader.Close()

        Success(result)
    with
        | :? System.IO.FileNotFoundException as ex   -> Failure(FileNotFound filePath)
        | :? System.Security.SecurityException as ex -> Failure(UnauthorizedAccess(filePath, ex))


let middleLayerDo action filePath =
      let fileResult = performActionOnFile action filePath
      fileResult


let topLayerDo action filePath =
    let fileResult = middleLayerDo action filePath
    fileResult

let entryPoint =
    let result: Result<(unit -> string),FileErrorReason> = topLayerDo (fun fr -> fr.ReadToEnd) "foo.txt"
    match result with
    | Success filePath -> "%A Success"
    | Failure reason  -> 
        match reason with
        | FileNotFound file -> "File not found"
        | UnauthorizedAccess (file, ex) -> "Unauthorized exception"
