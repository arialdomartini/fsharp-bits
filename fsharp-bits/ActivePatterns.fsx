let (|Int|_|) str = 
    match System.Int32.TryParse str with
    | (true, int) -> Some(int)
    | _ -> None
 
let (|Bool|_|) str =
    match System.Boolean.TryParse str with
    | (true, bool) -> Some(bool)
    | _ -> None

 
let validBool = "true"
let invalid = "foobar"
let validInteger = "180"

let testParse str =
    match str with
    | Bool i -> printfn "%A can be parsed as a boolean" str
    | Int i  -> printfn "%A can be parsed a an integer" str
    | _      -> printfn "%A is neither a boolean nor an integer" str

testParse validBool
testParse invalid
testParse validInteger