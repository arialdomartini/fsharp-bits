open System.Text.RegularExpressions

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


let (|FirstRegexGroup|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some m.Groups.[1].Value else None

let testRegex str = 
    match str with
    | FirstRegexGroup "http://(.*?)/(.*)" host -> 
           printfn "The value is a url and the host is %s" host
    | FirstRegexGroup ".*?@(.*)" host -> 
           printfn "The value is an email and the host is %s" host
    | _ -> printfn "The value '%s' is something else" str
   
// test
testRegex "http://google.com/test"
testRegex "alice@hotmail.com"
