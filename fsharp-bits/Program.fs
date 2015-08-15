open System.Net
open System

let square x = x * x

type Person = { First:string; Last:string; }

let old =
    [1..100] |> List.sum |> printfn "The sum is=%d"
    square 100 |> printfn "The square or 100 is %d"

    let mario = {First="Mario"; Last="Rossi"}
    mario.Last |> printfn "His lasst name is %s"

    let person1 = {First = "Foo"; Last = "Bar"}
    let person2 = {First = "Foo"; Last = "Bar"}

    printfn "is person1 == person2 ? %A" ( person1 = person2 )
    printfn "does person1.Equals(person2) ? %A" ( person1.Equals(person2) )
    printfn "is mario > person1 ? %A" (mario > person1)


let printpage(reader:IO.StreamReader) url =
    let html = reader.ReadToEnd()
    printfn "%s" html
    //html


let fetchUrl callback url =
    let request = WebRequest.Create(Uri(url))
    use response = request.GetResponse()
    use stream = response.GetResponseStream()
    use reader = new System.IO.StreamReader(stream)
    callback reader url
    0

[<EntryPoint>]
let main argv = 
    let result = fetchUrl printpage "http://www.yahoo.it"
    0
