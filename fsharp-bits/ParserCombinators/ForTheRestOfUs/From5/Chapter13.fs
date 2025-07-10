module FSharpBits.ParserCombinators.ForTheRestOfUs.From5.Chapter13

open FSharpBits.ParserCombinators.ForTheRestOfUs.From5.Chapter12.Chapter12
open Xunit
open Swensen.Unquote

open Monad

module Chapter13 =
    let str (s: string) = Parser (fun (input: string) ->
        if input.StartsWith(s)
        then Success(input[s.Length..], s)
        else Failure $"Expected {s}")

    type Node =
        { tag: string
          content: string }

    let alphaChars = [ 'a' .. 'z' ] @ [ 'A' .. 'Z' ]
    let punctuationMarks = [' '; ';'; ','; '.']

    let tagNameP = many1 (anyOf alphaChars) |>> String.Concat

    let openingTagP = tagNameP |> between (str "<")  (str ">")
    let closingTagP tagName = (str tagName) |> between (str "</") (str ">")

    [<Fact>]
    let ``closingTagP expects exactly a specific tag name`` () =
        test <@ run (closingTagP "pun") "</pun>" = Success ("", "pun") @>
        test <@ run (closingTagP "pun") "</xyz>" = Failure ("Expected pun") @>

    // let bind (m: 'a Parser) (f: 'a -> 'b Parser) =
    //     failwith "Not yet implemented"
    let bind m f = Parser (fun s ->
        let resultA = run m s
        match resultA with
        | Failure f -> Failure f
        | Success(rest, a) ->
            let bParser = f a
            run bParser rest)

    let (>>=) = bind
    let return' v = Parser (fun s -> Success (s, v))

    let nodeP =
        openingTagP >>= (fun tagName ->
            contentP >>= (fun content ->
                (closingTagP tagName) >>= (fun _ ->
                    return' { tag = tagName; content = content })))

    [<Fact>]
    let ``parses opening tag`` () =
      let s = "<foo>rest"
      test <@ run openingTagP s = Success ("rest", "foo") @>

    [<Fact>]
    let ``parses content`` () =
      let s = "Broken pencils are pointless</pun>rest"
      test <@ run contentP s = Success ("</pun>rest", "Broken pencils are pointless") @>

    [<Fact>]
    let ``parses closing tag`` () =
      let s = "</foo>rest"
      test <@ run (closingTagP "foo") s = Success ("rest", "foo") @>

    [<Fact>]
    let ``closingTag works in a context-sensitive grammar`` () =
      let s = "<pun>Broken pencils are pointless</pun>rest"

      let expected =
          { tag = "pun"
            content = "Broken pencils are pointless" }

      test <@ run nodeP s = Success ("rest", expected) @>

    [<Fact>]
    let ``not matching closing tags raise a failure`` () =
      let s = "<pun>Broken pencils are pointless</xml>rest"

      test <@ run nodeP s = Failure "Expected pun" @>


    [<Fact>]
    let ``parsing specified number of string`` () =
        let intP = (many1 (anyOf ['0'..'9']))  |>> String.Concat |>> System.Int32.Parse
        let parseNStrings n = (str (String.replicate n "Hey!"))
        let parse = intP >>= parseNStrings

        test <@ run parse "5Hey!Hey!Hey!Hey!Hey!" = Success("", "Hey!Hey!Hey!Hey!Hey!") @>

    let pure' v = Parser (fun s -> Success (s, v))

    [<Fact>]
    let ``a countdown`` () =
        let intP = (many1 (anyOf ['0'..'9']))  |>> String.Concat |>> System.Int32.Parse
        let downToZero n =
            let rec recurse x =
                if x = 0 then pure' $"Countdown from {n}"
                else (str $",{(string (x-1))}") >>= (fun _ -> recurse (x-1))
            recurse n

        let parse = intP >>= downToZero

        test <@ run parse "5,4,3,2,1,0" = Success("", "Countdown from 5") @>


    let x = mapP
