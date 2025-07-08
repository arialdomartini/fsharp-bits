module FSharpBits.ParserCombinators.ForTheRestOfUs.From5.Chapter12

open Xunit
open Swensen.Unquote

open Monad

module Chapter12 =
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
    let closingTagP = tagNameP |> between (str "</") (str ">")

    let contentP = many (anyOf (alphaChars @ punctuationMarks)) |>> String.Concat

    let buildNode openingTag content _closingTag =
        { tag = openingTag
          content = content }

    let nodeP = buildNode <!> openingTagP <*> contentP <*> closingTagP

    [<Fact>]
    let ``parses an XML node`` () =
      let s = "<pun>Broken pencils are pointless</pun>"

      let expected =
          { tag = "pun"
            content = "Broken pencils are pointless"}

      test <@ run nodeP s = Success ("", expected) @>

    module P2 =

        let reverse (s: string) = new string(s.ToCharArray() |> Array.rev)

        let PemaNgat = reverse <!> tagNameP

        let openingTagP = tagNameP |> between (str "<") (str ">")
        let closingTag =  PemaNgat |> between (str "</") (str ">")

        let nodeP = buildNode <!> openingTagP <*> contentP <*> closingTagP

        [<Fact>]
        let ``parses an XML tag node with semordnilap tags`` () =
          let s = "<hello>ciao ciao</olleh>"

          let expected =
              { tag = "hello"
                content = "ciao ciao"}

          test <@ run nodeP s = Success ("", expected) @>


        [<Theory>]
        [<InlineData("foo")>]
        [<InlineData("barBaz")>]
        [<InlineData("evil")>]
        [<InlineData("live")>]
        let ``possible tag names`` (s: string) =
            test <@ run tagNameP s = Success("", s)@>

        [<Theory>]
        [<InlineData("oof")>]
        [<InlineData("zaBrab")>]
        [<InlineData("live")>]
        [<InlineData("evil")>]
        let ``possible closing tag names`` (s: string) =
            test <@ run PemaNgat s = Success("", reverse s)@>


        [<Fact>]
        let ``a random string can be both an opening and a closing tag name`` () =
            let random = System.Random()

            let randomString =
                [| for _ in 1 .. 10 -> alphaChars.[random.Next(alphaChars.Length)] |]
                |> System.String

            test <@ run PemaNgat randomString = Success("", reverse randomString)@>


        // no  property

        [<Fact>]
        let ``accepts a wrong closing tag`` () =

          let s = "<hello>ciao ciao</picture>"

          let expected =
              { tag = "hello"
                content = "ciao ciao"}

          test <@ run nodeP s = Success ("", expected) @>


        [<Fact>]
        let ``XML node test`` () =
          let s = "<pun>Broken pencils are pointless</picture>"

          let expected =
              { tag = "pun"
                content = "Broken pencils are pointless"}

          test <@ run nodeP s = Success ("", expected) @>
