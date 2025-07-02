module FSharpBits.ParserCombinators.ForTheRestOfUs.From5.Ema.Ema

open System
open System.Net
open Xunit
open Swensen.Unquote

type String = string
type Input = String
type Rest = Input
type ParseError = String

type ParseResult<'a> =
    | Success of Rest * 'a
    | Failure of ParseError

type Parser<'a> = Parser of (string -> 'a ParseResult)

let run (parser: 'a Parser) (input: string) : 'a ParseResult =
    let (Parser f) = parser
    f input

let returnp (a: 'a) : 'a Parser = Parser(fun input -> Success(input, a))

let bind : 'a Parser -> ('a -> 'b Parser) -> 'b Parser = fun p f ->
    Parser(fun input ->
        let result = run p input
        match result with
        | Failure s -> Failure s
        | Success (rest, a) -> run (f a) rest)

let (>>=) = bind

let mapP (f: 'a -> 'b) (m: 'a Parser) : 'b Parser = m >>= (f >> returnp)
let (<!>): ('a -> 'b) -> 'a Parser -> 'b Parser = mapP

let flip f a b = f b a
let (|>>): 'a Parser -> ('a -> 'b) -> 'b Parser = fun a f -> f <!> a

let (<*>) (mf: ('a -> 'b) Parser) (mx: 'a Parser) : 'b Parser =
    mf >>= fun f -> mx >>= fun x -> returnp (f x)

type ParseBuilder() =
    member this.Bind(m, f) = m >>= f
    member this.Return(v) = returnp v

let parse = ParseBuilder()

let parseChar (charToMatch: char) : char Parser =
    Parser(fun input ->
        if String.IsNullOrEmpty(input) then
            Failure "Expecting 'A'. No input"
        else
            let first = input[0]

            if first = charToMatch then
                let remaining = input[1..]
                Success(remaining, first)
            else
                Failure $"Expecting '{charToMatch}'. Got '{first}'")

[<Fact>]
let ``parse a specific char`` () =
    test <@ run (parseChar 'a') "arialdo" =  Success ("rialdo", 'a')@>

let between (o: 'o Parser) (c: 'c Parser) (p: 'p Parser) : 'p Parser =
    parse {
        let! _ = o
        let! something = p
        let! _ = c
        return something
    }

[<Fact>]
let ``parses something between opening and closing tags`` () =
    let opening = parseChar '<'
    let closing = parseChar '>'
    let star = parseChar '*'

    test <@ run (between opening closing star) "<*> something else" = Success (" something else", '*') @>


let orElse (parser1: Parser<'a>) (parser2: Parser<'a>) : Parser<'a>  =
    let inner input =
        let parseResult = run parser1 input
        match parseResult with
        | Success (a, rest) as parser1Success -> parser1Success
        | Failure e ->
            let parseResult = run parser2 input
            match parseResult with
            | Failure e -> Failure e
            | Success (rest, a) as parser2Success -> parser2Success

    Parser inner

let (<|>) = orElse

let choice (parsers: 'a Parser list) : 'a Parser =
    List.reduce (<|>) parsers

let anyOf (listOfChars: char list) : char Parser =
    listOfChars
    |> List.map parseChar
    |> choice

[<Fact>]
let ``testing any`` () =
    test <@ run (anyOf ['a'..'z']) "hello" = Success ("ello", 'h') @>


let lift = mapP

let lift2: ('a -> 'b -> 'c) -> 'a Parser -> 'b Parser -> 'c Parser =
    fun f a b -> f <!> a <*> b

let lift3: ('a -> 'b -> 'c -> 'd) -> 'a Parser -> 'b Parser -> 'c Parser -> 'd Parser =
    fun f a b c -> f <!> a <*> b <*> c

let many<'a> (parser: Parser<'a>) : Parser<'a list> = Parser (fun input ->
    let rec zeroOrMore (parser: 'a Parser) (input: string) : string * 'a list =
        let result = run parser input
        match result with
        | Failure _ -> (input, [])
        | Success (rest, r) ->
            let remainingInput, others = (zeroOrMore parser) rest
            (remainingInput, r :: others)

    let rest, result = zeroOrMore parser input
    Success (rest, result))

[<Fact>]
let ``parses many`` () =
    test <@ run (many (anyOf ['a'..'z'])) "hello world" = Success (" world", ['h';'e';'l';'l';'o';]) @>
    test <@ run (many (anyOf ['a'..'z'])) "" = Success ("", []) @>

let many1 p = parse {
    let! first = p
    let! others = many p
    return first :: others
}

[<Fact>]
let ``parses many1`` () =
    test <@ run (many1 (anyOf ['a'..'z'])) "hello world" = Success (" world", ['h';'e';'l';'l';'o';]) @>
    test <@ run (many1 (anyOf ['a'..'z'])) "" = Failure "Expecting 'A'. No input" @>


let nat: uint Parser =

    let buildNat (cs: char list) : uint =
        System.String.Join("", cs) |> UInt32.Parse

    let parseDigit: char Parser = anyOf [ '0' .. '9' ]

    let l: Parser<char list> -> Parser<uint> = lift buildNat
    lift buildNat (many parseDigit)

[<Fact>]
let ``parses a number`` () =
    test <@ run nat "124 ciao" = Success (" ciao", 124u) @>

//
// let parseId'': int Parser =
//     (fun o v c -> v) <!> (parseChar '<') <*> parseInt <*> (parseChar '>')
//

let rec sequence (parsers: ('a Parser) list) : ('a list) Parser =
    let cons x xs = x :: xs
    let consP: Parser<'a> -> Parser<'a list> -> Parser<'a list> = lift2 cons

    match parsers with
    | [] -> returnp []
    | p::ps -> consP p (sequence ps)

[<Fact>]
let ``parses a sequence of parsers`` () =
    let s = sequence [parseChar 'a';parseChar 'b';parseChar 'c';]

    test <@ run s "abcde" = Success("de", ['a'; 'b'; 'c'])@>

let parseTheString (s: string) : string Parser =
    let charListToString (cs: char list): string =
        System.String.Join("", cs)

    s
    |> Seq.map parseChar
    |> Seq.toList
    |> sequence
    |> mapP charListToString


[<Fact>]
let ``parse a specific string`` () =
    test <@ run (parseTheString "hello") "hello world" = Success(" world", "hello")@>


type Keyword = Keyword


[<Fact>]
let ``parse a specific keyword`` () =

    let hello: string Parser = parseTheString "hello"
    // let parseKeyword: Keyword Parser =  (fun _ -> Keyword) <!> hello
    let parseKeyword: Keyword Parser =  hello |>> (fun _ -> Keyword)
    test <@ run parseKeyword "hello world" = Success(" world", Keyword)@>


let (.>>) p1 p2 =
    parse {
        let! v1 = p1
        let! _ = p2
        return v1 }

let (>>.) p1 p2 =
    parse {
        let! _ = p1
        let! v2 = p2
        return v2 }


let separatedBy<'sep, 'el> (separator: 'sep Parser) (el: 'el Parser) : 'el list Parser =
    // el >>= (fun first ->
    //     many (el .>> separator )
    //     >>= (fun x -> returnp (first :: x)))
    parse {
        let! first = el
        let! others = many (separator >>. el)
        return first :: others
    }

[<Fact>]
let ``test sep`` () =
    test <@ run (nat |> separatedBy (parseChar '/') ) "1/2/3/4" = Success ("", [1u;2u;3u;4u]) @>


let months: string list =
    [ "Jan"
      "Feb"
      "Mar"
      "Apr"
      "May"
      "Jun"
      "Jul"
      "Aug"
      "Sep"
      "Oct"
      "Nov"
      "Dec" ]

let monthParsers': int Parser list =
    months
    |> List.mapi (fun idx kw -> (fun _ -> idx + 1) <!> (parseTheString kw))

let monthParsers: int Parser list =
    months
    |> List.mapi (fun idx kw -> ((fun _ -> idx + 1) <!> (parseTheString kw)))

let monthParser = choice monthParsers


[<Fact>]
let ``parses a month`` () =
    test <@ run monthParser "Oct" = Success ("", 10) @>

let colon = parseChar ':'

let parseTime: TimeOnly Parser =
    parse {
        let! numbers = nat |> separatedBy colon
        return TimeOnly(int numbers[0], int numbers[1], int numbers[2])
    }

[<Fact>]
let ``parses a time`` () =
    test <@ run parseTime "18:12:24 ciao" = Success(" ciao", TimeOnly(18, 12, 24)) @>


let space = parseChar ' '
let newLine = parseChar '\n'

let parseDateTime: DateTime Parser =
    parse {
        let! month = choice monthParsers
        let! _ = many1 space
        let! day = nat
        let! _ = space
        let! time = parseTime

        return DateTime(DateTime.Now.Year, month, int day, time.Hour, time.Minute, time.Second)
    }

[<Fact>]
let ``parses a datetime`` () =
    test <@ run parseDateTime "Oct 3 18:12:24 ciao" = Success(" ciao", DateTime(2025, 10, 3, 18, 12, 24)) @>


let anyChar = anyOf (['a'..'z'] |> List.append ['A'..'Z'])

let private charListToString (cs: char list): string =
        System.String.Join("", cs)

let parseString =  (many (anyChar <|> space)) |>> charListToString

[<Fact>]
let ``parses any string`` () =
    test <@ run parseString "Hello world! Stops here" = Success ("! Stops here", "Hello world") @>

let any: char Parser =
    Parser(fun input ->
        if String.IsNullOrEmpty(input) then
            Failure "Expecting a char. No input"
        else
            Success(input[1..], input[0]))


let upTo (stop: 'stop Parser): string Parser =
    Parser (fun input ->
        let rec inner (input: string) : string * char list =
            match run stop input with
            | Success (rest, _) -> (rest, [])
            | Failure _ ->
                match run any input with
                | Failure f -> (input, [])
                | Success (rest, r) ->
                    let finalRest, others = inner rest
                    finalRest, r::others
        let rest, chars: string * char list = inner input
        Success (rest, chars |> charListToString))



[<Fact>]
let ``parses up to a stop element`` () =
    test <@ run (upTo (parseTheString "Stop")) "12345 Sto 123 Stop here, then the rest" = Success("Stop here, then the rest", "12345 Sto 123 ") @>

let parseMessage: string Parser =
    upTo (parseChar '\n')

let parseIP: IPAddress Parser =
    parse {
        let! elements = separatedBy (parseChar '.') nat
        let bytes = elements |> List.toArray |> Array.map byte
        return IPAddress(bytes)
    }

[<Fact>]
let ``parse IP`` () =
    test <@ run parseIP "127.0.0.1 zo" =  Success (" zo", IPAddress([|127uy; 0uy; 0uy; 1uy|]))@>




type LogItem =
    { id: uint
      emittedOn: DateTime
      ip: IPAddress
      message: string }


let parseId: uint Parser = between (parseChar '<') (parseChar '>') nat

[<Fact>]
let ``parses an id`` () =
    test <@ run parseId "<124> ciao" = Success (" ciao", 124u) @>

let parseLog = parse {
    let! id = parseId
    let! date = parseDateTime
    let! _ = space
    let! ip = parseIP
    let! _ = space
    let! message = parseMessage

    return
        { id = id
          emittedOn = date
          ip = ip
          message = message }
}

[<Fact>]
let ``parses Ema's example`` () =
    let log = "<134>May  6 18:00:19 127.0.0.1 example of a log message\n"

    let logItem = run parseLog log

    let expected =
        { id = 134u
          emittedOn = DateTime(2025, 05, 06, 18, 00, 19)
          ip = IPAddress([| 127uy; 0uy; 0uy; 1uy |])
          message = "example of a log message" }

    test <@ logItem = Success("", expected) @>
