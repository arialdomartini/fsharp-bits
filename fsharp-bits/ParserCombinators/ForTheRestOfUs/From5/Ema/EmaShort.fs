module FSharpBits.ParserCombinators.ForTheRestOfUs.From5.Ema.EmaShort

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

let run parser input =
    let (Parser f) = parser
    f input

let returnp a = Parser(fun input -> Success(input, a))

let bind p f =
    Parser(fun input ->
        let result = run p input
        match result with
        | Failure s -> Failure s
        | Success (rest, a) -> run (f a) rest)

let (>>=) = bind
let mapP f m = m >>= (f >> returnp)
let (<!>): ('a -> 'b) -> 'a Parser -> 'b Parser = mapP
let flip f a b = f b a
let (|>>) = flip mapP

let (<*>) mf mx =
    mf >>= fun f -> mx >>= fun x -> returnp (f x)

type ParseBuilder() =
    member this.Bind(m, f) = m >>= f
    member this.Return(v) = returnp v

let parse = ParseBuilder()

let private charListToString (cs: char list): string =
        System.String.Join("", cs)

let parseChar charToMatch =
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

let between o c p =
    parse {
        let! _ = o
        let! something = p
        let! _ = c
        return something
    }

let orElse parser1 parser2  =
    let inner input =
        let parseResult = run parser1 input
        match parseResult with
        | Success _ as parser1Success -> parser1Success
        | Failure _ ->
            let parseResult = run parser2 input
            match parseResult with
            | Failure e -> Failure e
            | Success _ as parser2Success -> parser2Success

    Parser inner

let (<|>) = orElse

let choice parsers =
    List.reduce (<|>) parsers

let anyOf listOfChars =
    listOfChars
    |> List.map parseChar
    |> choice

let lift = mapP

let lift2 f a b = f <!> a <*> b
let lift3 f a b c = f <!> a <*> b <*> c

let many parser = Parser (fun input ->
    let rec zeroOrMore (parser: 'a Parser) (input: string) : string * 'a list =
        let result = run parser input
        match result with
        | Failure _ -> (input, [])
        | Success (rest, r) ->
            let remainingInput, others = (zeroOrMore parser) rest
            (remainingInput, r :: others)

    let rest, result = zeroOrMore parser input
    Success (rest, result))

let many1 p = parse {
    let! first = p
    let! others = many p
    return first :: others
}

let colon = parseChar ':'
let space = parseChar ' '
let newLine = parseChar '\n'
let digit = anyOf [ '0' .. '9' ]

let nat =
    let buildNat = charListToString >> UInt32.Parse

    lift buildNat (many digit)

let rec sequence parsers =
    let cons x xs = x :: xs
    let consP: Parser<'a> -> Parser<'a list> -> Parser<'a list> = lift2 cons

    match parsers with
    | [] -> returnp []
    | p::ps -> consP p (sequence ps)

let theString s =

    s
    |> Seq.map parseChar
    |> Seq.toList
    |> sequence
    |> mapP charListToString


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


let separatedBy separator el =
    parse {
        let! first = el
        let! others = many (separator >>. el)
        return first :: others
    }


let anyChar = anyOf (['a'..'z'] |> List.append ['A'..'Z'])

let parseString =  (many (anyChar <|> space)) |>> charListToString

let any =
    Parser(fun input ->
        if String.IsNullOrEmpty(input) then
            Failure "Expecting a char. No input"
        else
            Success(input[1..], input[0]))


let upTo stop =
    Parser (fun input ->
        let rec inner (input: string) : string * char list =
            match run stop input with
            | Success (rest, _) -> (rest, [])
            | Failure _ ->
                match run any input with
                | Failure _ -> (input, [])
                | Success (rest, r) ->
                    let finalRest, others = inner rest
                    finalRest, r::others
        let rest, chars: string * char list = inner input
        Success (rest, chars |> charListToString))


// Ema

let parseTime: TimeOnly Parser =
    parse {
        let! numbers = nat |> separatedBy colon
        return TimeOnly(int numbers[0], int numbers[1], int numbers[2])
    }

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
    |> List.mapi (fun idx kw -> (fun _ -> idx + 1) <!> (theString kw))

let monthParsers: int Parser list =
    months
    |> List.mapi (fun idx kw -> ((fun _ -> idx + 1) <!> (theString kw)))

let parseDateTime: DateTime Parser =
    parse {
        let! month = choice monthParsers
        let! _ = many1 space
        let! day = nat
        let! _ = space
        let! time = parseTime

        return DateTime(DateTime.Now.Year, month, int day, time.Hour, time.Minute, time.Second)
    }

let parseMessage: string Parser =
    upTo newLine

let parseIP: IPAddress Parser =
    parse {
        let! elements = separatedBy (parseChar '.') nat
        let bytes = elements |> List.toArray |> Array.map byte
        return IPAddress(bytes)
    }

let parseId: uint Parser = between (parseChar '<') (parseChar '>') nat

type LogItem =
    { id: uint
      emittedOn: DateTime
      ip: IPAddress
      message: string }

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
