module FSharpBits.ParserCombinators.ForFunAndProfit.Between

open Xunit
open Swensen.Unquote
open Many
open Monad
open Many1
open Parser
open Lift
open ParseResult
open ThrowingResultsAway
open Pipe
open AndThen
open OrElse

let between<'a>: char Parser -> 'a Parser -> char Parser -> 'a Parser =
    let betweenBuilder (openTag: 's) (symbol: 'a) (closeTag: 's) =
        symbol

    lift3 betweenBuilder

// let between<'s, 'a>: 's Parser -> 's Parser -> 'a Parser -> 'a Parser =
//     fun o c p ->
//         o >>. p .>> c

[<Fact>]
let ``parses a number surrounded by double quotes`` () =
    let parser: uint Parser =
        let doubleQuotes = parseChar '"'
        doubleQuotes >>. parseNat .>> doubleQuotes

    test <@ run parser "\"123\" something else" = Success(123u, " something else") @>


let empty: 'a list Parser = Parser (fun input ->
    Success ([], input))

[<Fact>]
let ``parsing a list with separators`` () =

    let opening: char Parser = parseChar '{'
    let closing: char Parser =(parseChar '}')

    let parseNats: uint list Parser =
         (many (parseNat .>> parseChar '/') .>>. parseNat |>> (fun (p,d) -> List.concat [p; [d]]))
         <|> empty

    let parseNats: uint list Parser =
         let concat (body: 'a list) (last: 'a) = body @ [last]
         let natSlash = parseNat .>> (parseChar '/')
         let nonEmpty = (lift2 concat) (many natSlash) parseNat
         nonEmpty <|> empty

    let parseNats: uint list Parser =
         parseNat .>>. (many ((parseChar '/') >>. parseNat)) |>> (fun (p,l) -> p :: l)

    let parser = between opening parseNats closing

    test <@ run parser "{1/2/3/4} |> print" = Success ([1u;2u;3u;4u], " |> print") @>

[<Fact>]
let ``parsing a list with separators with Computation Expression`` () =

    let surroundedWith opening closing p = parse {
        let! _ = parseChar opening
        let! inner = p
        let! _ = parseChar closing
        return inner
    }

    let slashNat = parse {
        let! _ = parseChar '/'
        let! n = parseNat
        return n
    }

    let numbers = parse {
        let! first = parseNat
        let! others = many slashNat
        return first :: others
    }

    let parser = parse {
        return! (numbers |> surroundedWith '{' '}')
    }

    test <@ run parser "{1/2/3/4} |> print" = Success ([1u;2u;3u;4u], " |> print") @>
