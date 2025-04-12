module FSharpBits.ParserCombinators.Sequencing

open FSharpBits.ParserCombinators.ParserCombinators


let jsonStringParser : String Parser = failwith "Not yet implemented"

let colonParser : Char Parser = failwith "Not yet implemented"

let jsonValueParser : String Parser = failwith "Not yet implemented"

type JSONField = {Name: String; Value: String}

let json : JSONField Parser =
    Parser (fun input ->
        let rest, result = runParser jsonStringParser input
        match result with
        | Error errorValue -> (rest, Error errorValue)
        | Ok fieldName ->
            let rest, result = runParser colonParser rest
            match result with
            | Error errorValue -> (rest, Error errorValue)
            | Ok _ ->  // : is ignored
                let rest, result = runParser jsonValueParser rest
                match result with
                | Error errorValue -> (rest, Error errorValue)
                | Ok value -> rest, Ok {JSONField.Name = fieldName; Value = value }
        )
