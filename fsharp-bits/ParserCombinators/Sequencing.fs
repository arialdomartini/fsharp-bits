module FSharpBits.ParserCombinators.Sequencing

open FSharpBits.ParserCombinators.ParserCombinators


type FieldName = String
let jsonFieldNameParser : FieldName Parser = failwith "Not yet implemented"

let colonParser : Char Parser = failwith "Not yet implemented"

type FieldValue = String
let jsonValueParser : FieldValue Parser = failwith "Not yet implemented"

type JSONField = {Name: String; Value: String}

let json : JSONField Parser =
    Parser (fun input ->
        let rest, result = runParser jsonFieldNameParser input
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
                | Ok value -> rest, Ok {JSONField.Name = fieldName; Value = value } )
