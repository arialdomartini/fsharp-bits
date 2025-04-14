module FSharpBits.ParserCombinators.Sequencing

open FSharpBits.ParserCombinators.Monad
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


let json' : JSONField Parser =
    parser {
        let! fieldName = jsonFieldNameParser
        let! _colon = colonParser
        let! value = jsonValueParser
        return {JSONField.Name = fieldName; Value = value }
    }


let openTag : String Parser = failwith "Not yet implemented"
let closedTag : String Parser = failwith "Not yet implemented"
let value : String Parser = failwith "Not yet implemented"


let valueInsideTags = openTag .*> value <*. closedTag
