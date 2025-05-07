module FSharpBits.ParserCombinators.ForTheRestOfUs.First5.WithResult.ParsePerson

open FSharpBits.ParserCombinators.ForTheRestOfUs.First5.WithResult.Parser
open System

type Person =
    { Id: Guid
      Name: string
      Birthday: DateOnly }


let parseRecord: string Parser = failwith "Not yet implemented"
let parseGuid: Guid Parser = failwith "Not yet implemented"
let parseUpToName: string Parser = failwith "Not yet implemented"
let parseString: string Parser = failwith "Not yet implemented"
let parseUpToBirthday: string Parser = failwith "Not yet implemented"
let parseBirthday: DateOnly Parser = failwith "Not yet implemented"
let parseTillTheEnd: string Parser = failwith "Not yet implemented"

let parsePerson: Person Parser =
    fun input ->

        match parseRecord input with
        | Ok(remaining, _) ->
            match parseGuid remaining with
            | Ok(remaining, id) ->
                match parseUpToName remaining with
                | Ok(remaining, _) ->
                    match parseString remaining with
                    | Ok(remaining, name) ->
                        match parseUpToBirthday remaining with
                        | Ok(remaining, _) ->
                            match parseBirthday remaining with
                            | Ok(remaining, birthday) ->
                                match parseTillTheEnd remaining with
                                | Ok(remaining, _) ->
                                    Ok(
                                        remaining,
                                        { Id = id
                                          Name = name
                                          Birthday = birthday }
                                    )
                                | Error err -> Error err
                            | Error err -> Error err
                        | Error err -> Error err
                    | Error err -> Error err
                | Error err -> Error err
            | Error err -> Error err
        | Error err -> Error err
