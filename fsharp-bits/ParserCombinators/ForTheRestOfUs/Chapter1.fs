module FSharpBits.ParserCombinators.ForTheRestOfUs.Chapter1

open System
open Xunit
open Swensen.Unquote

type Person =
    { Id: Guid
      Name: string
      Birthday: DateOnly }

let parseGuid: string -> Guid =
    fun input -> failwith "Not yet implemented"

let parseString: string -> string =
    fun input -> failwith "Not yet implemented"

let parseDateOnly: string -> DateOnly =
    fun input -> failwith "Not yet implemented"

let parsePerson: string -> Person = fun input ->
    let recordPart: string = failwith "Not yet implemented"
    let guidPart: string = failwith "Not yet implemented"
    let namePart: string = failwith "Not yet implemented"
    let birthdayPart: string = failwith "Not yet implemented"


    { Id = parseGuid guidPart
      Name = parseString namePart
      Birthday = parseDateOnly birthdayPart }



[<Fact>]
let ``it parses a Person`` () =

    let input = """
inst Person
   - Id <- *b19b8e87-3d39-4994-8568-0157a978b89a*
   - Name <- <<Richard>>
   - Birthday <- date{16/03/1953}
"""

    let expected =
        { Id = Guid.Parse("b19b8e87-3d39-4994-8568-0157a978b89a")
          Name = "Richard"
          Birthday = DateOnly(1953, 03, 16) }

    test <@ parsePerson input = expected @>
