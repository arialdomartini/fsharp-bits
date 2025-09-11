module FSharpBits.ForFunAndProfit.Catamorphism.ManualRecursion

open RecursiveType
open SampleValues

let rec description (gift: Gift) =
    match gift with
    | Book book -> $"Book titled '{book.title}'"
    | Chocolate chocolate -> $"{chocolate.chocolateType} chocolate"
    | Wrapped(innerGift, wrappingPaperStyle) -> $"{description innerGift} wrapped in {wrappingPaperStyle} paper"
    | Boxed innerGift -> $"{description innerGift} in a box"
    | WithACard(innerGift, message) -> $"{description innerGift} with a card saying {message}"


open Xunit
open Swensen.Unquote

[<Fact>]
let ``description of sample values`` () =
    test <@ description chocolate = "SeventyPercent chocolate" @>
    test <@ description book = "Book titled 'Wolf Hall'" @>
    test <@ description gift1 = "Book titled 'Wolf Hall' wrapped in HappyBirthday paper with a card saying Happy Birthday" @>
    test <@ description gift2 = "SeventyPercent chocolate in a box wrapped in HappyHolidays paper" @>
