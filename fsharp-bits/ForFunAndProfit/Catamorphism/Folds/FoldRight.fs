module FSharpBits.ForFunAndProfit.Catamorphism.Folds.FoldRight

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType
open FSharpBits.ForFunAndProfit.Catamorphism.SampleValues


let rec foldRight
    fBook
    fChocolate
    fWrapped
    fBoxed
    fWithACard
    (gift: Gift)
    generator:
    'r =

    let recurse = foldRight fBook fChocolate fWrapped fBoxed fWithACard

    match gift with
    | Book book ->
        generator (fBook book)
    | Chocolate chocolate ->
        generator (fChocolate chocolate)
    | Wrapped(innerGift, wrappingPaperStyle) ->
        let newGenerator innerVal =
            let newInnerVal = fWrapped wrappingPaperStyle innerVal
            generator newInnerVal
        recurse innerGift newGenerator
    | Boxed innerGift ->
        let newGenerator innerVal =
            let newInnerVal = fBoxed innerVal
            generator newInnerVal
        recurse innerGift newGenerator
    | WithACard(innerGift, message) ->
        let newGenerator innerVal =
            let newInnerVal = fWithACard message innerVal
            generator newInnerVal
        recurse innerGift newGenerator

let description (gift: Gift) =
    let fBook (book: Book) = $"Book titled '{book.title}'"
    let fChocolate chocolate = $"{chocolate.chocolateType} chocolate"
    let fWrapped wrappingPaperStyle acc = $"{acc} wrapped in {wrappingPaperStyle} paper"
    let fBoxed acc = $"{acc} in a box"
    let fWithACard message acc = $"{acc} with a card saying {message}"

    foldRight fBook fChocolate fWrapped fBoxed fWithACard gift id


open Xunit
open Swensen.Unquote

[<Fact>]
let ``description of sample values`` () =
    test <@ description chocolate = "SeventyPercent chocolate" @>
    test <@ description book = "Book titled 'Wolf Hall'" @>
    test <@ description gift1 = "Book titled 'Wolf Hall' wrapped in HappyBirthday paper with a card saying Happy Birthday" @>
    test <@ description gift2 = "SeventyPercent chocolate in a box wrapped in HappyHolidays paper" @>
