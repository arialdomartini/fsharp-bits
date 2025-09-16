module FSharpBits.ForFunAndProfit.Catamorphism.Folds.Description

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType
open FSharpBits.ForFunAndProfit.Catamorphism.SampleValues
open Fold

let description (gift: Gift) =
    let fBook acc (book: Book) = $"Book titled '{book.title}'{acc}"
    let fChocolate acc chocolate = $"{chocolate.chocolateType} chocolate{acc}"
    let fWrapped innerDescription wrappingPaperStyle = $"{innerDescription} wrapped in {wrappingPaperStyle} paper"
    let fBoxed innerDescription = $"{innerDescription} in a box"
    let fWithACard innerDescription message = $"{innerDescription} with a card saying {message}"

    foldLeftGift fBook fChocolate fWrapped fBoxed fWithACard "" gift


open Xunit
open Swensen.Unquote

[<Fact>]
let ``description of sample values`` () =
    test <@ description chocolate = "SeventyPercent chocolate" @>
    test <@ description book = "Book titled 'Wolf Hall'" @>
    test <@ description gift1 = "Book titled 'Wolf Hall' with a card saying Happy Birthday wrapped in HappyBirthday paper" @>
    test <@ description gift2 = "SeventyPercent chocolate wrapped in HappyHolidays paper in a box" @>
