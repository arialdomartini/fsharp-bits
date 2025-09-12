module FSharpBits.ForFunAndProfit.Catamorphism.CatamorphismsForMapping

open RecursiveType
open SampleValues
open Cata

type GiftMinusChocolate =
    | Book of Book
    | Apology of string
    | Wrapped of gift: GiftMinusChocolate * wrappingPaperStyle: WrappingPaperStyle

let removeChocolate (gift: Gift) : GiftMinusChocolate =

    let fBook (book: Book) = Book book
    let fChocolate _ = Apology "The apology 'Sorry I ate your chocolate'"
    let fWrapped (gift, wrappingPaperStyle) = Wrapped(gift, wrappingPaperStyle)
    let fBoxed gift = gift
    let fWithACard (gift, _) = gift

    cataGift fBook fChocolate fWrapped fBoxed fWithACard gift


let rec cataGiftMinusChocolate fBook fApology fWrapped (gift: GiftMinusChocolate) =
    let recurse = cataGiftMinusChocolate fBook fApology fWrapped
    match gift with
    | Book book -> fBook book
    | Apology apology -> fApology apology
    | Wrapped(gift, wrappingPaperStyle) -> fWrapped (recurse gift) wrappingPaperStyle

let description' giftMinusChocolate =
    cataGiftMinusChocolate
        (fun (book: Book) -> $"Book titled '{book.title}'")
        id
        (fun gift wrappingPaperStyle -> $"{gift} wrapped in {wrappingPaperStyle} paper")
        giftMinusChocolate


open Xunit
open Swensen.Unquote

[<Fact>]
let ``description of sample values`` () =
    test <@ description' <| removeChocolate chocolate = "The apology 'Sorry I ate your chocolate'" @>
    test <@ description' <| removeChocolate book = "Book titled 'Wolf Hall'" @>
    test <@ description' <| removeChocolate gift1 = "Book titled 'Wolf Hall' wrapped in HappyBirthday paper" @>
    test <@ description' <| removeChocolate gift2 = "The apology 'Sorry I ate your chocolate' wrapped in HappyHolidays paper" @>
