module FSharpBits.ForFunAndProfit.Catamorphism.Cata

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType


// The relationship between the case constructors and the handlers
// Wrapped of Gift * WrappingPaperStyle
// fWrapped: 'r   -> WrappingPaperStyle -> 'r)

let rec cataGift
    (fBook: Book -> 'r)
    (fChocolate: Chocolate -> 'r)
    (fWrapped: 'r -> WrappingPaperStyle -> 'r)
    (fBoxed: 'r -> 'r)
    (fWithACard: 'r -> string -> 'r)
    (gift: Gift) : 'r =

    let recurse = cataGift fBook fChocolate fWrapped fBoxed fWithACard

    match gift with
    | Book book -> fBook book
    | Chocolate chocolate -> fChocolate chocolate
    | Wrapped(gift, wrappingPaperStyle) -> fWrapped (recurse gift) wrappingPaperStyle
    | Boxed gift -> fBoxed (recurse gift)
    | WithACard(gift, message) -> fWithACard (recurse gift) message

let description (gift: Gift) =
    let fBook (book: Book) = $"Book titled '{book.title}'"
    let fChocolate chocolate = $"{chocolate.chocolateType} chocolate"
    let fWrapped innerDescription wrappingPaperStyle = $"{innerDescription} wrapped in {wrappingPaperStyle} paper"
    let fBoxed innerDescription = $"{innerDescription} in a box"
    let fWithACard innerDescription message = $"{innerDescription} with a card saying {message}"

    cataGift fBook fChocolate fWrapped fBoxed fWithACard gift


let book = Book { title = "Wolf Hall"; price = 20m }

let chocolate =
    Chocolate
        { Chocolate.chocolateType = SeventyPercent
          price = 5m }

let gift1 =
    WithACard(
        gift = Wrapped(
            gift = book,
            wrappingPaperStyle = HappyBirthday),
        message = "Happy Birthday")

let gift2 = Wrapped(Boxed(chocolate), HappyHolidays)


open Xunit
open Swensen.Unquote

[<Fact>]
let ``description of sample values`` () =
    test <@ description chocolate = "SeventyPercent chocolate" @>
    test <@ description book = "Book titled 'Wolf Hall'" @>
    test <@ description gift1 = "Book titled 'Wolf Hall' wrapped in HappyBirthday paper with a card saying Happy Birthday" @>
    test <@ description gift2 = "SeventyPercent chocolate in a box wrapped in HappyHolidays paper" @>

let totalCost (gift: Gift) =

    let fBook (book: Book) = book.price
    let fChocolate chocolate = chocolate.price
    let fWrapped innerCost _ = innerCost + 0.5m
    let fBoxed innerCost = innerCost + 1.0m
    let fWithACard innerCost _ = innerCost + 2.0m

    cataGift fBook fChocolate fWrapped fBoxed fWithACard gift


[<Fact>]
let ``total cost of sample values`` () =
    test <@ totalCost chocolate = 5m @>
    test <@ totalCost book = 20m @>
    test <@ totalCost gift1 = 22.5m @>
    test <@ totalCost gift2 = 6.5m @>


let rec whatsInside (gift: Gift) =

    let fBook _ = "a book"
    let fChocolate _ = "chocolate"
    let fWrapped innerGift _ = innerGift
    let fBoxed innerGift = innerGift
    let fWithACard innerGift _ = innerGift

    cataGift fBook fChocolate fWrapped fBoxed fWithACard gift


[<Fact>]
let ``whats inside the sample values`` () =
    test <@ whatsInside chocolate = "chocolate" @>
    test <@ whatsInside book = "a book" @>
    test <@ whatsInside gift1 = "a book" @>
    test <@ whatsInside gift2 = "chocolate" @>
