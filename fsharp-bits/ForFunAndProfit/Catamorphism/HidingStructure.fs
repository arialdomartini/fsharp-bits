module FSharpBits.ForFunAndProfit.Catamorphism.HidingStructure

open RecursiveType
open SampleValues


// old version, created when WithACard was not a thing
let rec cataGift
    (fBook: Book -> 'r)
    (fChocolate: Chocolate -> 'r)
    (fWrapped: 'r -> WrappingPaperStyle -> 'r)
    (fBoxed: 'r -> 'r)
    // (fWithACard: 'r -> string -> 'r)
    (gift: Gift) : 'r =

    let recurse = cataGift fBook fChocolate fWrapped fBoxed

    match gift with
    | Book book -> fBook book
    | Chocolate chocolate -> fChocolate chocolate
    | Wrapped(gift, wrappingPaperStyle) -> fWrapped (recurse gift) wrappingPaperStyle
    | Boxed gift -> fBoxed (recurse gift)
    | WithACard(gift, _) -> recurse gift  // default implementation


let description (gift: Gift) =
    let fBook (book: Book) = $"Book titled '{book.title}'"
    let fChocolate chocolate = $"{chocolate.chocolateType} chocolate"
    let fWrapped innerDescription wrappingPaperStyle = $"{innerDescription} wrapped in {wrappingPaperStyle} paper"
    let fBoxed innerDescription = $"{innerDescription} in a box"

    cataGift fBook fChocolate fWrapped fBoxed gift

open Xunit
open Swensen.Unquote

[<Fact>]
let ``description of sample values`` () =
    test <@ description chocolate = "SeventyPercent chocolate" @>
    test <@ description book = "Book titled 'Wolf Hall'" @>
    test <@ description gift1 = "Book titled 'Wolf Hall' wrapped in HappyBirthday paper" @> // no output about WithACard
    test <@ description gift2 = "SeventyPercent chocolate in a box wrapped in HappyHolidays paper" @>


let rec handleContentOnly
    (fBook: Book -> 'r)
    (fChocolate: Chocolate -> 'r)
    // (fWrapped: 'r -> WrappingPaperStyle -> 'r)
    // (fBoxed: 'r -> 'r)
    // (fWithACard: 'r -> string -> 'r)
    (gift: Gift) : 'r =

    let recurse = handleContentOnly fBook fChocolate

    match gift with
    | Book book -> fBook book
    | Chocolate chocolate -> fChocolate chocolate
    | Wrapped(gift, _) -> recurse gift
    | Boxed gift -> recurse gift
    | WithACard(gift, _) -> recurse gift  // default implementation


let contentOf (gift: Gift) =
    let fBook (book: Book) = $"Book titled '{book.title}'"
    let fChocolate chocolate = $"{chocolate.chocolateType} chocolate"

    handleContentOnly fBook fChocolate gift


[<Fact>]
let ``description of content`` () =
    test <@ contentOf chocolate = "SeventyPercent chocolate" @>
    test <@ contentOf book = "Book titled 'Wolf Hall'" @>
    test <@ contentOf gift1 = "Book titled 'Wolf Hall'" @>
    test <@ contentOf gift2 = "SeventyPercent chocolate" @>
