module FSharpBits.ForFunAndProfit.Catamorphism.Folds.QuasiFoldRight

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType
open FSharpBits.ForFunAndProfit.Catamorphism.SampleValues

let rec quasiFoldRight
    (fBook: Book -> 'a -> 'r)
    (fChocolate: Chocolate -> 'a -> 'r)
    (fWrapped: WrappingPaperStyle -> 'a -> 'a)
    (fBoxed: 'a -> 'a)
    (fWithACard: string -> 'a -> 'a)
    (gift: Gift)
    (acc: 'a):
    'r =

    let recurse = quasiFoldRight fBook fChocolate fWrapped fBoxed fWithACard

    match gift with
    | Book book ->
        fBook book acc
    | Chocolate chocolate ->
        fChocolate chocolate acc
    | Wrapped(gift, wrappingPaperStyle) ->
         (recurse gift (fWrapped wrappingPaperStyle acc))
    | Boxed gift ->
        recurse gift (fBoxed acc )
    | WithACard(gift, message) ->
        recurse gift (fWithACard message acc)


let description (gift: Gift) =
    let fBook (book: Book) acc = acc $"Book titled '{book.title}'"
    let fChocolate chocolate acc = acc $"{chocolate.chocolateType} chocolate"
    let fWrapped wrappingPaperStyle acc =
        let newAcc a =
            acc $"{a} wrapped in {wrappingPaperStyle} paper"

        newAcc
    let fBoxed acc =
        let newAcc a =
            let v = $"{a} in a box"
            acc v
        newAcc
    let fWithACard message acc =
        let newAcc a =
            let v = $"{a} with a card saying {message}"
            acc v
        newAcc

    quasiFoldRight fBook fChocolate fWrapped fBoxed fWithACard gift id


open Xunit
open Swensen.Unquote

[<Fact>]
let ``description of sample values`` () =
    test <@ description chocolate = "SeventyPercent chocolate" @>
    test <@ description book = "Book titled 'Wolf Hall'" @>
    test <@ description gift1 = "Book titled 'Wolf Hall' wrapped in HappyBirthday paper with a card saying Happy Birthday" @>
    test <@ description gift2 = "SeventyPercent chocolate in a box wrapped in HappyHolidays paper" @>
