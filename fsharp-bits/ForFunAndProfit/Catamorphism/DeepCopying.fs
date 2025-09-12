module FSharpBits.ForFunAndProfit.Catamorphism.DeepCopying

open Cata
open RecursiveType
open SampleValues


let deepCopy gift =
    let fBook = Book
    let fChocolate = Chocolate
    let fWrapped gift wrappingPaperStyle = Wrapped(gift, wrappingPaperStyle)
    let fBoxed = Boxed
    let fWithACard gift message = WithACard(gift, message)

    cataGift
        fBook
        fChocolate
        fWrapped
        fBoxed
        fWithACard
        gift

open Xunit
open Swensen.Unquote
open LanguagePrimitives

[<Fact>]
let ``deep copy value`` () =
    test <@ deepCopy gift2 = gift2 @>
    test <@ PhysicalEquality (deepCopy gift2) gift2 = false @>
