module FSharpBits.ForFunAndProfit.Catamorphism.Folds.Fold

open FSharpBits.ForFunAndProfit.Catamorphism.Folds.NestedBoxes
open FSharpBits.ForFunAndProfit.Catamorphism.Folds.SampleValues
open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType


let rec foldGift
    (fBook: 'a -> Book -> 'r)
    (fChocolate: 'a -> Chocolate -> 'r)
    (fWrapped: 'a -> WrappingPaperStyle -> 'a)
    (fBoxed: 'a -> 'a)
    (fWithACard: 'a -> string -> 'a)
    (acc: 'a)
    (gift: Gift) :
    'r =

    let recurse = foldGift fBook fChocolate fWrapped fBoxed fWithACard

    match gift with
    | Book book ->
        fBook acc book
    | Chocolate chocolate ->
        fChocolate acc chocolate
    | Wrapped(gift, wrappingPaperStyle) ->
        recurse (fWrapped acc wrappingPaperStyle) gift
    | Boxed gift ->
        recurse (fBoxed acc) gift
    | WithACard(gift, message) ->
        recurse (fWithACard acc message) gift


let totalCost (gift: Gift) : decimal =

    let fBook acc (book: Book) = acc + book.price
    let fChocolate acc (chocolate: Chocolate) = acc + chocolate.price
    let fWrapped acc (_wrappingPaperStyle: WrappingPaperStyle) = acc + wrappedPrice
    let fBoxed acc = acc + boxedPrice
    let fWithACard acc _message = acc + withACardPrice

    foldGift fBook fChocolate fWrapped fBoxed fWithACard 0m gift


open Xunit
open Swensen.Unquote


[<Fact>]
let ``calculates cost'`` () =

    let gift = deeplyNestedBox (Book wolfHall) 100

    let tot = totalCost gift

    test <@ tot = 20m + 100m * boxedPrice @>
