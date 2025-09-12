module FSharpBits.ForFunAndProfit.Catamorphism.UpgradingChocolate

open RecursiveType
open Cata
open Xunit
open Swensen.Unquote

let upgradeChocolate gift =
    let fChocolate (chocolate: Chocolate) =
        Chocolate
            { price = chocolate.price
              chocolateType = SeventyPercent }

    cataGift Book fChocolate Wrapped Boxed WithACard gift


[<Fact>]
let ``upgrades chocolate, maintaining the structure`` () =

    let blackChocolate =
        Chocolate
            { Chocolate.chocolateType = ChocolateType.Black
              price = 2m }

    let inABox = Boxed blackChocolate
    let withACard = WithACard(inABox, "Choco!")

    let upgraded =
        Chocolate
            { Chocolate.chocolateType = ChocolateType.SeventyPercent
              price = 2m }

    test <@ upgradeChocolate blackChocolate = upgraded @>
    test <@ upgradeChocolate inABox = Boxed upgraded @>
    test <@ upgradeChocolate withACard = WithACard(Boxed upgraded, "Choco!") @>
