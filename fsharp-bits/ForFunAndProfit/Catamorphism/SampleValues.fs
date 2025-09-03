module FSharpBits.ForFunAndProfit.Catamorphism.SampleValues

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType

let book = { title = "Wolf Hall"; price = 20m }

let chocolate =
    { Chocolate.chocolateType = SeventyPercent
      price = 5m }

let gift1 =
    WithACard(
        gift = Wrapped(
            gift = Book book,
            wrappingPaperStyle = HappyBirthday),
        message = "Happy Birthday")

let gift2 = Wrapped(Boxed(Chocolate chocolate), HappyHolidays)
