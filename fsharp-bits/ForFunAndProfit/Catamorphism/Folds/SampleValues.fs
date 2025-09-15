module FSharpBits.ForFunAndProfit.Catamorphism.Folds.SampleValues

open FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType

let wolfHall = { title = "Wolf Hall"; price = 20m }

let yummyChoc =
    { chocolateType = SeventyPercent
      price = 5m }

let birthdayPresent =
    WithACard(Wrapped(Book wolfHall, HappyBirthday), "Happy Birthday")

let christmasPresent = Wrapped(Boxed(Chocolate yummyChoc), HappyHolidays)
