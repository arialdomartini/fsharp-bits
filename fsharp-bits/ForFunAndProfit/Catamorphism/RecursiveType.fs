module FSharpBits.ForFunAndProfit.Catamorphism.RecursiveType

type Book = { title: string; price: decimal }

type ChocolateType =
    | Black
    | Milk
    | SeventyPercent

type Chocolate = { chocolateType: ChocolateType; price: decimal }

type WrappingPaperStyle =
    | HappyBirthday
    | HappyHolidays
    | SolidColor

type Gift =
    | Book of Book
    | Chocolate of Chocolate
    | Wrapped of gift: Gift * wrappingPaperStyle: WrappingPaperStyle
    | Boxed of Gift
    | WithACard of gift: Gift * message: string
