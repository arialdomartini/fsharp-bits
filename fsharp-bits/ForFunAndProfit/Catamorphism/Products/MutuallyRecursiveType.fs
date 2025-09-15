module FSharpBits.ForFunAndProfit.Catamorphism.Products.MutuallyRecursiveType

type Product =
    | Bought of BoughtProduct
    | Made of MadeProduct

and BoughtProduct =
    { name: string
      weight: int
      vendor: string option }

and MadeProduct =
    { name: string
      weight: int
      components: Component list }

and Component = { product: Product; quantity: int }
