module FSharpBits.ForFunAndProfit.Catamorphism.Products.SampleValues

open MutuallyRecursiveType

let label =
    Bought
        { name = "label"
          weight = 1
          vendor = Some "ACME" }

let bottle =
    Bought
        { name = "bottle"
          weight = 2
          vendor = Some "ACME" }

let formulation =
    Bought
        { name = "formulation"
          weight = 3
          vendor = None }

let shampoo =
    Made
        { name = "shampoo"
          weight = 10
          components =
            [ { quantity = 1; product = formulation }
              { quantity = 1; product = bottle }
              { quantity = 2; product = label } ] }

let twoPack =
    Made
        { name = "twoPack"
          weight = 5
          components = [ { quantity = 2; product = shampoo } ] }
