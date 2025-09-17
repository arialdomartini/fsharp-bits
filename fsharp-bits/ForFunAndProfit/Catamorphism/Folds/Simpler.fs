module FSharpBits.ForFunAndProfit.Catamorphism.Folds.Simpler

open Xunit
open Swensen.Unquote


type Value =
    | Final of string
    | Recursive of Value
    | WithAttribute of (string * Value)

type SampleValues() =

    let final = Final "final"
    let recursive = Recursive final
    let withAttribute = WithAttribute ("attribute", recursive)

    let expectedDescription = "WithAttribute(attribute, Recursive(Final(final)))"

    let rec descriptionRecursion (value: Value) =
        match value with
        | Final s -> $"Final({s})"
        | Recursive innerValue ->
            let innerDescription = descriptionRecursion innerValue
            $"Recursive({innerDescription})"
        | WithAttribute(attribute, innerValue) ->
            let innerDescription = descriptionRecursion innerValue
            $"WithAttribute({attribute}, {innerDescription})"

    [<Fact>]
    member this.``description of sample values with plain recursion`` () =
        test <@ descriptionRecursion withAttribute = expectedDescription @>
