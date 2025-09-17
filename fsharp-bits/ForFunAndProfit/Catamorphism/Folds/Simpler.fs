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

    let rec descriptionQuasiCata (value: Value) =

        let fFinal s = $"Final({s})"
        let fRecursive innerDescription = $"Recursive({innerDescription})"
        let fWithAttribute innerDescription attribute = $"WithAttribute({attribute}, {innerDescription})"

        match value with
        | Final s -> fFinal s
        | Recursive innerValue ->
            let innerDescription = descriptionQuasiCata innerValue
            fRecursive innerDescription
        | WithAttribute(attribute, innerValue) ->
            let innerDescription = descriptionQuasiCata innerValue
            fWithAttribute innerDescription attribute

    let rec cata fFinal fRecursive fWithAttribute (value: Value) =
        let recurse = cata fFinal fRecursive fWithAttribute

        match value with
        | Final s -> fFinal s
        | Recursive innerValue ->
            let innerDescription = recurse innerValue
            fRecursive innerDescription
        | WithAttribute(attribute, innerValue) ->
            let innerDescription = recurse innerValue
            fWithAttribute innerDescription attribute

    let descriptionCata (value: Value) =

        let fFinal s = $"Final({s})"
        let fRecursive innerDescription = $"Recursive({innerDescription})"
        let fWithAttribute innerDescription attribute = $"WithAttribute({attribute}, {innerDescription})"

        cata fFinal fRecursive fWithAttribute value


    [<Fact>]
    member this.``description of sample values with plain recursion`` () =
        test <@ descriptionRecursion withAttribute = expectedDescription @>

    [<Fact>]
    member this.``description of sample values with quasi-catamorphism`` () =
        test <@ descriptionQuasiCata withAttribute = expectedDescription @>

    [<Fact>]
    member this.``description of sample values with catamorphism`` () =
        test <@ descriptionCata withAttribute = expectedDescription @>
