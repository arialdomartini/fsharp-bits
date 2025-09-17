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

    let expectedDescription = "WithAttribute of attribute + Recursive of Final(final)"

    let rec descriptionRecursive (value: Value) =
        match value with
        | Final s -> $"Final({s})"
        | Recursive innerValue ->
            let innerDescription = descriptionRecursive innerValue
            $"Recursive of {innerDescription}"
        | WithAttribute(attribute, innerValue) ->
            let innerDescription = descriptionRecursive innerValue
            $"WithAttribute of {attribute} + {innerDescription}"

    let rec descriptionQuasiCata (value: Value) =

        let fFinal s = $"Final({s})"
        let fRecursive innerDescription = $"Recursive of {innerDescription}"
        let fWithAttribute innerDescription attribute = $"WithAttribute of {attribute} + {innerDescription}"

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
        let fRecursive innerDescription = $"Recursive of {innerDescription}"
        let fWithAttribute innerDescription attribute = $"WithAttribute of {attribute} + {innerDescription}"

        cata fFinal fRecursive fWithAttribute value

    let rec descriptionAccumulator acc (value: Value) =
        match value with
        | Final s -> $"Final({s}) in {acc}"
        | Recursive innerValue ->
            let newAcc = $"Recursive in {acc}"
            descriptionAccumulator newAcc innerValue

        | WithAttribute(attribute, innerValue) ->
            let newAcc = $"WithAttribute({attribute})"
            descriptionAccumulator newAcc innerValue


    [<Fact>]
    member this.``with plain recursion`` () =
        test <@ descriptionRecursive withAttribute = expectedDescription @>

    [<Fact>]
    member this.``with quasi-catamorphism`` () =
        test <@ descriptionQuasiCata withAttribute = expectedDescription @>

    [<Fact>]
    member this.``with catamorphism`` () =
        test <@ descriptionCata withAttribute = expectedDescription @>

    [<Fact>]
    member this.``with plain recursion, using an accumulator: result it reversed!`` () =
        test <@ descriptionAccumulator "" withAttribute = "Final(final) in Recursive in WithAttribute(attribute)" @>
