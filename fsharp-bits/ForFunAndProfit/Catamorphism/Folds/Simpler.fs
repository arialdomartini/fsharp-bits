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
            let newAcc = $"WithAttribute({attribute}) with initial {acc}"
            descriptionAccumulator newAcc innerValue

    let rec foldLeft fFinal fRecursive fWithAttribute acc value=
        let recurse = foldLeft fFinal fRecursive fWithAttribute
        match value with
        | Final s -> fFinal acc s
        | Recursive innerValue ->
            let newAcc = fRecursive acc
            recurse newAcc innerValue

        | WithAttribute(attribute, innerValue) ->
            let newAcc = fWithAttribute acc attribute
            recurse newAcc innerValue

    let rec descriptionFoldLeft acc (value: Value) =
        let fFinal acc s = $"Final({s}) in {acc}"
        let fRecursive acc = $"Recursive in {acc}"
        let fWithAttribute acc attribute = $"WithAttribute({attribute}) with initial {acc}"

        foldLeft fFinal fRecursive fWithAttribute acc value

    let rec descriptionPassingFunction acc (value: Value) =
        let fFinal generate s = generate $"Final({s})"
        let fRecursive generate =
            let newGenerator description =
                let newDescription = $"Recursive of {description}"
                generate newDescription
            newGenerator

        let fWithAttribute generate attribute =
            let newGenerator description =
                let newDescription = $"WithAttribute of {attribute} + {description}"
                generate newDescription
            newGenerator

        foldLeft fFinal fRecursive fWithAttribute acc value

    let rec foldRight fFinal fRecursive fWithAttribute value generator =
        let recurse = foldRight fFinal fRecursive fWithAttribute

        match value with
        | Final s -> generator (fFinal s)
        | Recursive innerValue ->
            let newGenerator description =
                let newDescription = fRecursive description
                generator newDescription
            recurse innerValue newGenerator

        | WithAttribute(attribute, innerValue) ->
            let newGenerator description =
                let newDescription = fWithAttribute attribute description
                generator newDescription
            recurse innerValue newGenerator

    let rec descriptionFoldRight (value: Value) acc =

        let fFinal s = $"Final({s})"
        let fRecursive description = $"Recursive of {description}"
        let fWithAttribute attribute description = $"WithAttribute of {attribute} + {description}"

        foldRight fFinal fRecursive fWithAttribute value acc

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
        test <@ descriptionAccumulator "let's start" withAttribute = "Final(final) in Recursive in WithAttribute(attribute) with initial let's start" @>

    [<Fact>]
    member this.``with foldLeft`` () =
        test <@ descriptionFoldLeft "let's start" withAttribute = "Final(final) in Recursive in WithAttribute(attribute) with initial let's start" @>

    [<Fact>]
    member this.``with passing a function`` () =
        test <@ descriptionPassingFunction id withAttribute = expectedDescription @>

    [<Fact>]
    member this.``with foldRight`` () =
        test <@ descriptionFoldRight withAttribute id = expectedDescription @>
