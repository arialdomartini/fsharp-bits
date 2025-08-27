module FSharpBits.LeetCode.Triangle

open Xunit
open Swensen.Unquote

let minimumPathSum triangle =
    triangle
    |> List.rev
    |> List.reduce (fun (lower: int list) upper ->
        List.map2 min lower.Tail lower
        |> List.map2 (+) upper)

[<Fact>]
let ``minimum path in a triangle`` () =

    let input =
        [ [2]
          [3; 4]
          [6; 5; 7]
          [4; 1; 8; 3] ]

    let length = minimumPathSum input |> List.min

    test <@ length = 11 @>
