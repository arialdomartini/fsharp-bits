module FSharpBits.MonadTransformers.FunctionOverloading.NoFunctionOverloading


//
// open Xunit
// open Swensen.Unquote
//
// type Task = { Title: string; Duration: int }
// type Box = Box of (int * int * int)
//
//
// let joinTasks (t1: Task) (t2: Task) =
//     { Title = $"{t1.Title} and {t2.Title}"
//       Duration = t1.Duration + t2.Duration }
//
// let joinBoxes (boxLeft: Box) (boxRight: Box) =
//     let (Box (x1, y1, z1)) = boxLeft
//     let (Box (x2, y2, z2)) = boxRight
//     Box (x1 + x2, max y1 y2, max z1 z2)
//
// [<Fact>]
// let ``joins Tasks`` () =
//     let before = { Title = "buy new keyboards"; Duration = 1 }
//     let after = { Title = "write more bugs"; Duration = 5 }
//
//     test <@ joinTasks before after = {Title = "buy new keyboards and write more bugs"; Duration = 6} @>
//
// [<Fact>]
// let ``joins Boxes`` () =
//     let left = Box (1, 2, 3)
//     let right = Box (10, 20, 30)
//
//     test <@ joinBoxes left right = Box (11, 20, 30) @>
