module FSharpBits.PropertyBasedTesting.WithFsCheck.BirthdayGreetings

open Expecto
open FsCheck

let toEmail _ = "Happy birthday!"

let greeter friends  = friends |> Seq.map toEmail


[<Tests>]
let fizzBuzzTests =
    testProperty
        "sends the same email to each friend"
        (let friends = Arb.generate<string> |> Gen.listOf |> Arb.fromGen
         
         let props f =
             let emails = greeter f
             Seq.length emails = Seq.length f &&
             emails |> Seq.forall (fun e -> e = "Happy birthday!")

         Prop.forAll friends props)
