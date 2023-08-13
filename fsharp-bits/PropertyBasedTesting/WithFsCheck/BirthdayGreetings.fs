module FSharpBits.PropertyBasedTesting.WithFsCheck.BirthdayGreetings

open Expecto
open FsCheck

let toEmail _ = "Happy birthday!"

let greet friends = friends |> Seq.map toEmail

let friends: Arbitrary<string list> = Arb.generate<string list> |> Arb.fromGen

let theSameEmailWasSent friends: bool =
   let emails = greet friends
   Seq.length emails = Seq.length friends
   && emails |> Seq.forall (fun e -> e = "Happy birthday!")

[<Tests>]
let birthdayGreetingsKataTests =
    testList "Birthday Greetings Kata tests"
        [ testProperty "sends the same email to each friend" (Prop.forAll friends theSameEmailWasSent) ]
