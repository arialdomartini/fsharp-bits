module FSharpBits.PropertyBasedTesting.WithFsCheck.BirthdayGreetings

open System
open Expecto
open FsCheck

type Friend = { BirthDate: DateTime }

let toEmail _ = "Happy birthday!"

let toDays years = years * 365
let maxAge = 120 |> toDays

let born date friend= friend.BirthDate = date 

let greetFriends (friends: Friend list) today =
    friends
    |> Seq.filter (born today)
    |> Seq.map toEmail

let friends = Arb.from<string list>

let randomDate = Arb.toGen Arb.from<DateTime>
let randomAgeInDays: Gen<int> = Gen.choose (1, maxAge)

let randomFriendBornInDate birthday =
    gen {
        let! date = Gen.constant birthday
        return { Friend.BirthDate = date }
    }

let friendsBornOn birthday =
    (randomFriendBornInDate birthday) |> Gen.listOf

let friendsNotBornToday =
    gen {
        let! randomDate = randomDate
        let! friends = friendsBornOn randomDate
        return friends
    }

let friendsNotBornToday2 =
    randomDate >>= friendsBornOn

type RandomGen =
    { Today: DateTime
      BornToday: Friend list
      AllFriends: Friend list }

let myGroupOfFriends =
    gen {
        let! today = randomDate
        let! friendsBornToday = friendsBornOn today
        let! friendsNotBornToday = friendsNotBornToday
        let allOfThem = List.concat [ friendsBornToday; friendsNotBornToday ]

        return
            { Today = today
              BornToday = friendsBornToday
              AllFriends = allOfThem }
    }
    |> Arb.fromGen

let theSameEmailWasSentToTheOnesBornToday randomGen =
    let today = randomGen.Today
    let emails = greetFriends randomGen.AllFriends today

    Seq.length emails = List.length (randomGen.BornToday)
    && emails |> Seq.forall (fun e -> e = "Happy birthday!")


[<Tests>]
let birthdayGreetingsKataTests =
    testList
        "Birthday Greetings Kata tests"
        [ testProperty "sends the same email to all friends having birthday today" (Prop.forAll myGroupOfFriends theSameEmailWasSentToTheOnesBornToday) ]
