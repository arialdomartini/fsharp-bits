module FSharpBits.PropertyBasedTesting.WithFsCheck.BirthdayGreetings

open System
open Expecto
open FsCheck

type Friend = { BirthDate: DateTime }

let toEmail _ = "Happy birthday!"

let toDays years = years * 365
let maxAge = 120 |> toDays

let born date (friend: Friend) = friend.BirthDate = date 

let greetFriends (friends: Friend list) today =
    friends
    |> Seq.filter (born today)
    |> Seq.map toEmail

let friends: Arbitrary<string list> = Arb.from<string list>

let randomDate = Arb.toGen Arb.from<DateTime>
let randomAgeInDays: Gen<int> = Gen.choose (1, maxAge)

let randomFriendBornInDate (birthday: DateTime) : Gen<Friend> =
    gen {
        let! date = Gen.constant birthday
        return { Friend.BirthDate = date }
    }

let friendsBornToday (today: DateTime) : Gen<Friend list> =
    (randomFriendBornInDate today) |> Gen.listOf

let friendsNotBornToday: Gen<Friend list> =
    gen {
        let! randomDate = randomDate
        let friend = randomFriendBornInDate randomDate
        let! friends = Gen.listOf friend
        return friends
    }

type RandomGen =
    { Today: DateTime
      BornToday: Friend list
      AllFriends: Friend list }

let myGroupOfFriends: Arbitrary<RandomGen> =
    gen {
        let! today = randomDate
        let! friendsBornToday = friendsBornToday today
        let! friendsNotBornToday = friendsNotBornToday
        let allOfThem = List.concat [ friendsBornToday; friendsNotBornToday ]

        return
            { Today = today
              BornToday = friendsBornToday
              AllFriends = allOfThem }
    }
    |> Arb.fromGen

let theSameEmailWasSentToTheOnesBornToday (randomGen: RandomGen) =
    let today = randomGen.Today
    let emails = greetFriends randomGen.AllFriends today

    Seq.length emails = List.length (randomGen.BornToday)
    && emails |> Seq.forall (fun e -> e = "Happy birthday!")


[<Tests>]
let birthdayGreetingsKataTests =
    testList
        "Birthday Greetings Kata tests"
        [ testProperty "sends the same email to all friends having birthday today" (Prop.forAll myGroupOfFriends theSameEmailWasSentToTheOnesBornToday) ]
