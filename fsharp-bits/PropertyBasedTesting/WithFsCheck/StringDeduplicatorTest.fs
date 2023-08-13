module FSharpBits.PropertyBasedTesting.WithFsCheck.StringDeduplicatorTest

open Expecto
open FsCheck
open System

let randomChar = Gen.choose (int 'a', int 'z') |> Gen.map char |> Gen.map string

let randomString =
    gen {
        let! randomChars = Gen.nonEmptyListOf randomChar
        return String.Join ("", randomChars)
    }

[<Tests>]
let tests =
    testList
        "some properties are hard to express"
        [ test "repeated characters are replaced with one" {
              let randomlyRepeat char =
                  gen {
                      let! randomLength = Arb.generate<int> |> Gen.map (fun i -> abs i + 1)
                      return String(char, randomLength)
                  }

              let arbitrarilyRepeatCharactersInString s =
                  gen {
                      let! result = Seq.map randomlyRepeat s |> Gen.sequence
                      let single = String.concat "" result
                      return single
                  }

              let cases =
                  gen {
                      let! r = randomString
                      let! repeated = arbitrarilyRepeatCharactersInString r
                      return (r, repeated)
                  }

              let c = Gen.sample 10 100 cases

              Expect.isTrue true
          } ]
