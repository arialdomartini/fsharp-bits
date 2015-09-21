type Animal(noiseMakingStrategy) =
    member this.MakeNoise =
        noiseMakingStrategy() |> printfn "Making the noise %s"

let meowing() = "Meow"
let cat = Animal(meowing)
cat.MakeNoise


let isOddSecond() = if System.DateTime.Now.Second % 2 = 0 then true else false

let woofOrBark() = if isOddSecond() then "Bau" else "woooooo"

let dog = Animal(woofOrBark)

dog.MakeNoise
