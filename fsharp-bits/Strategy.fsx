type Animal(noiseMakingStrategy) =
    member this.MakeNoise =
        noiseMakingStrategy() |> printfn "Making the noise %s"

let meowing() = "Meow"
let cat = Animal(meowing)
cat.MakeNoise
