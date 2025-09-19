module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.InterfaceSegregation

open System.IO

type Configuration =
    { username: string
      password: string
      url: string }

open System.Text.Json

let private fromJson (content: string) =
    JsonSerializer.Deserialize<Configuration>(content)

let private toJson (content: 'a) = JsonSerializer.Serialize(content)

let private setUrl value =
    let configurationFileName = "shared-configuration-file.conf"
    let configuration = File.ReadAllText(configurationFileName) |> fromJson

    let newConfiguration = { configuration with url = value }

    File.WriteAllText(configurationFileName, newConfiguration |> toJson)

type OnlySetUrl =
    { setUrl : string -> unit }

// Principle of Least Authority
let onlySetUrl =
    { setUrl = setUrl }



module Client =

    onlySetUrl.setUrl "new-value"
