module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.KeyValueInterface

open System.IO

type Configuration =
    { username: string
      password: string
      url: string }

open System.Text.Json

let private fromJson (content: string) =
    JsonSerializer.Deserialize<Configuration>(content)

let private toJson (content: 'a) = JsonSerializer.Serialize(content)


let setConfigurationValue key value =
    let configurationFileName = "shared-configuration-file.conf"

    let configuration = File.ReadAllText(configurationFileName) |> fromJson

    let newConfiguration =
        match key with
        | "username" -> { configuration with username = value }
        | "password" -> { configuration with password = value }
        | "url" -> { configuration with url = value }
        | _ -> configuration

    File.WriteAllText(configurationFileName, newConfiguration |> toJson)


module Client =
    
    setConfigurationValue "url" "new-value"
