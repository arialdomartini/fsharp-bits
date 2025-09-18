module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.SharingFileName

open System.IO

type ConfigurationFileName = string

type IConfiguration =
    abstract member  getConfigurationFileName: unit -> ConfigurationFileName

// or

let getConfigurationFileName () = "shared-configuration-file.conf"

type Configuration = { key: string }

open System.Text.Json

let fromJson (content: string) =
    JsonSerializer.Deserialize<Configuration>(content)

let toJson (content: 'a) =
    JsonSerializer.Serialize(content)

module Client =
    let configurationFileName = getConfigurationFileName ()

    let configuration =
        File.ReadAllText(configurationFileName)
        |> fromJson

    let newConfiguration = { configuration with key = "new-value" }

    File.WriteAllText(configurationFileName, newConfiguration |> toJson)
