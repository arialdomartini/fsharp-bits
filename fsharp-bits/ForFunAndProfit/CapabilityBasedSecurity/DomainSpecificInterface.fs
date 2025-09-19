module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.DomainSpecificInterface

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

type Configs =
    { setUsername: string -> unit
      setPassword: string -> unit
      setUrl : string -> unit }

let configs =
    { setUsername = failwith "Not yet implemented"
      setPassword = failwith "Not yet implemented"
      setUrl = setUrl }



module Client =

    configs.setUrl "new-value"
