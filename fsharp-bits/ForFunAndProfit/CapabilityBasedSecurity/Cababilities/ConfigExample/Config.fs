module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.Cababilities.ConfigExample.Config

type MessageFlag =
    | ShowMessageAgain
    | DoNotShowMessageAgain

type ConnectionString = ConnectionString of string
type Color = System.Drawing.Color

type ConfigurationCapabilities =
    { getMessageFlag: unit -> MessageFlag
      setMessageFlag: MessageFlag -> unit
      getBackgroundColor: unit -> Color
      setBackgroundColor: Color -> unit
      getConnectionString: unit -> ConnectionString
      setConnectionString: ConnectionString -> unit }

module private ConfigurationStore =
    let mutable messageFlag: MessageFlag = ShowMessageAgain
    let mutable connectionString: ConnectionString = ConnectionString "invalid"
    let mutable color: Color = System.Drawing.Color.White


let capabilities =
    { getMessageFlag = fun () -> ConfigurationStore.messageFlag
      setMessageFlag = fun messageFlag -> ConfigurationStore.messageFlag <- messageFlag
      getBackgroundColor = fun () -> ConfigurationStore.color
      setBackgroundColor = fun color -> ConfigurationStore.color <- color
      getConnectionString = fun () -> ConfigurationStore.connectionString
      setConnectionString = fun connectionString -> ConfigurationStore.connectionString <- connectionString }
