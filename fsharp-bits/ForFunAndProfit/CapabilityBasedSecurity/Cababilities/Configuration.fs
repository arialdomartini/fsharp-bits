module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.Cababilities.Configuration

module Capabilities =
    type private Configuration =
        { messageFlag: bool }

    type SetMessageFlag = bool -> unit
    type ConfigurationCapabilities = { setMessageFlag: SetMessageFlag }

    let mutable private configuration =
        { messageFlag = false }

    // Capability
    let private setMessageFlag value =
        configuration <- { configuration with messageFlag = value }

    // PowerBox (http://wiki.c2.com/?PowerBox)
    let powerBox = { setMessageFlag = setMessageFlag }

open Capabilities

module Application =
    let doSomething () =
        // Acquire capability
        let messageFlagCapability = powerBox.setMessageFlag

        // Use capability
        messageFlagCapability true

module ApplicationAlternative =
    // Receives Capability
    let doSomething (messageFlagCapability: SetMessageFlag) =

        // Use capability
        messageFlagCapability true
