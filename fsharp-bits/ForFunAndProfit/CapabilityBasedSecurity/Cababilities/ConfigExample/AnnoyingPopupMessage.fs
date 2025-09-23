module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.Cababilities.ConfigExample.AnnoyingPopupMessage

open Config


// Received only the strictly necessary capabilities
let createConnectionStringForm capabilities =
    let get, set = capabilities

    let (ConnectionString connectionString) = get ()

    // do stuff

    let newValue = ConnectionString (connectionString.ToLower())

    set newValue


// Receives all the capabilities
let startApplication capabilities =
    // Passes only a subset of capabilities (the strictly necessary) to its inner components
    createConnectionStringForm (capabilities.getConnectionString, capabilities.setConnectionString)

    // May dispatch different fine-tuned capabilities to its other inner components.
