module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.Cababilities.DataBaseCapabilities.WithoutCapability

open System.Security.Principal

// Check if a Cart belongs to user
let belongsTo (_user: IPrincipal) (_cartId: int) : bool = failwith "Not yet implemented"

// Agents can access any Cart
let isAnAgent (_principal: IPrincipal) : bool = failwith "Not yet implemented"


module CartRepository =
    let getCart cartId user =
        if cartId |> belongsTo user || user |> isAnAgent then
            Some "Cart data"
        else
            None
