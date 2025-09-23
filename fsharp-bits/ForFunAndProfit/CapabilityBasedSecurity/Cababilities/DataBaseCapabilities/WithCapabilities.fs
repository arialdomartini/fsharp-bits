module FSharpBits.ForFunAndProfit.CapabilityBasedSecurity.Cababilities.DataBaseCapabilities.WithCapabilities

open System.Security.Principal

// Repository is never exposed to Application
// and it does not contain any check on identity and roles
module private CartRepository =
    let getCart _cartId = "Cart data"

// Security has its own module
module CapabilityProvider =

    let belongsTo (_user: IPrincipal) (_cartId: int) : bool = failwith "Not yet implemented"

    let isAnAgent (_principal: IPrincipal) : bool = failwith "Not yet implemented"

    let getCapability cartId user =
        if cartId |> belongsTo user || user |> isAnAgent then
            Some (fun () -> CartRepository.getCart cartId)
        else
            None

open CapabilityProvider

// Application is not directly using Repository
module Application =
    let run cartId user =
        match getCapability cartId user with
        | None -> "sorry, you can't"
        | Some getCart ->
            let cartData = getCart ()
            $"here's your cart: {cartData}"
