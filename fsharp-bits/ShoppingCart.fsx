type CartItem = string
type EmptyState = NoItems
type ActiveState = { UnpaidItems: CartItem list; }
type PaidForState   = { PaidItems: CartItem list; Payment: decimal}

type Cart =
  | Empty of EmptyState
  | Active of ActiveState
  | PaidFor of PaidForState

let addToEmptyState items =
   Cart.Active {UnpaidItems=[items]} 
  

let addToActiveState state item =
  let newList = item :: state.UnpaidItems
  Cart.Active {state with UnpaidItems = newList}

let removeFromActiveState state itemToRemove =
  let newList = state.UnpaidItems |> List.filter (fun item -> item <> itemToRemove)
  match newList with
    | [] -> Cart.Empty NoItems
    | _  -> Cart.Active {state with UnpaidItems = newList }

let payForActiveState state amount =
      Cart.PaidFor { PaidItems = state.UnpaidItems; Payment = amount }

type EmptyState with
  member this.Add = addToEmptyState
  member this.Display = fun() -> printfn "The cart is empty"
  
type ActiveState with
  member this.Add = addToActiveState this
  member this.Remove = removeFromActiveState this
  member this.PayFor = payForActiveState this
  member this.Display = fun() -> printfn "The cart contains %A unpaid items" this.UnpaidItems

type PaidForState with
  member this.Display = fun() -> printfn "The cart contains %A paid for %A euros" this.PaidItems this.Payment
  

let addItemToCard cart item =
  match cart with
  | Empty c -> c.Add item
  | Active c -> c.Add item
  | PaidFor _ ->
       printfn "Can't add items to paid Carts"
       cart

let removeItemFromCart cart item =
  match cart with
    | Empty _ ->
        printfn "Can't remove items from empty carts"
        cart
    | Active c -> c.Remove item
    | PaidFor _ ->
      printfn "Can't remove items from paid for carts"    
      cart

let payFor cart item =
  match cart with
    | Empty _ ->
      printfn "Pay for nothing..."
      cart
    | Active state -> state.PayFor item
    | PaidFor _ ->
      printfn "Already paid"
      cart
      
let displayCart cart =
  match cart with
    | Empty state -> state.Display
    | Active state -> state.Display
    | PaidFor state -> state.Display


type Cart with
  static member NewCart = Cart.Empty NoItems
  member this.Add = addItemToCard this
  member this.Remove = removeItemFromCart this
  member this.Display = displayCart this
  member this.PayFor = payFor this

// Usage
let cart = Cart.NewCart
cart.Display()
let cart2 = cart.Add "potato"
cart2.Display()
let cart3 = cart2.Add "carrot"
cart3.Display()
let cart4 = cart3.PayFor 100m
cart4.Display()
  
