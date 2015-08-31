type DateScale = Hour | Hours | Day | Days | Week | Weeks
type DateDirection = Ago | Hence

let getDate interval scale direction = 
    let absoluteHours = 
        match scale with 
        | Hour | Hours -> 1 * interval
        | Day | Days -> 24 * interval 
        | Week | Weeks -> 24 * 7 * interval
    let signedHours =
        match direction with
        | Ago -> -1 * absoluteHours
        | Hence -> absoluteHours
    System.DateTime.Now.AddHours(float signedHours) 


let fiveDaysAgo = getDate 5 Days Ago
printfn "5 days ago = %A" fiveDaysAgo
