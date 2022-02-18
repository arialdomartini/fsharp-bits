let (|MultipleOf3|_|) i = if i % 3 = 0 then Some MultipleOf3 else None
let (|MultipleOf5|_|) i = if i % 5 = 0 then Some MultipleOf5 else None


let fizzBuzz i =
    match i with
    | MultipleOf3 & MultipleOf5 -> printf "Fizz Buzz "
    | MultipleOf3               -> printf "Fizz "
    | MultipleOf5               -> printf "Buzz"
    | _                         -> printf "%d " i


[1..500] |> List.iter fizzBuzz