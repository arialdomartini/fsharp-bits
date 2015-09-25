type PersonName = {FirstName:string; SecondName:string}
let john = {FirstName = "John"; SecondName = "Miner"}

// Copying objects
let mary = {john with FirstName = "Mary"}
let john_copy = {john with FirstName = "John"}

assert System.Object.ReferenceEquals(john, john_copy)


let list = [1;2;3;4]
let prepended_list = 0 :: list
let like_list = list.Tail

assert System.Object.ReferenceEquals(list, like_list)

