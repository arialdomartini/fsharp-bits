type Shape =
    | Circle of radius:int
    | Rectangle of width:int * height:int
    | Polygon of point:(int * int) list
    | Point of position:(int * int)

let draw shape =
    match shape with
        | Circle radius ->
            printfn "The circle has a radius of %d" radius
        | Rectangle (height, width) ->
            printfn "The rectangle is %d high and %d wide" height width
        | Polygon points ->
            printfn "This polygon is made of these points %A" points
        | _ -> 
            printfn "I don't recognize this shape"

let circle = Circle(10)
let rectangle = Rectangle(5, 8)
let polygon = Polygon( [ (1,1); (5,4); (10,10)])

draw circle
draw rectangle
draw polygon