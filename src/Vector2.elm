module Vector2 exposing (..)

type Axis = X | Y
type alias Vector2 = { x: Int, y: Int }

new: Int -> Int -> Vector2
new x y = { x = x, y = y }

map: (Int -> Int) -> Vector2 -> Vector2
map f v = new (f v.x) (f v.y)


add: Vector2 -> Vector2 -> Vector2
add a b = new (a.x + b.x) (a.y + b.y)

tuple: Vector2 -> (Int, Int)
tuple v = (v.x, v.y)
