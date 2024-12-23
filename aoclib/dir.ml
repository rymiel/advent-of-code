type t = Up | Down | Left | Right
type dir = t

let to_coord = function
  | Up -> (0, -1)
  | Down -> (0, 1)
  | Left -> (-1, 0)
  | Right -> (1, 0)

let to_string = function
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
