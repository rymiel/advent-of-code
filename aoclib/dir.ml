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

let rotate_cw = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let rotate_ccw = function
  | Up -> Left
  | Right -> Up
  | Down -> Right
  | Left -> Down

let scale dir n =
  let x, y = to_coord dir in
  (x * n, y * n)
