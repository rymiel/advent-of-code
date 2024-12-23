type t = int * int
type coord = t

let compare = compare
let manhattan (ax, ay) (bx, by) = abs (ax - bx) + abs (ay - by)

let ( % ) (ax, ay) (bx, by) : coord =
  let ( % ) a b =
    let result = a mod b in
    if result >= 0 then result else result + b
  in
  (ax % bx, ay % by)

let ( + ) (ax, ay) (bx, by) = (ax + bx, ay + by)
let ( - ) (ax, ay) (bx, by) = (ax - bx, ay - by)
let ( * ) (x, y) s = (s * x, s * y)
let print o (x, y) : unit = Printf.fprintf o "(%d, %d)" x y
let to_string (x, y) : string = Printf.sprintf "(%d, %d)" x y
let ( >> ) p (d : Dir.t) = p + Dir.to_coord d
let left = (-1, 0)
let right = (1, 0)
let up = (0, -1)
let down = (0, 1)
let west = left
let east = right
let north = up
let south = down
