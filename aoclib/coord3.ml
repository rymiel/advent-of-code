type t = int * int * int
type coord3 = t

let ( + ) (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)
let ( - ) (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)
let x (x, _, _) = x
let y (_, y, _) = y
let z (_, _, z) = z
