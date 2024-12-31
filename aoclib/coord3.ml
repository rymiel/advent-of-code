type t = int * int * int
type coord3 = t

let ( + ) (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)
let ( - ) (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)
