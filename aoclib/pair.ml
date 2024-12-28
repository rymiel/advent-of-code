type ('a, 'b) t = 'a * 'b

let map f (a, b) = (f a, f b)
let apply f (a, b) = f a b
