type ('a, 'b) t = 'a * 'b

let map f (a, b) = (f a, f b)
