type ('a, 'b) t = 'a * 'b

let map f (a, b) = (f a, f b)
let apply f (a, b) = f a b
let flip (a, b) = (b, a)
let mem v (a, b) = a = v || b = v
let to_seq (a, b) = Seq.cons a (Seq.cons b Seq.empty)
let exists f (a, b) = f a || f b
