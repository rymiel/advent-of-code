type ('a, 'b) t = 'a * 'b
val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val apply : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val flip : 'a * 'b -> 'b * 'a
