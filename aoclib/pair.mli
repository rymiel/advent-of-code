type ('a, 'b) t = 'a * 'b
val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val apply : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
val flip : 'a * 'b -> 'b * 'a
val mem : 'a -> 'a * 'a -> bool
val to_seq : 'a * 'a -> 'a Seq.t
val exists : ('a -> bool) -> 'a * 'a -> bool
