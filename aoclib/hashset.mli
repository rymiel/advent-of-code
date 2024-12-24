type 'a t

val is_empty : 'a t -> bool
val to_seq : 'a t -> 'a Seq.t
val create : int -> 'a t
val add : 'a t -> 'a -> unit
val remove : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool
val iter : ('a -> unit) -> 'a t -> unit
val clear : 'a t -> unit
