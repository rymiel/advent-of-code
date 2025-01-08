type 'a t = ('a, unit) Hashtbl.t

val is_empty : 'a t -> bool
val to_seq : 'a t -> 'a Seq.t
val create : int -> 'a t
val add : 'a t -> 'a -> unit
val add_seq : 'a t -> 'a Seq.t -> unit
val remove : 'a t -> 'a -> unit
val mem : 'a t -> 'a -> bool
val iter : ('a -> unit) -> 'a t -> unit
val clear : 'a t -> unit
val length : 'a t -> int
val copy : 'a t -> 'a t
val filter_inplace : ('a -> bool) -> 'a t -> unit
val filter_map : ('a -> 'a option) -> 'a t -> 'a t
