module type Container = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module type S = sig
  type 'a t

  val sum : int t -> int
  val product : int t -> int
  val count_matches : ('a -> bool) -> 'a t -> int
  val count_item : 'a -> 'a t -> int
  val fold_lefti : ('acc -> 'a -> int -> 'acc) -> 'acc -> 'a t -> 'acc
end

module Make : functor (C : Container) -> S with type 'a t := 'a C.t
