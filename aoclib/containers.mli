module type Container = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val uncons : 'a t -> ('a * 'a t) option
end

module type S = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val uncons : 'a t -> ('a * 'a t) option
  val sum : int t -> int
  val product : int t -> int
  val count_matches : ('a -> bool) -> 'a t -> int
  val count_item : 'a -> 'a t -> int
  val fold_lefti : ('acc -> 'a -> int -> 'acc) -> 'acc -> 'a t -> 'acc
  val max : int t -> int
  val min : int t -> int
  val minmax : int t -> (int * int)
  val map_minmax : ('a -> int) -> 'a t -> int * int
  val map_sum : ('a -> int) -> 'a t -> int
  val fold_left' : ('a -> 'a -> 'a) -> 'a t -> 'a
  val last : 'a t -> 'a
  val tally : 'a t -> ('a, int) Hashtbl.t
end

module Make : functor (C : Container) -> S with type 'a t := 'a C.t
module List : Container with type 'a t = 'a List.t
module Array : Container with type 'a t = 'a Array.t
module Seq : Container with type 'a t = 'a Seq.t
