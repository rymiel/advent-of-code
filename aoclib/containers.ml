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
  val map_sum : ('a -> int) -> 'a t -> int
  val fold_left' : ('a -> 'a -> 'a) -> 'a t -> 'a
end

module Make (C : Container) : S with type 'a t := 'a C.t = struct
  let fold_left = C.fold_left
  let uncons = C.uncons
  let sum = C.fold_left ( + ) 0
  let product = C.fold_left ( * ) 1

  let count_matches pred =
    C.fold_left (fun n c -> if pred c then n + 1 else n) 0

  let count_item i = C.fold_left (fun n c -> if c = i then n + 1 else n) 0

  let fold_lefti f acc c =
    C.fold_left (fun (a, i) j -> (f a j i, i + 1)) (acc, 0) c |> fst

  let max = C.fold_left max Int.min_int
  let min = C.fold_left min Int.max_int
  let map_sum f = C.fold_left (fun acc i -> acc + f i) 0

  let fold_left' f c =
    match uncons c with
    | None -> invalid_arg "fold_left'"
    | Some (x, xs) -> fold_left f x xs
end

module List : Container with type 'a t = 'a List.t = struct
  type 'a t = 'a list

  let fold_left = List.fold_left
  let uncons = function [] -> None | x :: xs -> Some (x, xs)
end

module Array : Container with type 'a t = 'a Array.t = struct
  type 'a t = 'a array

  let fold_left = Array.fold_left

  let uncons arr =
    if Array.length arr = 0 then None
    else Some (arr.(0), Array.sub arr 1 (Array.length arr - 1))
end

module Seq : Container with type 'a t = 'a Seq.t = Seq
