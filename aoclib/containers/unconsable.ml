module type Unconsable = sig
  include Foldable.Foldable

  val uncons : 'a t -> ('a * 'a t) option
end

module type S = sig
  type 'a t

  val uncons : 'a t -> ('a * 'a t) option
  val fold_left' : ('a -> 'a -> 'a) -> 'a t -> 'a
  val last : 'a t -> 'a
end

module Make (C : Unconsable) : S with type 'a t := 'a C.t = struct
  let uncons = C.uncons

  let fold_left' f c =
    match uncons c with
    | None -> invalid_arg "fold_left'"
    | Some (x, xs) -> C.fold_left f x xs

  let last c = fold_left' (fun _ x -> x) c
end

module List = Make (struct
  include List

  let uncons = function [] -> None | x :: xs -> Some (x, xs)
end)

module Seq = Make (Seq)
