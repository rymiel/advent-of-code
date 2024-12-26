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

module Make (C : Container) : S with type 'a t := 'a C.t = struct
  let sum = C.fold_left ( + ) 0
  let product = C.fold_left ( * ) 1

  let count_matches pred =
    C.fold_left (fun n c -> if pred c then n + 1 else n) 0

  let count_item i = C.fold_left (fun n c -> if c = i then n + 1 else n) 0

  let fold_lefti f acc c =
    C.fold_left (fun (a, i) j -> (f a j i, i + 1)) (acc, 0) c |> fst
end
