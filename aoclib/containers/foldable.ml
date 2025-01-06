module type Foldable = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
end

module type S = sig
  type 'a t

  val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
  val sum : int t -> int
  val product : int t -> int
  val count_matches : ('a -> bool) -> 'a t -> int
  val count_item : 'a -> 'a t -> int
  val fold_lefti : ('acc -> 'a -> int -> 'acc) -> 'acc -> 'a t -> 'acc
  val max : int t -> int
  val min : int t -> int
  val minmax : int t -> int * int
  val map_minmax : ('a -> int) -> 'a t -> int * int
  val map_sum : ('a -> int) -> 'a t -> int
  val tally : 'a t -> ('a, int) Hashtbl.t
end

module Make (C : Foldable) : S with type 'a t := 'a C.t = struct
  let fold_left = C.fold_left
  let sum = C.fold_left ( + ) 0
  let product = C.fold_left ( * ) 1

  let count_matches pred =
    C.fold_left (fun n c -> if pred c then n + 1 else n) 0

  let count_item i = C.fold_left (fun n c -> if c = i then n + 1 else n) 0

  let fold_lefti f acc c =
    C.fold_left (fun (a, i) j -> (f a j i, i + 1)) (acc, 0) c |> fst

  let max = C.fold_left max Int.min_int
  let min = C.fold_left min Int.max_int

  let minmax =
    C.fold_left
      (fun (small, big) i -> (Stdlib.min small i, Stdlib.max big i))
      (Int.max_int, Int.min_int)

  let map_minmax f =
    C.fold_left
      (fun (small, big) i ->
        let j = f i in
        (Stdlib.min small j, Stdlib.max big j))
      (Int.max_int, Int.min_int)

  let map_sum f = C.fold_left (fun acc i -> acc + f i) 0

  let tally c =
    let table = Hashtbl.create 0 in
    C.fold_left
      (fun () i ->
        Hashtbl.replace table i
          (Option.value ~default:0 (Hashtbl.find_opt table i) + 1))
      () c;
    table
end

module List = Make (List)
module Array = Make (Array)
module Seq = Make (Seq)
