type solver = in_channel -> int

let solve (year : int) (s : solver) (problem : string) : unit =
  In_channel.with_open_text (Printf.sprintf "%d/txt/%s.txt" year problem)
    (fun i ->
      let result = s i in
      if result <> 0 then Printf.printf "%d\n%!" result)

let scan_lines (i : in_channel) fmt f =
  In_channel.input_lines i |> List.map (fun s -> Scanf.sscanf s fmt f)

let scan_all i fmt f =
  let rec aux i fmt f =
    match Scanf.bscanf i fmt f with
    | x -> x :: aux i fmt f
    | exception End_of_file -> []
  in
  let i = Scanf.Scanning.from_channel i in
  aux i fmt f

module ListExt = Containers.Make (Containers.List)
module ArrayExt = Containers.Make (Containers.Array)
module SeqExt = Containers.Make (Containers.Seq)

(* backwards compatibility *)
let sum = ListExt.sum
let product = ListExt.product
let count_matches = ListExt.count_matches
let count_item = ListExt.count_item
let fold_lefti = ListExt.fold_lefti
(* end backwards compatibility *)

let chars s = String.to_seq s |> List.of_seq
let string_of_char = String.make 1
let chars_s = Fun.compose (List.map string_of_char) chars

let count_char char =
  String.fold_left (fun n c -> if c = char then n + 1 else n) 0

let as_pair = function [ a; b ] -> (a, b) | _ -> failwith "not a pair"

let partition_at split_at list =
  let rec part left right split = function
    | [] -> (List.rev left, List.rev right)
    | x :: l ->
        if split then part left (x :: right) true l
        else if x = split_at then part left right true l
        else part (x :: left) right false l
  in
  part [] [] false list

let rec partition_all split_at list =
  match partition_at split_at list with
  | [], _ -> []
  | current, rest -> current :: partition_all split_at rest

let ( % ) a b =
  let result = a mod b in
  if result >= 0 then result else result + b

type coord = Coord.t
type dir = Dir.t

module CoordSet = Set.Make (Coord)
module CoordMap = Map.Make (Coord)

module BinaryMap2D = struct
  type t = { width : int; height : int; set : CoordSet.t }

  let init width height : t = { width; height; set = CoordSet.empty }
  let add coord v = { v with set = CoordSet.add coord v.set }
  let has coord v = CoordSet.mem coord v.set

  let iter f v =
    for y = 0 to v.height - 1 do
      for x = 0 to v.width - 1 do
        if has (x, y) v then f (x, y)
      done
    done

  let iter_all f v =
    for y = 0 to v.height - 1 do
      for x = 0 to v.width - 1 do
        f (x, y) (has (x, y) v)
      done
    done

  let print v =
    for y = 0 to v.height - 1 do
      for x = 0 to v.width - 1 do
        print_char (if has (x, y) v then '#' else '.')
      done;
      print_newline ()
    done

  let dump v =
    Printf.printf "width = %d, height = %d, set = [%s]\n%!" v.width v.height
      (CoordSet.to_list v.set
      |> List.map (fun (x, y) -> Printf.sprintf "(%d, %d)" x y)
      |> String.concat ", ")

  let read_matrix_cb (i : in_channel) (on : char -> coord -> bool) =
    let lines = In_channel.input_lines i in
    let height = List.length lines in
    let width = List.hd lines |> String.length in
    let map = init width height in
    let add_row y m s =
      fold_lefti
        (fun m c x -> if on c (x, y) then add (x, y) m else m)
        m (chars s)
    in
    fold_lefti (fun m line y -> add_row y m line) map lines

  let read_matrix (i : in_channel) (on : char) =
    read_matrix_cb i (fun c _ -> c = on)

  let width v = v.width
  let height v = v.height
  let oob (x, y) (map : t) = x < 0 || y < 0 || x >= map.width || y >= map.height
end

module CharMap2D = struct
  type t = { width : int; height : int; map : char CoordMap.t }

  let init width height : t = { width; height; map = CoordMap.empty }
  let add coord c v = { v with map = CoordMap.add coord c v.map }
  let has coord v = CoordMap.mem coord v.map
  let get coord v = CoordMap.find coord v.map
  let get_opt coord v = CoordMap.find_opt coord v.map

  let iter f v =
    for y = 0 to v.height - 1 do
      for x = 0 to v.width - 1 do
        match get_opt (x, y) v with None -> () | Some c -> f c (x, y)
      done
    done

  let print v =
    for y = 0 to v.height - 1 do
      for x = 0 to v.width - 1 do
        print_char (match get_opt (x, y) v with None -> '.' | Some c -> c)
      done;
      print_newline ()
    done

  let dump v =
    Printf.printf "width = %d, height = %d, set = [%s]\n%!" v.width v.height
      (CoordMap.bindings v.map
      |> List.map (fun ((x, y), c) -> Printf.sprintf "(%d, %d)=%C" x y c)
      |> String.concat ", ")

  let read_matrix (i : in_channel) (off : char) =
    let lines = In_channel.input_lines i in
    let height = List.length lines in
    let width = List.hd lines |> String.length in
    let map = init width height in
    let add_row y m s =
      fold_lefti
        (fun m c x -> if c = off then m else add (x, y) c m)
        m (chars s)
    in
    fold_lefti (fun m line y -> add_row y m line) map lines

  let width v = v.width
  let height v = v.height
  let oob (x, y) (map : t) = x < 0 || y < 0 || x >= map.width || y >= map.height

  let in_bounds (x, y) (map : t) =
    x >= 0 && y >= 0 && x < map.width && y < map.height
end

module Pathfind = Astar.Make (Coord)

let cardinal_pathfind ~(width : int) ~(height : int) (walls : coord Hashset.t)
    (start : coord) (goal : coord) =
  let in_bounds (x, y) = x >= 0 && y >= 0 && x < width && y < height in
  let dirs = [ Coord.up; Coord.right; Coord.down; Coord.left ] in
  let neighbours p =
    List.filter_map
      (fun d ->
        let n = Coord.(p + d) in
        if in_bounds n && (not @@ Hashset.mem walls n) then Some n else None)
      dirs
  in
  Pathfind.pathfind (Coord.manhattan goal) neighbours Coord.manhattan start goal

let rec binary_searchi (left : int) (right : int) (f : int -> bool) : int =
  if left >= right then left
  else
    let mid = left + ((right - left) / 2) in
    if f mid then binary_searchi left mid f
    else binary_searchi (mid + 1) right f

let rec pow2 = function
  | 0 -> 1
  | 1 -> 2
  | n ->
      let b = pow2 (n / 2) in
      b * b * if n mod 2 = 0 then 1 else 2

let head (seq : 'a Seq.t) : 'a =
  match seq () with
  | Seq.Nil -> failwith "empty sequence"
  | Seq.Cons (a, _) -> a

module StringSet = Set.Make (String)

let array_shift_left arr n =
  let last = Array.length arr - 1 in
  let new_arr =
    Array.mapi (fun i _ -> if i = last then n else arr.(i + 1)) arr
  in
  new_arr

let sliding_window : 'a. int -> 'a Seq.t -> 'a array Seq.t =
 fun size seq ->
  match seq () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (x, rest) ->
      let initial = Array.make size x in
      Seq.scan (fun arr n -> array_shift_left arr n) initial rest
      |> Seq.drop (size - 1)

let sliding_pair : 'a. 'a Seq.t -> ('a * 'a) Seq.t =
 fun seq -> sliding_window 2 seq |> Seq.map (fun arr -> (arr.(0), arr.(1)))

let fixed_window : 'a. int -> 'a Seq.t -> 'a array Seq.t =
  let rec filteri f i seq () =
    match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (x, next) ->
        if f i then Seq.Cons (x, filteri f (i + 1) next)
        else filteri f (i + 1) next ()
  in
  let filteri f seq = filteri f 0 seq in
  fun size seq -> sliding_window size seq |> filteri (fun i -> i mod size = 0)

let rec permute_pairs seq =
  match seq () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (x, next) ->
      let s = Seq.map (fun y -> (x, y)) next in
      let n = permute_pairs next in
      Seq.append s n
