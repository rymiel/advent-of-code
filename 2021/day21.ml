open Aoclib
open Aoclib.Util

let rolls = ref 0

let dice () =
  let r = (!rolls mod 100) + 1 in
  incr rolls;
  r

type state = { pos : int * int; score : int * int }

let turn
    { pos = active_pos, inactive_pos; score = active_score, inactive_score } =
  let new_pos = (active_pos + dice () + dice () + dice ()) mod 10 in
  let new_score = active_score + new_pos + 1 in
  { pos = (inactive_pos, new_pos); score = (inactive_score, new_score) }

let game_over { score; _ } = Pair.exists (fun i -> i >= 1000) score
let loser { score = a, b; _ } = if a < b then a else b

let day21a i =
  let initial_posns =
    scan_lines i "Player %d starting position: %d" (fun _ i -> i - 1) |> as_pair
  in
  let initial_state = { pos = initial_posns; score = (0, 0) } in
  let loser_score =
    Seq.iterate turn initial_state
    |> Seq.drop_while (Fun.negate game_over)
    |> head |> loser
  in
  let rolls = !rolls in
  rolls * loser_score

let day21b i =
  ignore i;
  failwith "TODO"
