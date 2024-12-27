open Aoclib.Util

type hand = Rock | Paper | Scissors
type outcome = { better : hand; worse : hand }

let outcome = function
  | Rock -> { better = Paper; worse = Scissors }
  | Paper -> { better = Scissors; worse = Rock }
  | Scissors -> { better = Rock; worse = Paper }

let round_score ~me ~opponent =
  if me = opponent then 3 else if (outcome me).worse = opponent then 6 else 0

let hand_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let hand_of_char = function
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> invalid_arg "hand_of_char"

let day2a i =
  scan_lines i "%c %c" (fun a b -> (hand_of_char a, hand_of_char b))
  |> ListExt.map_sum (fun (opponent, me) ->
         round_score ~me ~opponent + hand_score me)

let adjust_hand h =
  let oc = outcome h in
  function
  | 'X' -> (*lose*) oc.worse
  | 'Y' -> (*draw*) h
  | 'Z' -> (*win*) oc.better
  | _ -> invalid_arg "adjust_hand"

let day2b i =
  scan_lines i "%c %c" (fun a b -> (hand_of_char a, b))
  |> List.map (fun (h, c) -> (h, adjust_hand h c))
  |> ListExt.map_sum (fun (opponent, me) ->
         round_score ~me ~opponent + hand_score me)
