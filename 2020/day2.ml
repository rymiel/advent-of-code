open Aoclib.Util

let day2a (i : in_channel) : int =
  let data = scan_lines i "%d-%d %c: %s%!" (fun a b c d -> (a, b, c, d)) in
  let matches =
    count_matches
      (fun (a, b, c, d) ->
        let m = count_char c d in
        m >= a && m <= b)
      data
  in
  matches

let day2b (i : in_channel) : int =
  let data = scan_lines i "%d-%d %c: %s%!" (fun a b c d -> (a, b, c, d)) in
  let matches =
    count_matches (fun (a, b, c, d) -> d.[a - 1] = c <> (d.[b - 1] = c)) data
  in
  matches
