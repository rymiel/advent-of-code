open Aoclib
open Aoclib.Util

let mix = ( lxor )
let prune a = a mod 16777216

let step secret =
  let secret = secret * 64 |> mix secret |> prune in
  let secret =
    float_of_int secret /. 32.0 |> floor |> int_of_float |> mix secret |> prune
  in
  let secret = secret * 2048 |> mix secret |> prune in
  secret

let day22a i =
  let simulate initial =
    Seq.iterate step initial |> Seq.drop 2000 |> Util.head
  in
  In_channel.input_lines i |> List.map int_of_string |> List.map simulate |> sum

let day22b i =
  let simulate initial =
    Seq.iterate step initial
    |> Seq.map (fun i -> i mod 10)
    |> Seq.scan (fun (prev, _) price -> (price, price - prev)) (0, 0)
    |> Seq.drop 2 |> Seq.take 2000
    |> Seq.scan
         (fun ((_, d2, d3, d4), _) (price, diff) -> ((d2, d3, d4, diff), price))
         ((0, 0, 0, 0), 0)
    |> Seq.drop 4
  in
  let results seq =
    let table = Hashtbl.create 2000 in
    Seq.iter
      (fun (k, v) ->
        match Hashtbl.find_opt table k with
        | Some _ -> ()
        | None -> Hashtbl.add table k v)
      seq;
    table
  in
  let merge m h1 h2 =
    Hashtbl.iter
      (fun k v ->
        let n =
          match Hashtbl.find_opt h1 k with
          | Some existing -> m existing v
          | None -> v
        in
        Hashtbl.replace h1 k n)
      h2
  in
  In_channel.input_lines i |> List.map int_of_string
  |> List.fold_left
       (fun h i ->
         simulate i |> results |> merge ( + ) h;
         h)
       (Hashtbl.create 1_000_000)
  |> Hashtbl.to_seq_values |> Seq.fold_left max 0
