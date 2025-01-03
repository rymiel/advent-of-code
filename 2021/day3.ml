open Aoclib

let binary_to_int s = "0b" ^ s |> int_of_string
let bit_counts seq = Seq.partition (( = ) '1') seq |> Pair.map Seq.length

let day3a i =
  let common =
    In_channel.input_lines i |> List.to_seq |> Seq.map String.to_seq
    |> Seq.transpose |> Seq.map bit_counts
    |> Seq.map (fun (ones, zeroes) -> if ones > zeroes then '1' else '0')
  in
  let gamma = String.of_seq common |> binary_to_int in
  let epsilon =
    Seq.map (fun c -> if c = '1' then '0' else '1') common
    |> String.of_seq |> binary_to_int
  in
  gamma * epsilon

let day3b i =
  let input = In_channel.input_lines i in
  let bits = List.hd input |> String.length in
  let rating f =
    Seq.ints 0 |> Seq.take bits
    |> Seq.fold_left
         (fun lines i ->
           match lines with
           | [] -> []
           | x :: [] -> [ x ]
           | lines ->
               let ones, zeroes =
                 List.to_seq lines |> Seq.map (fun s -> s.[i]) |> bit_counts
               in
               let common = f ones zeroes in
               List.filter (fun s -> s.[i] = common) lines)
         input
    |> List.hd |> binary_to_int
  in
  let oxygen =
    rating (fun ones zeroes -> if ones >= zeroes then '1' else '0')
  in
  let co2 = rating (fun ones zeroes -> if zeroes <= ones then '0' else '1') in
  oxygen * co2
