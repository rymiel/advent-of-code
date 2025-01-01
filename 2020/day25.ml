open Aoclib.Util

let transform n i = i * n mod 20201227

let day25a i =
  let card_public, door_public =
    In_channel.input_lines i |> List.map int_of_string |> as_pair
  in
  let card_loop =
    Seq.iterate (transform 7) 1
    |> Seq.find_index (( = ) card_public)
    |> Option.get
  in
  Seq.iterate (transform door_public) 1 |> Seq.drop card_loop |> head
