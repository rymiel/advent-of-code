open Aoclib.Util

let rec permute l n f =
  match (n, l) with
  | 1, _ -> List.iter (fun x -> f [ x ]) l
  | _, x :: xs ->
      permute xs (n - 1) (fun ys -> f (x :: ys));
      permute xs n f
  | _, [] -> ()

exception ExitWith of int

let day1a (i : in_channel) : int =
  let nums = In_channel.input_lines i |> List.map int_of_string in
  try
    permute nums 2 (fun x -> if sum x = 2020 then raise (ExitWith (product x)));
    failwith "no 2020"
  with ExitWith m -> m

let day1b (i : in_channel) : int =
  let nums = In_channel.input_lines i |> List.map int_of_string in
  try
    permute nums 3 (fun x -> if sum x = 2020 then raise (ExitWith (product x)));
    failwith "no 2020"
  with ExitWith m -> m
