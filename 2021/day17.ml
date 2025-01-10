open Aoclib.Util

let day17a i =
  (* the arc's peak is always a triangle number.
     given a velocity y, we get the peak by adding y + (y - 1) + (y - 2) + ... + 1.
     this is a triangle number.
     then, we find another triangle number where the difference lies in the target y range.
     since the arc crosses y=0 on the way down, we really only have to check until the difference becomes
     greater than the smaller target y *)
  let _min_x, _max_x, min_y, max_y =
    Scanf.sscanf (In_channel.input_all i) "target area: x=%d..%d, y=%d..%d"
      (fun a b c d -> (a, b, c, d))
  in
  let triangle n = Seq.ints n |> Seq.map (fun n -> n * (n + 1) / 2) in
  triangle 0
  |> Seq.take (abs min_y)
  |> Seq.mapi (fun i n -> (i, n))
  |> Seq.filter (fun (i, n) ->
         triangle i
         |> Seq.drop_while (fun a -> n - a > max_y)
         |> Seq.take_while (fun a -> n - a >= min_y)
         |> Seq.is_empty |> not)
  |> Seq.map snd |> SeqExt.max

let day17b i =
  ignore i;
  failwith "TODO"
