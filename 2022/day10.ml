open Aoclib.Util

let parse i =
  In_channel.input_lines i
  |> List.concat_map (fun s ->
         if s = "noop" then [ 0 ]
         else
           let d = Scanf.sscanf s "addx %d" Fun.id in
           [ 0; d ])
  |> List.to_seq
  |> Seq.scan (fun (_, cur) n -> (cur, cur + n)) (1, 1)

let day10a i =
  let indices = [ 20; 60; 100; 140; 180; 220 ] in
  parse i
  |> Seq.mapi (fun i (a, _) -> (i, a))
  |> Seq.filter (fun (i, _) -> List.mem i indices)
  |> Seq.map (fun (i, a) -> i * a)
  |> SeqExt.sum

let day10b i =
  parse i |> Seq.drop 1
  |> Seq.mapi (fun i a -> (i mod 40, fst a))
  |> Seq.fold_left
       (fun buf (ix, sx) ->
         if ix = 0 then Buffer.add_char buf '\n';
         if abs (ix - sx) <= 1 then Buffer.add_string buf "\u{2588}"
         else Buffer.add_char buf ' ';
         buf)
       (Buffer.create 300)
  |> fun buf ->
  Buffer.contents buf |> print_endline;
  0
