open Aoclib.Util
module CharMap = Map.Make (Char)

let read_bits bits stream =
  Seq.repeat () |> Seq.take bits
  |> Seq.fold_left (fun i () -> (i lsl 1) + stream ()) 0

type body = Literal of int | Operator of packet list
and packet = { version : int; id : int; body : body; read : int }

let rec read_packet stream =
  let version = read_bits 3 stream in
  let id = read_bits 3 stream in
  let body, read =
    (match id with 4 -> read_literal_body | _ -> read_operator_body) stream
  in
  { version; id; body; read = read + 6 }

and read_literal_body stream =
  let acc = ref 0 in
  let continue = ref 1 in
  let read = ref 0 in
  while !continue = 1 do
    continue := stream ();
    acc := (!acc lsl 4) + read_bits 4 stream;
    incr read
  done;
  (Literal !acc, !read * 5)

and read_operator_body stream =
  let length_type = stream () in
  if length_type = 0 then (
    let limit = read_bits 15 stream in
    let buf = ref [] in
    let read = ref 0 in
    while !read < limit do
      let packet = read_packet stream in
      read := !read + packet.read;
      buf := packet :: !buf
    done;
    (Operator (List.rev !buf), limit + 16))
  else
    let len = read_bits 11 stream in
    let packets = List.init len (fun _ -> read_packet stream) in
    (Operator packets, ListExt.map_sum (fun p -> p.read) packets + 12)

let hex_map =
  CharMap.of_list
    [
      ('0', [ 0; 0; 0; 0 ]);
      ('1', [ 0; 0; 0; 1 ]);
      ('2', [ 0; 0; 1; 0 ]);
      ('3', [ 0; 0; 1; 1 ]);
      ('4', [ 0; 1; 0; 0 ]);
      ('5', [ 0; 1; 0; 1 ]);
      ('6', [ 0; 1; 1; 0 ]);
      ('7', [ 0; 1; 1; 1 ]);
      ('8', [ 1; 0; 0; 0 ]);
      ('9', [ 1; 0; 0; 1 ]);
      ('A', [ 1; 0; 1; 0 ]);
      ('B', [ 1; 0; 1; 1 ]);
      ('C', [ 1; 1; 0; 0 ]);
      ('D', [ 1; 1; 0; 1 ]);
      ('E', [ 1; 1; 1; 0 ]);
      ('F', [ 1; 1; 1; 1 ]);
    ]

let parse i =
  let stream =
    In_channel.input_all i |> String.trim |> String.to_seq
    |> Seq.flat_map (fun c -> CharMap.find c hex_map |> List.to_seq)
    |> Seq.to_dispenser
    |> fun dispenser () -> dispenser () |> Option.get
  in
  read_packet stream

let day16a i =
  let rec version_sum { version; body; _ } =
    match body with
    | Literal _ -> version
    | Operator list -> version + ListExt.map_sum version_sum list
  in
  version_sum (parse i)

let day16b i =
  let rec evaluate { body; id; _ } =
    match body with
    | Literal i -> i
    | Operator list -> (
        let children = List.map evaluate list in
        let op f =
          let a, b = as_pair children in
          if f a b then 1 else 0
        in
        match id with
        | 0 -> ListExt.sum children
        | 1 -> ListExt.product children
        | 2 -> ListExt.min children
        | 3 -> ListExt.max children
        | 5 -> op ( > )
        | 6 -> op ( < )
        | 7 -> op ( = )
        | _ -> failwith "invalid")
  in
  evaluate (parse i)
