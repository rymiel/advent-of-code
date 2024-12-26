module IntMap = Map.Make (Int)
module IntHash = Hashtbl.Make (Int)

let day15a i =
  let input =
    In_channel.input_all i |> String.split_on_char ',' |> List.map int_of_string
  in
  let (state : (int * int) IntMap.t ref) = ref IntMap.empty in
  let last = ref (-1) in
  let turn = ref 0 in
  let cycle (a, _) x = (x, a) in
  let speak num =
    incr turn;
    let v = IntMap.find_opt num !state |> Option.value ~default:(0, 0) in
    state := IntMap.add num (cycle v !turn) !state;
    last := num
  in
  let next () =
    let a, b = IntMap.find !last !state in
    if b = 0 then 0 else a - b
  in
  List.iter speak input;
  while !turn < 2020 do
    next () |> speak
  done;
  !last

let day15b i =
  let input =
    In_channel.input_all i |> String.split_on_char ',' |> List.map int_of_string
  in
  let (state : (int * int) IntHash.t) = IntHash.create 5_000_000 in
  let last = ref (-1) in
  let turn = ref 0 in
  let cycle (a, _) x = (x, a) in
  let speak num =
    incr turn;
    let v = IntHash.find_opt state num |> Option.value ~default:(0, 0) in
    IntHash.replace state num (cycle v !turn);
    last := num
  in
  let next () =
    let a, b = IntHash.find state !last in
    if b = 0 then 0 else a - b
  in
  List.iter speak input;
  while !turn < 30000000 do
    next () |> speak
  done;
  !last
