open Aoclib.Util

type op = Add | Mul
type calc = { a : int option; b : int option; op : op }

type monkey = {
  id : int;
  items : int list ref;
  operation : calc;
  test : int;
  if_true : int;
  if_false : int;
  tally : int ref;
}

let read_arg s = if s = "old" then None else Some (int_of_string s)

let run_operation { a; b; op } num =
  let a = Option.value ~default:num a in
  let b = Option.value ~default:num b in
  (match op with Add -> ( + ) | Mul -> ( * )) a b

let append l a = l := a :: !l

let monkey_business monkeys monkey num =
  incr monkey.tally;
  let num = run_operation monkey.operation num in
  let num = num / 3 in
  let divisible = num mod monkey.test = 0 in
  let next_monkey =
    monkeys.(if divisible then monkey.if_true else monkey.if_false)
  in
  append next_monkey.items num

let parse i =
  scan_all i
    "Monkey %d:\n\
    \  Starting items: %[0-9, ]\n\
    \  Operation: new = %s %s %s\n\
    \  Test: divisible by %d\n\
    \    If true: throw to monkey %d\n\
    \    If false: throw to monkey %d "
    (fun id starting arg_a op arg_b test if_true if_false ->
      let starting =
        String.split_on_char ',' starting
        |> List.map String.trim |> List.map int_of_string
      in
      let items = ref starting in
      let op = match op with "+" -> Add | "*" -> Mul | _ -> failwith "op" in
      let operation = { a = read_arg arg_a; b = read_arg arg_b; op } in
      { id; items; operation; test; if_true; if_false; tally = ref 0 })
  |> Array.of_list

let day11a i =
  let monkeys = parse i in
  for _ = 1 to 20 do
    Array.iter
      (fun monkey ->
        List.iter
          (fun num -> monkey_business monkeys monkey num)
          !(monkey.items);
        monkey.items := [])
      monkeys
  done;
  Array.map (fun m -> !(m.tally)) monkeys
  |> Array.to_list |> List.sort compare |> List.rev |> List.to_seq |> Seq.take 2
  |> SeqExt.product

let rec gcd a b = match b with 0 -> a | b -> gcd b (a mod b)
let lcm a b = a * b / gcd a b

let monkey_business2 monkeys monkey num =
  incr monkey.tally;
  let num = run_operation monkey.operation num in
  let num =
    num mod (Array.map (fun m -> m.test) monkeys |> ArrayExt.fold_left' lcm)
  in
  let divisible = num mod monkey.test = 0 in
  let next_monkey =
    monkeys.(if divisible then monkey.if_true else monkey.if_false)
  in
  append next_monkey.items num

let day11b i =
  let monkeys = parse i in
  for _ = 1 to 10000 do
    Array.iter
      (fun monkey ->
        List.iter
          (fun num -> monkey_business2 monkeys monkey num)
          !(monkey.items);
        monkey.items := [])
      monkeys
  done;
  Array.map (fun m -> !(m.tally)) monkeys
  |> Array.to_list |> List.sort compare |> List.rev |> List.to_seq |> Seq.take 2
  |> SeqExt.product
