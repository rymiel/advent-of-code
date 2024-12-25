open Aoclib.Util
open Aoclib

let scan s =
  Scanf.bscanf s
    "Register A: %i\nRegister B: %i\nRegister C: %i\n\nProgram: %s %!"
    (fun a b c p ->
      ( a,
        b,
        c,
        String.split_on_char ',' p |> List.map int_of_string |> Array.of_list ))

type combo = Literal of int | A | B | C

type op =
  | Adv of combo
  | Bxl of int
  | Bst of combo
  | Jnz of int
  | Bxc
  | Out of combo
  | Bdv of combo
  | Cdv of combo

let read_combo x =
  match x with
  | 0 | 1 | 2 | 3 -> Literal x
  | 4 -> A
  | 5 -> B
  | 6 -> C
  | _ -> failwith "invalid combo"

type state = {
  a : int;
  b : int;
  c : int;
  ip : int;
  ops : int array;
  out : int list;
}

let next state = { state with ip = state.ip + 2 }

let resolve_combo state = function
  | Literal x -> x
  | A -> state.a
  | B -> state.b
  | C -> state.c

let dv state c = state.a / pow2 (resolve_combo state c)

let read_op a b =
  match a with
  | 0 -> Adv (read_combo b)
  | 1 -> Bxl b
  | 2 -> Bst (read_combo b)
  | 3 -> Jnz b
  | 4 -> Bxc
  | 5 -> Out (read_combo b)
  | 6 -> Bdv (read_combo b)
  | 7 -> Cdv (read_combo b)
  | _ -> failwith "nope"

let string_of_combo = function
  | Literal x -> string_of_int x
  | A -> "A"
  | B -> "B"
  | C -> "C"

let eval state = function
  | Adv c ->
      (* Printf.printf "[adv %s] A = A / 2^%s = %d / %d = %d\n%!"
         (string_of_combo c) (string_of_combo c) state.a
         (pow2 (resolve_combo state c))
         (dv state c); *)
      next { state with a = dv state c }
  | Bdv c ->
      (* Printf.printf "[adv %s] B = A / 2^%s = %d / %d = %d\n%!"
         (string_of_combo c) (string_of_combo c) state.a
         (pow2 (resolve_combo state c))
         (dv state c); *)
      next { state with b = dv state c }
  | Cdv c ->
      (* Printf.printf "[adv %s] C = A / 2^%s = %d / %d = %d\n%!"
         (string_of_combo c) (string_of_combo c) state.a
         (pow2 (resolve_combo state c))
         (dv state c); *)
      next { state with c = dv state c }
  | Bxl i ->
      (* Printf.printf "[bxl %d] B = %d ^ %d = %d\n%!" i state.b i (state.b lxor i); *)
      next { state with b = state.b lxor i }
  | Bst c ->
      (* Printf.printf "[bst %s] B = %s %% 8 = %d %% 8 = %d\n%!"
         (string_of_combo c) (string_of_combo c) (resolve_combo state c)
         (resolve_combo state c land 0b111); *)
      next { state with b = resolve_combo state c land 0b111 }
  | Jnz i ->
      (* Printf.printf "[jnz %d] A = %d -> %s\n%!" i state.a
         (if state.a = 0 then "don't jump" else "jump"); *)
      { state with ip = (if state.a = 0 then state.ip + 2 else i) }
  | Bxc ->
      (* Printf.printf "[bxc _] B = B ^ C = %d ^ %d = %d\n%!" state.b state.c
         (state.b lxor state.c); *)
      next { state with b = state.b lxor state.c }
  | Out c ->
      (* Printf.printf "[out %s] out %s = out %d = %d\n%!" (string_of_combo c)
         (string_of_combo c) (resolve_combo state c)
         (resolve_combo state c land 0b111); *)
      next { state with out = (resolve_combo state c land 0b111) :: state.out }

let eval_all a b c ops =
  let state = { a; b; c; ops; ip = 0; out = [] } in
  let rec step state =
    if state.ip >= Array.length state.ops then state
    else
      let op = read_op state.ops.(state.ip) state.ops.(state.ip + 1) in
      step (eval state op)
  in
  (step state).out |> List.rev

let day17a i =
  let i = Scanf.Scanning.from_channel i in
  let a, b, c, ops = scan i in
  eval_all a b c ops |> List.map string_of_int |> String.concat ","
  |> print_endline;
  0

let day17b i =
  let i = Scanf.Scanning.from_channel i in
  let _, b, c, ops = scan i in
  let initial_set = Hashset.create 1_000_000 in
  Hashset.add initial_set 0;
  Array.to_list ops |> List.rev |> List.to_seq
  |> Seq.fold_left
       (fun set num ->
         let new_set = Hashset.create 1_000_00 in
         Hashset.iter
           (fun curr ->
             for i = 0 to 7 do
               let n = (curr * 8) + i in
               let res = eval_all n b c ops in
               if num = List.hd res then Hashset.add new_set n
             done)
           set;
         new_set)
       initial_set
  |> Hashset.to_seq
  |> Seq.fold_left min Int.max_int
