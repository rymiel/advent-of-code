open Aoclib.Util

let scan s =
  Scanf.bscanf s
    "Register A: %d\nRegister B: %d\nRegister C: %d\n\nProgram: %s %!"
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

let eval state = function
  | Adv c -> next { state with a = dv state c }
  | Bdv c -> next { state with b = dv state c }
  | Cdv c -> next { state with c = dv state c }
  | Bxl i -> next { state with b = state.b lxor i }
  | Bst c -> next { state with b = resolve_combo state c land 0b111 }
  | Jnz i -> { state with ip = (if state.a = 0 then state.ip + 2 else i) }
  | Bxc -> next { state with b = state.b lxor state.c }
  | Out c ->
      next
        {
          state with
          out = (resolve_combo state c |> ( land ) 0b111) :: state.out;
        }

let eval_all a b c ops =
  let state = { a; b; c; ops; ip = 0; out = [] } in
  let rec step state =
    if state.ip >= Array.length state.ops then state
    else
      let op = read_op state.ops.(state.ip) state.ops.(state.ip + 1) in
      step (eval state op)
  in
  (step state).out |> List.rev |> List.map string_of_int |> String.concat ","

let day17a i =
  let i = Scanf.Scanning.from_channel i in
  let a, b, c, ops = scan i in
  eval_all a b c ops |> print_endline;
  0

let day17b i =
  ignore i;
  failwith "TODO"
