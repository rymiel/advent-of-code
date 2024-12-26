open Aoclib
open Aoclib.Util

type op = Acc | Jmp | Nop
type instr = op * int

let op_of_str = function
  | "acc" -> Acc
  | "jmp" -> Jmp
  | "nop" -> Nop
  | _ -> failwith "invalid"

module IntSet = Set.Make (Int)

type state = { ip : int; acc : int; instrs : instr array }

let exec (state : state) : state =
  let curr = state.instrs.(state.ip) in
  match curr with
  | Acc, i -> { state with ip = state.ip + 1; acc = state.acc + i }
  | Jmp, i -> { state with ip = state.ip + i }
  | Nop, _ -> { state with ip = state.ip + 1 }

let parse i =
  scan_lines i "%s %d%!" (fun a b -> (op_of_str a, b)) |> Array.of_list

let day8a i =
  let instrs = parse i in
  let seen = Hashset.create 1000 in
  Seq.iterate exec { ip = 0; acc = 0; instrs }
  |> Seq.find_map (fun state ->
         if Hashset.mem seen state.ip then Some state.acc
         else (
           Hashset.add seen state.ip;
           None))
  |> Option.get

let terminates_with instrs =
  let seen = Hashset.create 1000 in
  let initial = { ip = 0; acc = 0; instrs } in
  let last = ref initial in
  let loops =
    Seq.iterate exec initial
    |> Seq.take_while (fun s -> s.ip < Array.length s.instrs)
    |> Seq.exists (fun state ->
           last := state;
           if Hashset.mem seen state.ip then true
           else (
             Hashset.add seen state.ip;
             false))
  in
  if loops then None else Some (exec !last).acc

let day8b i =
  let instrs = parse i in
  Array.find_mapi
    (fun i instr ->
      let copy = Array.copy instrs in
      match instr with
      | Acc, _ -> None
      | Jmp, j ->
          copy.(i) <- (Nop, j);
          terminates_with copy
      | Nop, j ->
          copy.(i) <- (Jmp, j);
          terminates_with copy)
    instrs
  |> Option.get
