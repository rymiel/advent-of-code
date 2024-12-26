let day13a i =
  let now = input_line i |> int_of_string in
  let ids =
    input_line i |> String.split_on_char ','
    |> List.filter (( <> ) "x")
    |> List.map int_of_string
  in
  let waits = List.map (fun x -> (x - (now mod x), x)) ids in
  let wait, id = List.fold_left min (Int.max_int, 0) waits in
  id * wait

module CRT = struct
  open Z

  let bezout_coefficients a b =
    let rec aux (old_r, r) (old_s, s) (old_t, t) =
      if r = Z.zero then (old_s, old_t)
      else
        let quotient = old_r / r in
        aux
          (r, old_r - (quotient * r))
          (s, old_s - (quotient * s))
          (t, old_t - (quotient * t))
    in
    aux (a, b) (one, zero) (zero, one)

  type congruence = { rhs : Z.t; modulo : Z.t }

  let solve_pair { rhs = a1; modulo = n1 } { rhs = a2; modulo = n2 } =
    let m1, m2 = bezout_coefficients n1 n2 in
    let solution = (a1 * m2 * n2) + (a2 * m1 * n1) in
    let new_modulo = n1 * n2 in
    { rhs = solution mod new_modulo; modulo = new_modulo }

  let reduce f = function
    | [] -> invalid_arg "reduce"
    | x :: xs -> List.fold_left f x xs

  let solve_system = reduce solve_pair

  let system_of_schedule s =
    s |> String.split_on_char ',' |> List.to_seq
    |> Seq.map (fun i -> if i = "x" then None else Some (of_string i))
    |> Seq.mapi (fun i a -> (of_int i, a))
    |> Seq.filter_map (fun (i, a) ->
           Option.map (fun a -> { rhs = ~-i; modulo = a }) a)
    |> List.of_seq

  let positive_value { rhs; modulo } =
    let result = rhs mod modulo in
    if result >= zero then result else result + modulo
end

let day13b i =
  input_line i |> ignore;
  input_line i |> CRT.system_of_schedule |> CRT.solve_system
  |> CRT.positive_value |> Z.to_int
