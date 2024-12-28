open Aoclib.Util

type dir = { name : string; children : node list ref; parent : dir option }
and node = Dir of dir | File of { name : string; size : int }

let parse i =
  let fs = { name = "/"; children = ref []; parent = None } in
  let cwd = ref fs in
  In_channel.input_lines i
  |> List.iter (fun line ->
         if String.starts_with ~prefix:"$ cd" line then
           match String.sub line 5 (String.length line - 5) with
           | "/" -> cwd := fs
           | ".." -> cwd := Option.get !cwd.parent
           | new_dir ->
               cwd :=
                 !(!cwd.children)
                 |> List.find_map (function
                      | Dir ({ name; _ } as d) ->
                          if name = new_dir then Some d else None
                      | _ -> None)
                 |> Option.get
         else if line = "$ ls" then ()
         else if String.starts_with ~prefix:"dir" line then
           let dir = String.sub line 4 (String.length line - 4) in
           !cwd.children :=
             Dir { name = dir; children = ref []; parent = Some !cwd }
             :: !(!cwd.children)
         else
           let size, name = String.split_on_char ' ' line |> as_pair in
           let size = int_of_string size in
           !cwd.children := File { name; size } :: !(!cwd.children));
  fs

let dir_sizes fs =
  let sizes = Dynarray.create () in
  let rec record_sizes = function
    | Dir d ->
        let r = !(d.children) |> List.map record_sizes |> sum in
        Dynarray.add_last sizes r;
        r
    | File f -> f.size
  in
  let root_size = record_sizes (Dir fs) in
  (root_size, sizes)

let day7a i =
  let fs = parse i in
  let _, sizes = dir_sizes fs in
  Dynarray.to_seq sizes |> Seq.filter (fun v -> v <= 100_000) |> SeqExt.sum

let day7b i =
  let fs = parse i in
  let root_size, sizes = dir_sizes fs in
  Dynarray.to_seq sizes
  |> Seq.filter (fun v -> root_size - v <= 40000000)
  |> SeqExt.min
