open Containers

module CharSet = Set.Make(Char)

let () =
  let part =
    let list_of_sets =
      IO.read_all stdin
      |> String.rtrim
      |> String.split ~by:"\n\n"
      |> List.map (fun input ->
          String.split_on_char '\n' input
          |> List.map (String.fold (Fun.flip CharSet.add) CharSet.empty))
    in fun f ->
      list_of_sets
      |> List.map (fun ls -> List.fold_left f (List.hd ls) ls |> CharSet.cardinal)
      |> Lib.List.sum
  in
  Printf.printf "%d %d\n" (part CharSet.union) (part CharSet.inter)
