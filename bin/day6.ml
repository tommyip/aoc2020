module CharSet = CCSet.Make(Char)

let () =
  let part =
    let list_of_sets = open_in "inputs/day6.txt"
      |> CCIO.read_all
      |> CCString.rtrim
      |> CCString.split ~by:"\n\n"
      |> List.map (fun input ->
          String.split_on_char '\n' input
          |> List.map (CCString.fold (CCFun.flip CharSet.add) CharSet.empty))
    in fun f ->
      List.map (fun ls -> List.fold_left f (List.hd ls) ls |> CharSet.cardinal) list_of_sets
  in
  let part1 = Lib.List.sum (part CharSet.union) in
  let part2 = Lib.List.sum (part CharSet.inter) in
  Printf.printf "%d %d\n" part1 part2
