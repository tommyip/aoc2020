module CharMap = Map.Make(Char)

let map_input =
  CCString.fold
    (fun map question ->
      CharMap.update question (function Some x -> Some (x + 1) | None -> Some 1) map)
    CharMap.empty

let () =
  let map = open_in "inputs/day6.txt"
    |> CCIO.read_all
    |> CCString.split ~by:"\n\n"
    |> List.map (fun input ->
        let group = CCString.replace ~sub:"\n" ~by:"" input in
        (map_input group, List.length (CCString.lines input)))
  in
  let part1 = List.map (fun (m, _) -> CharMap.cardinal m) map in
  let part2 = List.map (fun (m, n) -> CharMap.fold (fun _ x acc -> if x = n then acc + 1 else acc) m 0) map in
  Printf.printf "%d %d\n" (Lib.List.sum part1) (Lib.List.sum part2)
