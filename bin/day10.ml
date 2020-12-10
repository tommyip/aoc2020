open Lib
open Containers

let ways adapters =
  let length = Array.length adapters in
  let aux aux' i =
    if i = length - 1 then 1
    else
      let x = Array.get adapters i in
      let subways j =
        match Array.get_safe adapters (i + j) with
        | Some y -> if (y - x) <= 3 then aux' (i + j) else 0
        | None -> 0
      in
      subways 1 + subways 2 + subways 3
  in (CCCache.with_cache_rec (CCCache.lru ~eq:(=) length) aux) 0

let () =
  let adapters = IO.read_lines_l stdin
    |> List.map int_of_string
    |> List.sort compare
  in
  let adapters = 0 :: (adapters @ [(List.last_opt adapters |> Option.get_exn) + 3]) in
  let part1 = List.sublists_of_len ~offset:1 2 adapters
    |> List.map (fun lst -> let (a, b) = MYList.to_pair lst in b - a)
    |> fun lst -> (List.count (( = ) 1) lst) * (List.count (( = ) 3) lst)
  in
  let part2 = ways (Array.of_list adapters) in
  Printf.printf "%d %d\n" part1 part2
