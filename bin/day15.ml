open Containers

let start =
  IO.read_all stdin |> String.rtrim |> String.split_on_char ','
  |> List.map int_of_string

let start_less_last, last =
  List.take_drop (List.length start - 1) start |> Pair.map_snd List.hd

let nth n =
  let tbl = Array.make n 0 in
  List.iteri (fun i num -> tbl.(num) <- i + 1) start_less_last;
  let rec turn ith recent =
    if ith = n + 1 then recent
    else
      let next = match tbl.(recent) with
        | 0 -> 0
        | last_ith -> ith - 1 - last_ith
      in
      tbl.(recent) <- ith - 1;
      turn (ith + 1) next
  in turn (List.length start + 1) last

let part1 = nth 2020
let part2 = nth 30000000

let () = Printf.printf "%d %d\n" part1 part2
