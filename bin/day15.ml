open Containers

let () =
  let start = IO.read_all stdin
    |> String.rtrim
    |> String.split_on_char ','
    |> List.map int_of_string
  in
  let (first, last) = List.take_drop (List.length start - 1) start
    |> Pair.map_snd List.hd
  in

  let nth_number n =
    let tbl = Hashtbl.of_seq (Seq.zip (List.to_seq first) (Seq.(1 -- List.length first))) in
    let rec turn ith recent =
      if ith = n + 1 then recent else
      let next = match Hashtbl.get tbl recent with
      | Some last_nth -> ith - 1 - last_nth
      | None -> 0
      in
      Hashtbl.add tbl recent (ith - 1);
      turn (ith + 1) next
    in
    turn (List.length start + 1) last
  in

  let part1 = nth_number 2020 in
  let part2 = nth_number 30000000 in
  Printf.printf "%d %d\n" part1 part2
