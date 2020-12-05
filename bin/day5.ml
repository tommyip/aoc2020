let seat_id specifier =
  let id = "0b" ^ String.map (fun c -> if c = 'F' || c = 'L' then '0' else '1') specifier in
  Scanf.sscanf id "%i" (CCFun.id)

let () =
  let ids = open_in "inputs/day5.txt"
    |> CCIO.read_lines_l
    |> List.map seat_id
    |> List.sort (CCFun.flip compare)
  in
  let part1 = List.hd ids in
  let part2 =
    let rec aux = function
    | a :: b :: _ when a - 2 = b -> a - 1
    | _ :: tl -> aux tl
    | [] -> failwith "No seat for me!"
    in aux ids
  in
  Printf.printf "%d %d\n" part1 part2
