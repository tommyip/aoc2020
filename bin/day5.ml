open Containers

let seat_id specifier =
  let id = "0b" ^ String.map (fun c -> if Char.equal c 'F' || Char.equal c 'L' then '0' else '1') specifier in
  Scanf.sscanf id "%i" (Fun.id)

let () =
  let ids =
    IO.read_lines_l stdin
    |> List.map seat_id
    |> List.sort (Fun.flip compare)
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
