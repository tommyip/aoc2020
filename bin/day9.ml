open Containers

let preamble_size = 25

let not_valid x preamble =
  not
    (Array.exists
       (fun y ->
         Array.exists (fun z -> if y <> z then y + z = x else false) preamble)
       preamble)

let () =
  let data = IO.read_lines_l stdin |> List.map int_of_string |> Array.of_list in
  let part1 =
    Array.find_map_i
      (fun i x ->
        if i >= preamble_size &&
          not_valid x (Array.sub data (i - preamble_size) preamble_size) then Some x
        else None)
      data
    |> Option.get_exn
  in
  let part2 =
    List.range 2 (Array.length data)
    |> List.find_map (fun length ->
        Array.find_map_i
          (fun i _x ->
            if i + length < Array.length data then
              let set = Array.sub data i length in
              if Array.fold_left ( + ) 0 set = part1 then Some set
              else None
            else None)
          data)
    |> Option.get_exn |> Array.sorted compare
    |> fun arr -> arr.(0) + arr.(Array.length arr - 1)
  in
  Printf.printf "%d %d\n" part1 part2
