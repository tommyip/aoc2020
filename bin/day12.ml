open Containers

let rad d = d *. Float.pi /. 180.

let rotate (x, y) dr =
  let original_r = atan2 y x in
  let new_r = original_r +. dr in
  let length = sqrt ((x ** 2.) +. (y ** 2.)) in
  (length *. cos new_r, length *. sin new_r)

let () =
  let input = IO.read_lines_l stdin
    |> List.map (String.take_drop 1)
    |> List.map (Pair.map_snd float_of_string)
  in
  let part1 = input
  |> List.fold_left (fun (x, y, dir) (action, value) ->
    match action with
    | "N" -> (x, y +. value, dir)
    | "S" -> (x, y -. value, dir)
    | "E" -> (x +. value, y, dir)
    | "W" -> (x -. value, y, dir)
    | "L" -> (x, y, dir +. (rad value))
    | "R" -> (x, y, dir -. (rad value))
    | "F" -> ((cos dir) *. value +. x, (sin dir) *. value +. y, dir)
    | _ -> failwith ""
    ) (0., 0., 0.)
    |> fun (x, y, _) -> Float.abs x +. Float.abs y
  in

  let part2 = input
  |> List.fold_left (fun (x, y, wx, wy) (action, value) ->
    match action with
    | "N" -> (x, y, wx, wy +. value)
    | "S" -> (x, y, wx, wy -. value)
    | "E" -> (x, y, wx +. value, wy)
    | "W" -> (x, y, wx -. value, wy)
    | "L" -> let (wx, wy) = rotate (wx, wy) (rad value) in (x, y, wx, wy)
    | "R" -> let (wx, wy) = rotate (wx, wy) (rad (-. value)) in (x, y, wx, wy)
    | "F" -> (x +. wx *. value, y +. wy *. value, wx, wy)
    | _ -> failwith ""
    ) (0., 0., 10., 1.)
    |> fun (x, y, _, _) -> Float.abs x +. Float.abs y
  in

  Printf.printf "%.0f %.0f\n" part1 part2
