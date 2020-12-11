open Containers

let layout = IO.read_lines_l stdin |> List.map String.to_array |> Array.of_list

let bound_check =
  let length = Array.length layout.(0) in
  let height = Array.length layout in
  fun (x, y) -> x >= 0 && x < length && y >= 0 && y < height

let rec visible_neighbor (i, j) (di, dj) current =
  let new_i, new_j = (i + di, j + dj) in
  if bound_check (new_i, new_j) then
    match current.(new_j).(new_i) with
    | '.' -> visible_neighbor (new_i, new_j) (di, dj) current
    | seat -> seat
  else current.(j).(i)

let neighbors ?(check_visible = false) i j current =
  [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
  |> List.filter (fun (di, dj) -> bound_check (i + di, j + dj))
  |> List.map @@ fun (di, dj) ->
     if check_visible then visible_neighbor (i, j) (di, dj) current
     else current.(j + dj).(i + di)

let round ~part current =
  Array.mapi
    (fun j row ->
      Array.mapi
        (fun i seat ->
          let neighbors = neighbors i j current ~check_visible:(part = 2) in
          let tolerance = if part = 1 then 4 else 5 in
          match seat with
          | 'L' when not (List.exists (Char.equal '#') neighbors) -> '#'
          | '#' when List.count (Char.equal '#') neighbors >= tolerance -> 'L'
          | s -> s)
        row)
    current

let rec occupied ~part state =
  let next = round ~part state in
  if Array.equal (Array.equal Char.equal) state next then
    Array.flat_map (Array.filter (Char.equal '#')) state |> Array.length
  else occupied ~part next

let () =
  Printf.printf "%d %d\n" (occupied ~part:1 layout) (occupied ~part:2 layout)
