open Containers
module TileSet = Set.Make(struct
  type t = int * int
  let compare = Ord.pair Ord.int Ord.int
end)

let inputs = IO.read_lines_l stdin

let grid =
  inputs |> List.fold_left (fun grid directions ->
    let rec step (x, y) = function
      | 'e' :: tl        -> step (x + 1, y    ) tl
      | 's' :: 'e' :: tl -> step (x    , y - 1) tl
      | 's' :: 'w' :: tl -> step (x - 1, y - 1) tl
      | 'w' :: tl        -> step (x - 1, y    ) tl
      | 'n' :: 'w' :: tl -> step (x    , y + 1) tl
      | 'n' :: 'e' :: tl -> step (x + 1, y + 1) tl
      | [] -> (x, y)
      | _ -> failwith "Unrecognised direction"
    in
    let loc = step (0, 0) (String.to_list directions) in
    match TileSet.find_opt loc grid with
    | Some _ -> TileSet.remove loc grid
    | None -> TileSet.add loc grid
  ) TileSet.empty

let flip grid =
  let is_black (x, y) = TileSet.find_opt (x, y) grid |> Option.is_some in
  let delta = [(1, 0); (0, -1); (-1, -1); (-1, 0); (0, 1); (1, 1)] in
  let check_set = TileSet.fold (fun (x, y) check_set ->
      TileSet.add_list check_set (List.map (fun (dy, dx) -> (x + dx, y + dy)) delta)
    ) grid grid
  in
  TileSet.fold (fun (x, y) grid' ->
    let n_adjacent_black = delta
      |> List.map (fun (dx, dy) -> is_black (x + dx, y + dy))
      |> List.count Fun.id in
    if is_black (x, y) then
      if n_adjacent_black = 0 || n_adjacent_black > 2 then grid'
      else TileSet.add (x, y) grid'
    else
      if n_adjacent_black = 2 then TileSet.add (x, y) grid'
      else grid'
  ) check_set TileSet.empty

let () =
  let part1 = TileSet.cardinal grid in
  let part2 = Seq.(0 --^ 100)
    |> Seq.fold (fun grid _ -> flip grid) grid
    |> TileSet.cardinal
  in
  Printf.printf "%d %d\n" part1 part2
