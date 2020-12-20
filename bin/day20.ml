open Lib
open Containers

type tile =
  { id: int;
    top: bool list;
    left: bool list;
    bottom: bool list;
    right: bool list;
    map: bool list list;
    neighbor_top: int option;
    neighbor_left: int option;
    neighbor_bottom: int option;
    neighbor_right: int option;
  }

let parse_tile input =
  let header, tile = List.hd_tl (String.lines input)
    |> Pair.map_snd (List.map (Fun.compose String.to_list (List.map (Char.equal '#'))))
  in
  let id = String.sub header 5 4 |> int_of_string in
  let top, bottom = MYList.take_ends tile in
  let col idx = List.map (List.get_at_idx_exn idx) tile in
  let left = col 0 in
  let right = col 9 in
  let map = MYList.filter_mapi (fun i row ->
    if i = 0 || i = 9 then None
    else Some (List.drop 1 row |> List.take 8)
  ) tile in
  { id; top; left; bottom; right; map;
    neighbor_top=None;
    neighbor_left=None;
    neighbor_bottom=None;
    neighbor_right=None }

let tiles = IO.read_all stdin
  |> String.rtrim
  |> String.split ~by:"\n\n"
  |> List.map parse_tile
  |> Array.of_list

let side_equal = List.equal Bool.equal

let side_matched tile_id side =
  tiles |> Array.exists @@ fun ({ id; top; left; bottom; right; _ }) ->
    if id <> tile_id then
      let sides = [top; left; bottom; right] in
      (List.exists (side_equal side) sides) || (List.exists (side_equal (List.rev side)) sides)
    else false

let _ =
  tiles |> Array.filter_map (fun ({ id; top; left; bottom; right; _ }) ->
      [side_matched id top; side_matched id left; side_matched id bottom; side_matched id right]
      |> List.count Fun.id
      |> function 2 -> Some id | _ -> None)
  |> Array.fold_left ( * ) 1
  |> print_int
