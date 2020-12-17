open Lib
open Containers

module Coord = struct
  type t = int * int * int

  let compare = Stdlib.compare
  let equal (x1, y1, z1) (x2, y2, z2) = (x1 = x2) && (y1 = y2) && (z1 = z2)
end
module Pocket = Set.Make (Coord)

let initial =
  let input = IO.read_lines_l stdin in
  List.foldi
    (fun pocket y line ->
      List.foldi
        (fun pocket x state ->
          if Char.equal state '#' then Pocket.add (x, y, 0) pocket else pocket)
        pocket (String.to_list line))
    Pocket.empty input

let deltas = List.cartesian_product (List.replicate 3 [-1; 0; 1])
  |> List.filter_map @@ function
    | [x; y; z] when x = 0 && y = 0 && z = 0 -> None
    | [x; y; z] -> Some (x, y, z)
    | _ -> failwith "Unreachable"

let neighbors (x, y, z) =
  deltas |> List.map @@ fun (dx, dy, dz) -> (x + dx, y + dy, z + dz)

let turn prev =
  let is_active coord = Pocket.exists (Coord.equal coord) prev in
  let need_checking = Pocket.fold
    (fun coord set ->
      neighbors coord |> List.fold_left (Fun.flip Pocket.add) set) prev prev in
  Pocket.fold (fun coord current ->
    let active = neighbors coord |> List.count is_active in
    if is_active coord then
      if active = 2 || active = 3 then current
      else Pocket.remove coord current
    else
      if active = 3 then Pocket.add coord current
      else current
  ) need_checking prev

let () =
  Seq.range 0 5 |> Seq.fold (fun prev _ -> turn prev) initial
  |> Pocket.cardinal
  |> print_int
