open Containers

module Coord = struct
  type t = int list

  let compare = List.compare Int.compare
  let equal = List.equal ( = )
end
module Pocket = Set.Make (Coord)

let input = IO.read_lines_l stdin

let conway dimensions =
  let initial =
    List.foldi
      (fun pocket y line ->
        List.foldi
          (fun pocket x state ->
            if Char.equal state '#' then
              let coord = [x; y] @ (List.replicate (dimensions - 2) 0) in
              Pocket.add coord pocket
            else pocket)
          pocket (String.to_list line))
      Pocket.empty input
  in

  let deltas = List.cartesian_product (List.replicate dimensions [-1; 0; 1])
    |> List.remove_one ~eq:(List.equal (=)) (List.replicate dimensions 0)
  in

  let neighbors coord = List.map (fun delta -> List.map2 ( + ) coord delta) deltas in

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
  in

  Seq.range 0 5
  |> Seq.fold (fun prev _ -> turn prev) initial
  |> Pocket.cardinal

let () =
  Printf.printf "%d %d\n" (conway 3) (conway 4)
