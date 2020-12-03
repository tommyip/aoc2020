let is_tree map x y =
  let layer = Array.get map y in
  Bool.to_int (Array.get layer (x mod (Array.length layer)))

let count_trees map (right, down) =
  let rec aux x y acc =
    if y < (Array.length map) then
      aux (x + right) (y + down) (acc + (is_tree map x y))
    else
      acc
  in aux 0 0 0

let () =
  let map = open_in "inputs/day3.txt"
    |> CCIO.read_lines_l
    |> List.map (fun layer -> CCString.to_array layer |> Array.map ((=) '#'))
    |> Array.of_list
  in
  let part1 = count_trees map (3, 1) in
  let part2 = [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
    |> List.map (count_trees map)
    |> List.fold_left ( * ) 1
  in
  Printf.printf "%d %d\n" part1 part2
