open Lib
open Containers

let inputs = IO.read_all stdin |> String.rtrim |> String.split ~by:"\n\n"
let rules =
  List.get_at_idx_exn 0 inputs
  |> String.split_on_char '\n'
  |> List.map @@ fun rule ->
    let _, constrains = MYString.split_to_pair_by ~by:": " rule in
    let c1, c2 = MYString.split_to_pair_by ~by:" or " constrains in
    let range x = MYString.split_to_pair '-' x |> Pair.map_same int_of_string in
    let (c11, c12), (c21, c22) = range c1, range c2 in
    fun x -> (x >= c11 && x <= c12) || (x >= c21 && x <= c22)
let values = Fun.compose (String.split_on_char ',') (List.map int_of_string)
let my_ticket =
  List.get_at_idx_exn 1 inputs
  |> String.split_on_char '\n' |> List.get_at_idx_exn 1 |> values
let nearby_tickets =
  List.get_at_idx_exn 2 inputs
  |> String.split_on_char '\n' |> List.tl |> List.map values

let part1 =
  List.flatten nearby_tickets
  |> List.filter (fun value -> List.for_all (fun rule -> not (rule value)) rules)
  |> MYList.sum

let part2 =
  let valid_tickets =
    List.filter
      (fun ticket ->
        List.for_all (fun value -> List.exists (fun rule -> rule value) rules) ticket)
      nearby_tickets
    |> List.map Array.of_list |> Array.of_list
  in
  let n_values = Array.length valid_tickets.(0) in
  let bag_of_rules =
    List.range' 0 n_values
    |> List.map @@ fun bag_id ->
      let col = Array.map (fun ticket -> ticket.(bag_id)) valid_tickets in
      let rules = MYList.filter_mapi
        (fun i rule -> if Array.for_all (fun value -> rule value) col then Some i else None)
        rules
      in
      (bag_id, rules)
  in
  let rec deduce bag_of_rules deduced =
    if List.is_empty bag_of_rules then deduced
    else
      let bag_idx, (bag_id, rules) =
        List.find_idx (fun (_, rules) -> List.length rules = 1) bag_of_rules
        |> Option.get_exn
      in
      let rule_idx = List.hd rules in
      let bags = List.remove_at_idx bag_idx bag_of_rules in
      deduce
        (List.map
           (Pair.map_snd (fun rules ->
                List.remove_one ~eq:( = ) rule_idx rules))
           bags)
        ((bag_id, rule_idx) :: deduced)
  in
  let deduced = deduce bag_of_rules [] in
  List.sort (fun (a, _) (b, _) -> compare a b) deduced
  |> List.map2
       (fun value (_, rule_idx) -> if rule_idx < 6 then Some value else None)
       my_ticket
  |> List.filter_map Fun.id |> List.fold_left ( * ) 1

let () =
  Printf.printf "%d %d\n" part1 part2
