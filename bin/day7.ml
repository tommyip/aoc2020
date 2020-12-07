open Containers

module RulesMap = Map.Make(String)

let parse_rule rule =
  match List.hd (Lib.String.matches "^(.+) bags contain (.+)\\.$" rule) with
  | color :: "no other bags" :: [] -> (color, [])
  | color :: regulations :: [] ->
      let regulations = List.map
        (fun regulation -> Scanf.sscanf regulation "%d %s %s" (fun n c1 c2 -> (n, c1 ^ " " ^ c2)))
        (String.split ~by:", " regulations)
      in (color, regulations)
  | _ -> failwith "Regex failed"

let rec n_bags rules regulations =
  Lib.List.sum
    (List.map (fun (n, color) ->
      match RulesMap.find_opt color rules with
      | Some regulations' -> n + n * (n_bags rules regulations')
      | None -> n) regulations)

let rec can_carry_shiny_gold_bag rules regulations =
  List.exists (fun (_, color) ->
    if String.equal color "shiny gold" then true
    else
      match RulesMap.find_opt color rules with
      | Some regulations' -> can_carry_shiny_gold_bag rules regulations'
      | None -> false)
    regulations

let () =
  let rules = IO.read_lines_l stdin
    |> List.map parse_rule
    |> List.fold_left (fun map (color, regulations) -> RulesMap.add color regulations map) RulesMap.empty
  in
  let part1 =
    RulesMap.filter (fun _ regulations -> can_carry_shiny_gold_bag rules regulations) rules
    |> RulesMap.cardinal
  in
  let part2 = n_bags rules (RulesMap.find "shiny gold" rules) in
  Printf.printf "%d %d\n" part1 part2
