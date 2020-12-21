open Lib
open Containers

module WordSet = Set.Make(String)

let foods = IO.read_lines_l stdin
  |> List.map @@ fun line ->
    MYString.split_to_pair_by ~by:" (contains " line
    |> Pair.map_fst (fun ingredients ->
        String.split_on_char ' ' ingredients |> WordSet.of_list)
    |> Pair.map_snd (fun allergens ->
      String.take (String.length allergens - 1) allergens
      |> String.split ~by:", " |> WordSet.of_list)

let allergens =
  List.fold_left
    (fun set (_, allergens) -> WordSet.union set allergens) WordSet.empty foods

let () =
  (* Each allergen contains possible ingredients *)
  let bad_foods = WordSet.fold (fun allergen out ->
    let bad_ingredients = List.fold_left (fun set (ingredients, allergens) ->
      if WordSet.exists (String.equal allergen) allergens then
        if WordSet.is_empty set then ingredients
        else WordSet.inter set ingredients
      else set
    ) WordSet.empty foods
    in
    (allergen, bad_ingredients) :: out
  ) allergens []
  in
  let rec prune ingredients result =
    if List.is_empty ingredients then
      result
    else
      match List.find_idx (fun (_, ingredients) -> WordSet.cardinal ingredients = 1) ingredients with
      | Some (idx, (allergen, ingredient)) ->
          let ingredients' = MYList.filter_mapi
            (fun i (allergen, ingredients_for_allergen) ->
              if i = idx then None
              else Some (allergen, WordSet.diff ingredients_for_allergen ingredient))
            ingredients in
          prune ingredients' ((allergen, WordSet.choose ingredient) :: result)
      | None -> failwith "Failed pruning"
  in
  let allergen_ingredient_map = prune bad_foods [] in
  let definitely_bad_set = List.fold_left
    (fun set (_, ingredient) -> WordSet.add ingredient set) WordSet.empty allergen_ingredient_map in
  let part1 = foods
    |> List.map (fun (ingredients, _) ->
      WordSet.diff ingredients definitely_bad_set |> WordSet.cardinal)
    |> MYList.sum
  in
  let part2 =
    allergen_ingredient_map
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> List.map (fun (_, x) -> x)
    |> String.concat ","
  in
  Printf.printf "%d\n%s\n" part1 part2
