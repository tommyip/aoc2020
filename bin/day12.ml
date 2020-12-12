open Containers
open Complex

let scale z multipler = { re=(z.re *. multipler); im=(z.im *. multipler) }

let () =
  let input = IO.read_lines_l stdin
    |> List.map (String.take_drop 1)
    |> List.map (Pair.map_snd float_of_string)
  in
  let part x =
    let (init, select) =
      if x = 1 then (one, Pair.map_fst)
      else ({ re=10.; im=1. }, Pair.map_snd)
    in input
    |> List.fold_left (fun (loc, dir) (action, value) ->
        match action with
        | "N" -> (loc, dir) |> select (add (scale i value))
        | "S" -> (loc, dir) |> select (add (scale (neg i) value))
        | "E" -> (loc, dir) |> select (add (scale one value))
        | "W" -> (loc, dir) |> select (add (scale (neg one) value))
        | "L" -> (loc, mul dir (pow i (scale one (value /. 90.))))
        | "R" -> (loc, mul dir (pow (neg i) (scale one (value /. 90.))))
        | "F" -> (add loc (scale dir value), dir)
        | _ -> failwith "Unknown command"
      ) (zero, init)
    |> fun ({ re; im }, _) -> Float.abs re +. Float.abs im
  in
  Printf.printf "%.0f %.0f\n" (part 1) (part 2)
