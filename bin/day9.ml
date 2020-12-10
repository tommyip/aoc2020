open Lib
open Containers

let preamble_size = 25

let first_last lst =
  (List.hd lst, Option.get_exn (List.last_opt lst))

let valid x preamble =
  MYItertools.combinations 2 preamble
  |> Gen.exists @@ fun comb -> (Pair.fold ( + ) (first_last comb)) = x

let () =
  let data = IO.read_lines_l stdin |> List.rev_map int_of_string in
  let part1 =
    List.sublists_of_len ~offset:1 (preamble_size + 1) data
    |> List.find_map (Fun.compose List.hd_tl @@ fun (x, preamble) ->
        if valid x preamble then None else Some x)
    |> Option.get_exn
  in
  let part2 =
    List.range 2 (List.length data)
    |> List.find_map (fun length ->
        List.sublists_of_len ~offset:1 length data
        |> List.find_opt (Fun.compose MYList.sum (( = ) part1)))
    |> Option.get_exn |> List.sort compare
    |> first_last |> Pair.fold ( + )
  in
  Printf.printf "%d %d\n" part1 part2
