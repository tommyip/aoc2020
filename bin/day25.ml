open Lib
open Containers

let a, b = IO.read_all stdin |> String.rtrim |> MYString.split_to_pair '\n'
  |> Pair.map_same int_of_string

let inverse_transform public_key =
  let rec aux value loop_size =
    let value' = (value * 7) mod 20201227 in
    if value' = public_key then loop_size
    else aux value' (loop_size + 1)
  in
  aux 1 1

let transform subject loop_size =
  let rec aux value loop_size =
    if loop_size = 0 then value
    else aux ((value * subject) mod 20201227) (loop_size - 1)
  in aux 1 loop_size

let () =
  let part1 =
    let loop_size = inverse_transform b in
    transform a loop_size
  in
  Printf.printf "%d\n" part1
