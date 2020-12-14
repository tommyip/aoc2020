open Lib
open Containers

let () =
  let (n, ids) = IO.read_all stdin |> String.rtrim |> MYString.split_to_pair '\n'
    |> Pair.map_fst int_of_string
    |> Pair.map_snd @@ fun line ->
      String.split_on_char ',' line
      |> MYList.filter_mapi (fun i id -> Int.of_string id |> Option.map (fun id -> (i, id)))
  in
  let part1 =
    ids
    |> List.map (fun (_, id) -> (id, (n + id - 1) / id * id))
    |> List.sort (fun (_, x) (_, y) -> compare x y)
    |> List.hd
    |> fun (id, earliest) -> id * (earliest - n)
  in
  let part2 =
    let rec aux t jump ids =
      match ids with
      | (offset, id) :: tl when (t + offset) mod id = 0 ->
          if List.is_empty tl then t
          else
          let jump = jump * id in
          aux (t + jump) jump tl
      | ids ->
          aux (t + jump) jump ids
    in aux 0 1 ids
  in
  Printf.printf "%d %d\n" part1 part2

(* Part 2 solution using Chinese Remainder Theorem

let multiplicative_inverse a m =
  let rec extended_gcd a b =
    if a = 0 then (b, 0, 1)
    else
      let gcd, x1, y1 = extended_gcd (b mod a) a in
      let x = y1 - (b / a * x1) in
      let y = x1 in
      (gcd, x, y)
  in extended_gcd a m |> fun (_, a', _) -> a'

(* Like the % in python *)
let ( % ) x y =
  let result = x mod y in
  if result >= 0 then result else result + y

let part2 ids =
  let big_m = List.fold_left (fun acc (_, m) -> acc * m) 1 ids in
  ids
  |> List.map (fun (i, id) ->
      let a = (-i) % id in
      let b = big_m / id in
      let b' = multiplicative_inverse b id in
      a * b * b')
  |> MYList.sum
  |> fun x -> x mod big_m
*)
