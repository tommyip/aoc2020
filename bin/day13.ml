open Lib
open Containers

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

let () =
  let n = IO.read_line stdin |> Option.get_exn |> int_of_string in
  let ids =
    IO.read_line stdin |> Option.get_exn |> String.split_on_char ','
    |> List.mapi (fun i id -> Int.of_string id |> Option.map (fun id -> (i, id)))
    |> List.filter_map Fun.id
  in
  let part1 =
    ids
    |> List.map (fun (_, id) -> (id, (n + id - 1) / id * id))
    |> List.sort (fun (_, x) (_, y) -> compare x y)
    |> List.hd
    |> fun (id, earliest) -> id * (earliest - n)
  in
  let part2 =
    let big_m = List.fold_left (fun acc (_, m) -> acc * m) 1 ids in
    ids
    |> List.map (fun (i, id) ->
        let a = (-i) % id in
        let b = big_m / id in
        let b' = multiplicative_inverse b id in
        a * b * b')
    |> MYList.sum
    |> fun x -> x mod big_m
  in
  Printf.printf "%d %d\n" part1 part2
