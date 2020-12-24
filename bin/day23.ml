open Lib
open Containers

(*
let input = "467528193" |> String.to_list |> List.map (Fun.compose Char.to_string int_of_string)

let to_string = Fun.compose (List.map string_of_int) (String.concat "")

let move cups =
  match cups with
  | current :: a :: b :: c :: tl ->
    let three_cups = [a; b; c] in
    let remaining = current :: tl in
    let rec destination value =
      if List.exists (( = ) value) three_cups then
        destination (value - 1)
      else
        let min, max = List.sort compare remaining |> MYList.take_ends in
        if value < min then max
        else if List.exists (( = ) value) remaining then value
        else destination (value - 1)
    in
    let dest = destination (current - 1) in
    let dest_idx, _ = List.find_idx (( = ) dest) remaining |> Option.get_exn in
    let inserted = remaining
      |> List.insert_at_idx (dest_idx + 1) c
      |> List.insert_at_idx (dest_idx + 1) b
      |> List.insert_at_idx (dest_idx + 1) a in
    let hd, tl = List.take_drop 1 inserted in
    tl @ hd
  | _ -> failwith "unreachable"

let () =
  let part1 =
    let result = List.range' 0 100
      |> List.fold_left (fun cups _ -> move cups) input in
    let idx, _ = List.find_idx (( = ) 1) result |> Option.get_exn in
    List.remove_at_idx idx result
    |> List.take_drop idx
    |> fun (b, a) -> a @ b
  in
  print_endline (to_string part1)
*)

let n_cups = 100000
let n_moves = 1000000
let circle =
  let prefix = [4; 6; 7; 5; 2; 8; 1; 9; 3] in
  Array.init n_cups @@ fun i ->
    if i < 10 then List.get_at_idx_exn i prefix
    else i + 1

let _ =
  Seq.(0 --^ n_moves)
  |> Seq.iter @@ fun i ->
    let current_idx = i mod n_cups in
    let current = circle.(current_idx) in
    let cup_a = circle.((current_idx + 1) mod n_cups) in
    let cup_b = circle.((current_idx + 2) mod n_cups) in
    let cup_c = circle.((current_idx + 3) mod n_cups) in
    let destination = if current <= 10 then n_cups else current - 1 in
    let destination_idx, _ = Array.find_idx (( = ) destination) circle |> Option.get_exn in
