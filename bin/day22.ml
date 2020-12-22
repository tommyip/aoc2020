open Lib
open Containers

let (player1, player2) =
  IO.read_all stdin |> String.rtrim |> MYString.split_to_pair_by ~by:"\n\n"
  |> Pair.map_same @@ fun deck ->
    String.lines deck |> List.tl |> List.map int_of_string
    |> CCFQueue.of_list

let score cards =
  let multipliers = CCList.range (CCFQueue.size cards) 1 in
  let values = CCFQueue.to_list cards in
  List.fold_left2 (fun acc multiplier value -> acc + multiplier * value) 0 multipliers values

let add_cards player card1 card2 =
  let player = CCFQueue.snoc player card1 in
  CCFQueue.snoc player card2

let rec combat player1 player2 =
  match Pair.map_same CCFQueue.is_empty (player1, player2) with
  | false, true -> score player1
  | true, false -> score player2
  | _ ->
    let card1, player1 = CCFQueue.take_front_exn player1 in
    let card2, player2 = CCFQueue.take_front_exn player2 in
    let player1', player2' =
      if card1 > card2 then
        (add_cards player1 card1 card2, player2)
      else
        (player1, add_cards player2 card2 card1)
    in combat player1' player2'

let () =
  let part1 = combat player1 player2 in
  Printf.printf "%d\n" part1
