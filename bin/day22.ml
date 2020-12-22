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

let is_repeat player history =
  List.exists (CCFQueue.equal ( = ) player) history

let recursive_combat player1 player2 =
  let rec aux player1 player2 player1_history player2_history =
    match Pair.map_same CCFQueue.is_empty (player1, player2) with
    | false, true -> (`Player1, player1)
    | true, false -> (`Player2, player2)
    | _ ->
      if (is_repeat player1 player1_history) || (is_repeat player2 player2_history) then
        (`Player1, player1)
      else
        let card1, player1' = CCFQueue.take_front_exn player1 in
        let card2, player2' = CCFQueue.take_front_exn player2 in
        if (CCFQueue.size player1' >= card1) && (CCFQueue.size player2' >= card2) then begin
          let player1_sub = CCFQueue.to_seq player1' |> Seq.take card1 |> CCFQueue.of_seq in
          let player2_sub = CCFQueue.to_seq player2' |> Seq.take card2 |> CCFQueue.of_seq in
          match aux player1_sub player2_sub [] [] with
          | `Player1, _ ->
              let player1'' = add_cards player1' card1 card2 in
              aux player1'' player2' (player1 :: player1_history) (player2 :: player2_history)
          | `Player2, _ ->
              let player2'' = add_cards player2' card2 card1 in
              aux player1' player2'' (player1 :: player1_history) (player2 :: player2_history)
        end else
          let player1'', player2'' =
            if card1 > card2 then
              (add_cards player1' card1 card2, player2')
            else
              (player1', add_cards player2' card2 card1)
          in aux player1'' player2'' (player1 :: player1_history) (player2 :: player2_history)
  in
  let _, winner = aux player1 player2 [] [] in
  score winner

let () =
  let part1 = combat player1 player2 in
  let part2 = recursive_combat player1 player2 in
  Printf.printf "%d %d\n" part1 part2
