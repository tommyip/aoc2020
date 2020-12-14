open Lib
open Containers

type op
  = Mem of int * int
  | Mask of int array

let nth_bit size x n = x land (1 lsl ((size - 1) - n)) <> 0 |> Bool.to_int
let bitarray length x = Array.init length (nth_bit length x)
let bitarray_value = Seq.fold (fun acc bit -> acc * 2 + bit) 0

module IntMap = Map.Make(Int)

let () =
  let program = IO.read_lines_l stdin
  |> List.map @@ fun line ->
    let (lhs, rhs) = MYString.split_to_pair_by ~by:" = " line in
    if String.equal lhs "mask" then
      let out = String.to_seq rhs |> Seq.map (function
        | '1' -> 1
        | '0' -> 0
        | 'X' -> -1
        | _ -> failwith "")
      |> Array.of_seq in
      Mask out
    else
      Scanf.sscanf lhs "mem[%d]" (fun loc -> Mem (loc, int_of_string rhs))
  in
  let part1 = List.fold_left (fun (memory, mask) op ->
    match op with
    | Mem (loc, value) ->
        let value_array = bitarray 36 value in
        let result =
          Array.map2 (fun a b -> if b = -1 then a else b) value_array mask
          |> Array.to_seq |> bitarray_value
        in (IntMap.add loc result memory, mask)
    | Mask mask -> (memory, mask)
  ) (IntMap.empty, Array.make 0 0) program
  |> fun (memory, _) -> IntMap.fold (fun _loc value acc -> value + acc) memory 0
  in
  let part2 = List.fold_left (fun (memory, mask) op ->
    match op with
    | Mem (loc, value) ->
        let loc_array = bitarray 36 loc in
        let result = Array.map2 (fun a b -> if b = 0 then a else b) loc_array mask |> Array.to_list in
        let n_x = List.count ((=) (-1)) result in
        let indices = List.foldi (fun indices i bit -> if bit = -1 then i :: indices else indices) [] result in
        let perms = List.cartesian_product (List.replicate n_x [0; 1]) in
        let new_memory =
          List.fold_left (fun memory perm ->
              let address =
                List.fold_left2 (fun address bit idx -> List.set_at_idx idx bit address)
                  result perm indices
                |> List.to_seq |> bitarray_value
              in
              IntMap.add address value memory
            ) memory perms
        in
        (new_memory, mask)
    | Mask mask -> (memory, mask)
  ) (IntMap.empty, Array.make 0 0) program
  |> fun (memory, _) -> IntMap.fold (fun _loc value acc -> value + acc) memory 0
  in
  Printf.printf "%d %d\n" part1 part2
