open Lib
open Containers

let console code =
  let module IntSet = Set.Make(Int) in
  let length = List.length code in
  let rec interp acc pc mem =
    if pc = length then Ok acc
    else if IntSet.exists ((=) pc) mem then Error acc
    else
      let mem = IntSet.add pc mem in
      match List.get_at_idx_exn pc code with
      | ("nop", _) -> interp acc (pc + 1) mem
      | ("acc", v) -> interp (acc + v) (pc + 1) mem
      | ("jmp", o) -> interp acc (pc + o) mem
      | _ -> failwith "Unknown instruction"
  in interp 0 0 IntSet.empty

let () =
  let code = IO.read_lines_l stdin
    |> List.map (Fun.compose (MYString.split_to_pair ' ') (Pair.map_snd int_of_string))
  in
  let part1 = MYResult.get_err_exn (console code) in
  let part2 = List.find_mapi (fun i (op, arg) ->
    match op with
    | "nop" -> console (List.set_at_idx i ("jmp", arg) code) |> Result.to_opt
    | "jmp" -> console (List.set_at_idx i ("nop", arg) code) |> Result.to_opt
    | _ -> None
  ) code |> Option.get_exn in
  Printf.printf "%d %d\n" part1 part2
