open Containers

let product_of_2020_pair nums =
  List.find_map
    (fun num1 ->
      List.find_map
        (* Assume it is fine if the two numbers are the same entry *)
        (fun num2 -> if num1 + num2 = 2020 then Some (num1 * num2) else None)
        nums)
    nums
  |> Option.get_exn

let product_of_2020_triple nums =
  List.find_map
    (fun num1 ->
      List.find_map
        (fun num2 ->
          List.find_map
            (fun num3 ->
              if num1 + num2 + num3 = 2020 then Some (num1 * num2 * num3) else None)
            nums)
        nums)
    nums
  |> Option.get_exn

let () =
  let nums =
    IO.read_lines_l stdin
    |> List.map int_of_string
  in
  let part1 = product_of_2020_pair nums in
  let part2 = product_of_2020_triple nums in
  Printf.printf "%d %d\n" part1 part2
