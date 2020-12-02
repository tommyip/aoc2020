let product_of_2020_pair nums =
  CCList.find_map
    (fun num1 ->
      CCList.find_map
        (* Assume it is fine if the two numbers are the same entry *)
        (fun num2 -> if num1 + num2 = 2020 then Some (num1 * num2) else None)
        nums)
    nums
  |> CCOpt.get_exn

let product_of_2020_triple nums =
  CCList.find_map
    (fun num1 ->
      CCList.find_map
        (fun num2 ->
          CCList.find_map
            (fun num3 ->
              if num1 + num2 + num3 = 2020 then Some (num1 * num2 * num3) else None)
            nums)
        nums)
    nums
  |> CCOpt.get_exn

let () =
  let nums = open_in "inputs/day1.txt"
    |> CCIO.read_lines_l
    |> List.map int_of_string
  in
  let part1 = product_of_2020_pair nums in
  let part2 = product_of_2020_triple nums in
  Printf.printf "%d %d\n" part1 part2
