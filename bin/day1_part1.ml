let product_of_2020_pair nums =
  CCList.find_map
    (fun num1 ->
      CCList.find_map
        (* Assume it is fine if the two numbers are the same entry *)
        (fun num2 -> if num1 + num2 = 2020 then Some (num1 * num2) else None)
        nums)
    nums

let () =
  open_in "inputs/day1.txt"
  |> CCIO.read_lines_l
  |> List.map int_of_string
  |> product_of_2020_pair
  |> CCOpt.pp CCFormat.int Format.std_formatter
