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

let () =
  open_in "inputs/day1.txt"
  |> CCIO.read_lines_l
  |> List.map int_of_string
  |> product_of_2020_triple
  |> CCOpt.pp CCFormat.int Format.std_formatter
