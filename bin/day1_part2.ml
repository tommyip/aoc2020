let find_2020_sum_triple nums =
  CCVector.find_map
    (fun num1 ->
      CCVector.find_map
        (fun num2 ->
          CCVector.find_map
            (fun num3 ->
              if num1 + num2 + num3 = 2020 then Some (num1, num2, num3) else None)
            nums)
        nums)
    nums

let () =
  open_in "inputs/day1.txt"
  |> CCIO.read_lines_gen
  |> Gen.map int_of_string
  |> CCVector.of_gen
  |> find_2020_sum_triple
  |> CCOpt.map (fun (x, y, z) -> x * y * z)
  |> CCOpt.pp CCFormat.int Format.std_formatter
