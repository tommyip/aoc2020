let find_2020_sum_pair nums =
  CCVector.find_map
    (fun num1 ->
      CCVector.find_map
        (* Assume it is fine if the two numbers are the same entry *)
        (fun num2 -> if num1 + num2 = 2020 then Some (num1, num2) else None)
        nums)
    nums

let () =
  open_in "inputs/day1.txt"
  |> CCIO.read_lines_gen
  |> Gen.map int_of_string
  |> CCVector.of_gen
  |> find_2020_sum_pair
  |> CCOpt.map (fun (x, y) -> x * y)
  |> CCOpt.pp CCFormat.int Format.std_formatter
