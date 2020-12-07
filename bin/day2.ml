open Containers

type entry =
  { lo: int;
    hi: int;
    letter: char;
    password: string
  }

let parse_policy_password input =
  Scanf.sscanf input "%u-%u %c: %s"
    (fun lo hi letter password -> { lo; hi; letter; password })

let is_valid_part1 entry =
  let n = String.filter (Char.equal entry.letter) entry.password
    |> String.length
  in n >= entry.lo && n <= entry.hi

let is_valid_part2 entry =
  not (Bool.equal
    (Char.equal entry.password.[entry.lo - 1] entry.letter)
    (Char.equal entry.password.[entry.hi - 1] entry.letter))

let () =
  let entries =
    IO.read_lines_l stdin
    |> List.map parse_policy_password
  in
  let part1 = entries |> List.filter is_valid_part1 |> List.length in
  let part2 = entries |> List.filter is_valid_part2 |> List.length in
  Printf.printf "%d %d\n" part1 part2
