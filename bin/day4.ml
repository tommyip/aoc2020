let in_range s lo hi =
let n = int_of_string s in
n >= lo && n <= hi

let kvs_valid kvs =
  List.for_all (fun kv ->
    match kv with
    | "byr:", v -> in_range v 1920 2002
    | "iyr:", v -> in_range v 2010 2020
    | "eyr:", v -> in_range v 2020 2030
    | "hgt:", v -> begin
        match CCString.take_drop (String.length v - 2) v with
        | h, "cm" -> in_range h 150 193
        | h, "in" -> in_range h 59 76
        | _ -> false
      end
    | "hcl:", v -> Str.string_match (Str.regexp "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$") v 0
    | "ecl:", v -> List.mem v ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    | "pid:", v -> String.length v = 9 && (int_of_string_opt v |> CCOpt.is_some)
    | _ -> true)
    kvs

let _ =
  let required =
    open_in "inputs/day4.txt"
    |> CCIO.read_all
    |> CCString.split ~by:"\n\n"
    |> List.map (fun entry -> entry
      |> Str.split (Str.regexp "[ \n]")
      |> List.map (CCString.take_drop 4))
    |> List.filter (fun kvs ->
      let n = List.length kvs in
      n = 8 || (n = 7 && not (List.exists (fun (k, _) -> k = "cid:") kvs)))
  in
  let part1 = List.length required in
  let part2 = List.filter kvs_valid required |> List.length in
  Printf.printf "%d %d\n" part1 part2
