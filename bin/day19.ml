open Lib
open Containers
open Angstrom

let rules, messages =
  IO.read_all stdin |> String.rtrim |> MYString.split_to_pair_by ~by:"\n\n"
  |> Pair.map_same (String.split_on_char '\n')

module RulesMap = Map.Make(Int)

type rule
  = Char of char
  | Just of int list
  | Either of int list * int list

let whitespace = take_while (Char.equal ' ')
let integer = take_while1 (function '0'..'9' -> true | _ -> false) >>| int_of_string
let integers = many1 (whitespace *> integer)
let literal = char '"' *> (char 'a' <|> char 'b') <* char '"'

let rule =
  integer <* string ": " >>= fun id ->
  (literal >>| fun c -> (id, Char c)) <|>
  (integers >>= fun option1 ->
    (end_of_input >>| fun () -> (id, Just option1)) <|>
    (string " | " *> integers >>| fun option2 -> (id, Either (option1, option2)))
  )

let rules_map = List.fold_left
  (fun map line ->
    let id, rule = parse_string ~consume:Consume.All rule line |> Result.get_exn in
    RulesMap.add id rule map)
  RulesMap.empty rules

type state
  = Finished
  | Invalid
  | Remaining of string list

let check_rule rules_map input : bool =
  let rec seq input ids : state =
    match ids with
    | [id] -> aux id input
    | [id1; id2] -> begin
        match aux id1 input with
        | Finished | Invalid -> Invalid
        | Remaining rests ->
            let rec aux' rests rests' : state =
              match rests with
              | rest :: tl -> begin match aux id2 rest with
                  | Finished -> Finished
                  | Invalid -> aux' tl rests'
                  | Remaining remaining -> aux' tl (remaining @ rests')
                end
              | [] -> if List.is_empty rests' then Invalid else Remaining rests'
            in aux' rests []
      end
    | [id1; id2; id3] -> begin
        match aux id1 input with
        | Finished | Invalid -> Invalid
        | Remaining rests ->
            let rec aux' rests rests' : state =
              match rests with
              | rest :: tl -> begin match aux id2 rest with
                  | Finished | Invalid -> aux' tl rests'
                  | Remaining remaining -> aux' tl (remaining @ rests')
                end
              | [] -> if List.is_empty rests' then Invalid else Remaining rests'
            in
            match aux' rests [] with
            | Finished | Invalid -> Invalid
            | Remaining rests ->
                let rec aux' rests rests' : state =
                  match rests with
                  | rest :: tl -> begin match aux id3 rest with
                      | Finished -> Finished
                      | Invalid -> aux' tl rests'
                      | Remaining remaining -> aux' tl (remaining @ rests')
                    end
                  | [] -> if List.is_empty rests' then Invalid else Remaining rests'
                in aux' rests []
      end
    | _ -> failwith "More than three rules"
  and aux rule_id input =
    match RulesMap.get rule_id rules_map |> Option.get_exn with
    | Char c ->
        (try
          if Char.equal input.[0] c then
            match String.drop 1 input with
            | "" -> Finished
            | remaining -> Remaining [remaining]
          else Invalid
        with Invalid_argument _ -> Invalid)
    | Just ids -> seq input ids
    | Either (ids1, ids2) -> begin
        match seq input ids1 with
        | Finished -> Finished
        | Invalid -> seq input ids2
        | Remaining pos1 -> begin
            match seq input ids2 with
            | Finished -> Finished
            | Invalid -> Remaining pos1
            | Remaining pos2 -> Remaining (pos1 @ pos2)
          end
      end
  in
  match aux 0 input with
  | Finished -> true
  | _ -> false

let () =
  let part1 = List.map (check_rule rules_map) messages |> List.count Fun.id in

  let new_rules = rules_map
    |> RulesMap.add 8 (Either ([42], [42; 8]))
    |> RulesMap.add 11 (Either ([42; 31], [42; 11; 31]))
  in
  let part2 = List.map (check_rule new_rules) messages |> List.count Fun.id in

  Printf.printf "%d %d\n" part1 part2
