open Lib
open Containers
open Angstrom

let integer = take_while1 (function '0'..'9' -> true | _ -> false) >>| int_of_string
let add = char '+' *> return ( + )
let mul = char '*' *> return ( * )
let paren p = char '(' *> p <* char ')'
let spaced p = char ' ' *> p <* char ' '

let chain_expr expr op =
  let rec chain acc =
    (lift2 (fun f x -> f acc x) op expr >>= chain) <|> return acc
  in expr >>= fun init -> chain init

let basic_expr = fix @@ fun expr ->
  let factor = paren expr <|> integer in
  chain_expr factor (spaced (add <|> mul))

let advanced_expr = fix @@ fun expr ->
  let factor = paren expr <|> integer in
  let term = chain_expr factor (spaced add) in
  chain_expr term (spaced mul)

let () =
  let input = IO.read_lines_l stdin in
  let eval parser = input
    |> List.map (Fun.compose (parse_string ~consume:Consume.All parser) Result.get_exn)
    |> MYList.sum
  in
  Printf.printf "%d %d\n" (eval basic_expr) (eval advanced_expr)
