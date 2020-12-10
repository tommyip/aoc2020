open CCList

type 'a gen = unit -> 'a option

let index_list l indices =
  List.map (CCFun.flip get_at_idx_exn l) indices

let permutations ?r l =
  let n = length l in
  let r = CCOpt.get_or ~default:n r in
  let indices =
    (0 --^ n) |> replicate r |> cartesian_product
    |> sort (compare (Int.compare))
    |> ref
  in
  let rec aux () =
    if r > n then None
    else match !indices with
    | hd :: tl -> begin
        indices := tl;
        if length (sort_uniq ~cmp:Int.compare hd) = r
        then Some (index_list l hd)
        else aux ()
      end
    | [] -> None
  in aux

let combinations r l =
  let n = List.length l in
  let indices_gen = permutations ~r (0 --^ n) in
  let rec aux () =
    match indices_gen () with
    | Some indices ->
        if List.sort Int.compare indices = indices
        then Some (index_list l indices)
        else aux ()
    | None -> None
  in aux
