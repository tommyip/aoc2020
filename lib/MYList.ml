let sum = CCList.fold_left (+) 0

let to_pair l = (CCList.hd l, CCList.get_at_idx_exn 1 l)
let take_ends l = (CCList.hd l, CCList.last_opt l |> Option.get)
