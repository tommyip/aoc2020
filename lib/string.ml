let split_to_pair c s =
  let idx = CCString.index s c in
  (CCString.take idx s, CCString.drop (idx + 1) s)

let split_to_pair_by ~by s =
  let idx = CCString.find ~sub:by s in
  (CCString.take idx s, CCString.drop (idx + (CCString.length by)) s)

let is_match re s = Re.execp (Re.Posix.compile_pat re) s

let matches re s =
  CCList.map
    (CCFun.compose Array.to_list CCList.tl) (* Drop the whole match *)
    (CCList.map
      Re.Group.all
      (Re.all (Re.Posix.compile_pat re) s))
