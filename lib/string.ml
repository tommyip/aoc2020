let split_to_pair c s =
  let idx = CCString.index s c in
  (CCString.take idx s, CCString.drop (idx + 1) s)

let is_match re s = Re.execp (Re.Posix.compile_pat re) s

let matches re s = Re.all (Re.Posix.compile_pat re) s
