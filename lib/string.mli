val split_to_pair : char -> string -> string * string
(** [split_to_pair c s] split [s] at the first occurance of [c]. *)

val re_match : string -> string -> bool
(** [re_match re s] check if [s] matches [re]. *)
