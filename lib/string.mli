val split_to_pair : char -> string -> string * string
(** [split_to_pair c s] split [s] at the first occurance of [c]. *)

val split_to_pair_by : by:string -> string -> string * string
(** [split_to_pair ~by s] split [s] at the first occurance of [by]. *)

val is_match : string -> string -> bool
(** [is_match re s] check if [s] matches [re]. *)

val matches : string -> string -> string list list
(** [matches re s] return the list of match groups matched by [re] on [s] *)
