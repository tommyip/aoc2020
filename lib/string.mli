val split_to_pair : char -> string -> string * string
(** [split_to_pair c s] split [s] at the first occurance of [c]. *)

val is_match : string -> string -> bool
(** [is_match re s] check if [s] matches [re]. *)

val matches : string -> string -> Re.Group.t list
(** [matches re s] return the groups matched by [re] on [s] *)
