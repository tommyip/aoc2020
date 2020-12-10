val sum : int list -> int
(** [sum lst] sums [lst], duh! *)

val to_pair : 'a list -> 'a * 'a
(** [to_pair l] converts a list of length 2 to a pair. *)

val take_ends : 'a list -> 'a * 'a
(** [take_ends l] returns a pair containing the first and last element of [l] *)
