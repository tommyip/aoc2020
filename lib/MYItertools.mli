type 'a gen = unit -> 'a option

val permutations : ?r:int -> 'a list -> 'a list gen
(** [permutations ~r l] return successive [r] length permutations of elements
    in [l]. If [r] is not specified, then [r] defaults to the length of [l].

    The permutations are emitted in lexicographic ordering according to the
    order of [l]. Uniqueness of elements are based on the elements' position. *)

val combinations : int -> 'a list -> 'a list gen
(** [combinations r l] return [r] length subsequences of elements from [l].

    The combinations are emitted in lexicographic ordering according to the
    order of [l]. Uniqueness of elements are based on the elements' position. *)
