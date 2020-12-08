val get_err_exn : ('a, 'b) result -> 'b
(** [get_err_exn result] extract the value [x] for [Error x], fails otherwise.
    @raise Get_error if the value is [Ok x] *)
