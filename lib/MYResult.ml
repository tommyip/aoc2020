open CCResult

let get_err_exn = function
  | Error err -> err
  | Ok _ -> raise Get_error
