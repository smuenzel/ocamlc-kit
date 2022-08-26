
exception Fatal_error

let fatal_errorf fmt =
  Format.kfprintf
    (fun _ -> raise Fatal_error)
    Format.err_formatter
    ("@?>> Fatal error: " ^^ fmt ^^ "@.")

let fatal_error msg = fatal_errorf "%s" msg
