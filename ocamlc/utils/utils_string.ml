
include Identifiable.Make(struct
    include String

    let sexp_of_t = [%sexp_of: string]
    let hash = Hashtbl.hash
    let print = Format.pp_print_string
    let output = output_string 
  end)

include String

let for_all f t =
  let len = String.length t in
  let rec loop i =
    i = len || (f t.[i] && loop (i + 1))
  in
  loop 0

let print ppf t =
  Format.pp_print_string ppf t
