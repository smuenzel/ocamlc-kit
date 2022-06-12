
type t =
  | Amd64

let of_string = function
  | "amd64" -> Amd64
  | other -> failwith (Printf.sprintf "unknown platform '%s'" other)

module Amd64 = struct
  open Arch_amd64
  module Arch = Arch
  module CSE = CSE
  module Emit = Emit
  module Proc = Proc
  module Scheduling = Scheduling
  module Selection = Selection
  module Reload = Reload
end

let platform : t -> (module Platform_intf.S
                      with type Arch.addressing_mode = 'a
                       and type Arch.specific_operation = 's)
  = function
  | Amd64 -> (module Amd64)


let default_platform = Amd64

let read_environment () =
  try
    let s = Sys.getenv "OCAMLPLATFORM" in
    if s <> ""
    then of_string s
    else default_platform
  with
  | Not_found -> default_platform
