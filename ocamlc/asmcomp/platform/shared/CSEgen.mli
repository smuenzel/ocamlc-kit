(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common subexpression elimination by value numbering over extended
   basic blocks. *)

type op_class =
  | Op_pure     (* pure, produce one result *)
  | Op_checkbound     (* checkbound-style: no result, can raise an exn *)
  | Op_load of Asttypes.mutable_flag  (* memory load *)
  | Op_store of bool  (* memory store, false = init, true = assign *)
  | Op_other   (* anything else that does not allocate nor store in memory *)

class ['addressing_mode, 'specific_operation] cse_generic : (module Proc_intf.S with type specific_operation = 'specific_operation and type addressing_mode = 'addressing_mode) -> object
  (* The following methods can be overridden to handle processor-specific
     operations. *)

  method class_of_operation: ('addressing_mode,'specific_operation) Mach.operation -> op_class

  method is_cheap_operation: ('addressing_mode,'specific_operation) Mach.operation -> bool
    (* Operations that are so cheap that it isn't worth factoring them. *)

  (* The following method is the entry point and should not be overridden *)
  method fundecl: (('addressing_mode,'specific_operation) Mach.fundecl as 'f) -> 'f

end
