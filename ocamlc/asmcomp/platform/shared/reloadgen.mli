(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

class ['addressing_mode, 'specific_operation] reload_generic : object
  method reload_operation
    : ('addressing_mode, 'specific_operation) Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array * Reg.t array
  method reload_test : Mach.test -> Reg.t array -> Reg.t array
    (* Can be overridden to reflect instructions that can operate
       directly on stack locations *)
  method makereg : Reg.t -> Reg.t
    (* Can be overridden to avoid creating new registers of some class
       (i.e. if all "registers" of that class are actually on stack) *)
  method fundecl : 'f -> int array -> (('addressing_mode, 'specific_operation) Mach.fundecl as 'f) * bool
    (* The entry point *)
end
