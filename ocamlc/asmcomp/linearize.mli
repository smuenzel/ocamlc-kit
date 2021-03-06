(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Transformation of Mach code into a list of pseudo-instructions. *)
val fundecl
  : (module Proc_intf.S
      with type addressing_mode = 'addressing_mode
       and type specific_operation = 'specific_operation
    )
  -> ('addressing_mode, 'specific_operation) Mach.fundecl
  -> ('addressing_mode, 'specific_operation) Linear.fundecl
