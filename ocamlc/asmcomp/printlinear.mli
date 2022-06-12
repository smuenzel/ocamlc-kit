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

(* Pretty-printing of linearized machine code *)

open Format
open Linear

module type S = sig
  type addressing_mode
  type specific_operation
  module Printmach : Printmach.S
    with type addressing_mode = addressing_mode
     and type specific_operation = specific_operation
  val instr: formatter -> (addressing_mode, specific_operation) instruction -> unit
  val fundecl: formatter -> (addressing_mode, specific_operation) fundecl -> unit
end

module Make(Platform : Platform_intf.S)
  : S with type addressing_mode = Platform.Arch.addressing_mode
       and type specific_operation = Platform.Arch.specific_operation
