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

(* Pretty-printing of pseudo machine code *)

open Format

module type S = sig
  type addressing_mode
  type specific_operation
  val reg: formatter -> Reg.t -> unit
  val regs: formatter -> Reg.t array -> unit
  val regset: formatter -> Reg.Set.t -> unit
  val regsetaddr: formatter -> Reg.Set.t -> unit
  val operation: (addressing_mode, specific_operation) Mach.operation -> Reg.t array -> formatter -> Reg.t array -> unit
  val test: Mach.test -> formatter -> Reg.t array -> unit
  val instr: formatter -> (addressing_mode, specific_operation) Mach.instruction -> unit
  val fundecl: formatter -> (addressing_mode, specific_operation) Mach.fundecl -> unit
  val phase: string -> formatter -> (addressing_mode, specific_operation) Mach.fundecl -> unit
  val interferences: formatter -> unit -> unit
  val intervals: formatter -> unit -> unit
  val preferences: formatter -> unit -> unit
end

module Make(Platform : Platform_intf.S)
  : S with type addressing_mode = Platform.Arch.addressing_mode
     and type specific_operation = Platform.Arch.specific_operation
