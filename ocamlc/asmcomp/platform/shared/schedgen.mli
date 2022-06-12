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

(* Instruction scheduling *)

type ('addressing_mode, 'specific_operation) code_dag_node =
  { instr: ('addressing_mode, 'specific_operation) Linear.instruction;
    delay: int;
    mutable sons: (('addressing_mode, 'specific_operation) code_dag_node * int) list;
    mutable date: int;
    mutable length: int;
    mutable ancestors: int;
    mutable emitted_ancestors: int }

class virtual ['addressing_mode, 'specific_operation] scheduler_generic
  : identity_addressing:'addressing_mode
    -> destroyed_at_oper:(('addressing_mode, 'specific_operation) Mach_intf.instruction_desc -> Reg.t array)
    -> object
  (* Can be overridden by processor description *)
  method virtual oper_issue_cycles : ('addressing_mode, 'specific_operation) Mach.operation -> int
      (* Number of cycles needed to issue the given operation *)
  method virtual oper_latency : ('addressing_mode, 'specific_operation) Mach.operation -> int
      (* Number of cycles needed to complete the given operation *)
  method reload_retaddr_issue_cycles : int
      (* Number of cycles needed to issue a Lreloadretaddr operation *)
  method reload_retaddr_latency : int
      (* Number of cycles needed to complete a Lreloadretaddr operation *)
  method oper_in_basic_block : ('addressing_mode, 'specific_operation) Mach.operation -> bool
      (* Says whether the given operation terminates a basic block *)
  method is_store : ('addressing_mode, 'specific_operation) Mach.operation -> bool
      (* Says whether the given operation is a memory store *)
  method is_load : ('addressing_mode, 'specific_operation) Mach.operation -> bool
      (* Says whether the given operation is a memory load *)
  method is_checkbound : ('addressing_mode, 'specific_operation) Mach.operation -> bool
      (* Says whether the given operation is a checkbound *)
  (* Entry point *)
  method schedule_fundecl : (('addressing_mode, 'specific_operation) Linear.fundecl as 'f) -> 'f
end

val reset : unit -> unit
