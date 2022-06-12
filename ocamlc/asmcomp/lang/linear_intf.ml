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

module T = struct

  type label = Cmm.label

  type ('addressing_mode, 'specific_operation) instruction =
    { mutable desc: ('addressing_mode, 'specific_operation) instruction_desc;
      mutable next: ('addressing_mode, 'specific_operation) instruction;
      arg: Reg.t array;
      res: Reg.t array;
      dbg: Debuginfo.t;
      live: Reg.Set.t }

  and ('addressing_mode, 'specific_operation) instruction_desc =
    | Lprologue
    | Lend
    | Lop of ('addressing_mode, 'specific_operation) Mach.operation
    | Lreloadretaddr
    | Lreturn
    | Llabel of label
    | Lbranch of label
    | Lcondbranch of Mach.test * label
    | Lcondbranch3 of label option * label option * label option
    | Lswitch of label array
    | Lentertrap
    | Ladjust_trap_depth of { delta_traps : int; }
    | Lpushtrap of { lbl_handler : label; }
    | Lpoptrap
    | Lraise of Lambda.raise_kind

  type ('addressing_mode, 'specific_operation) fundecl =
    { fun_name: string;
      fun_args: Reg.Set.t;
      fun_body: ('addressing_mode, 'specific_operation) instruction;
      fun_fast: bool;
      fun_dbg : Debuginfo.t;
      fun_tailrec_entry_point_label : label;
      fun_contains_calls: bool;
      fun_num_stack_slots: int array;
      fun_frame_required: bool;
      fun_prologue_required: bool;
    }

end

module type Linear = sig
  include module type of struct include T end

  val has_fallthrough : (_,_) instruction_desc -> bool
  val end_instr: unit -> (_,_) instruction
  val instr_cons:
    ('addressing_mode, 'specific_operation) instruction_desc -> Reg.t array -> Reg.t array -> (('addressing_mode, 'specific_operation) instruction as 'i) -> 'i
  val invert_test: Mach.test -> Mach.test

end
