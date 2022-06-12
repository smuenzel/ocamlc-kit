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

(* Representation of machine code by sequences of pseudoinstructions *)

include Mach_intf.T

let rec dummy_instr =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty
  }

let end_instr () =
  { desc = Iend;
    next = dummy_instr;
    arg = [||];
    res = [||];
    dbg = Debuginfo.none;
    live = Reg.Set.empty
  }

let instr_cons d a r n =
  { desc = d; next = n; arg = a; res = r;
    dbg = Debuginfo.none; live = Reg.Set.empty
  }

let instr_cons_debug d a r dbg n =
  { desc = d; next = n; arg = a; res = r; dbg = dbg; live = Reg.Set.empty }

let rec instr_iter f i =
  match i.desc with
    Iend -> ()
  | _ ->
      f i;
      match i.desc with
        Iend -> ()
      | Ireturn | Iop Itailcall_ind | Iop(Itailcall_imm _) -> ()
      | Iifthenelse(_tst, ifso, ifnot) ->
          instr_iter f ifso; instr_iter f ifnot; instr_iter f i.next
      | Iswitch(_index, cases) ->
          for i = 0 to Array.length cases - 1 do
            instr_iter f cases.(i)
          done;
          instr_iter f i.next
      | Icatch(_, handlers, body) ->
          instr_iter f body;
          List.iter (fun (_n, handler) -> instr_iter f handler) handlers;
          instr_iter f i.next
      | Iexit _ -> ()
      | Itrywith(body, handler) ->
          instr_iter f body; instr_iter f handler; instr_iter f i.next
      | Iraise _ -> ()
      | _ ->
          instr_iter f i.next

let operation_is_pure = function
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _
  | Iextcall _ | Istackoffset _ | Istore _ | Ialloc _ | Ipoll _
  | Idls_get
  | Iintop(Icheckbound) | Iintop_imm(Icheckbound, _) | Iopaque -> false
  | Ispecific sop -> Arch.operation_is_pure sop
  | _ -> true

let operation_can_raise op =
  match op with
  | Icall_ind | Icall_imm _ | Iextcall _
  | Iintop (Icheckbound) | Iintop_imm (Icheckbound, _)
  | Ialloc _ | Ipoll _ -> true
  | Ispecific sop -> Arch.operation_can_raise sop
  | _ -> false
