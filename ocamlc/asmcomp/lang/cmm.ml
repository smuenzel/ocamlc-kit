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

include Cmm_intf.T

let typ_void = ([||] : machtype_component array)
let typ_val = [|Val|]
let typ_addr = [|Addr|]
let typ_int = [|Int|]
let typ_float = [|Float|]

(** [machtype_component]s are partially ordered as follows:

      Addr     Float
       ^
       |
      Val
       ^
       |
      Int

  In particular, [Addr] must be above [Val], to ensure that if there is
  a join point between a code path yielding [Addr] and one yielding [Val]
  then the result is treated as a derived pointer into the heap (i.e. [Addr]).
  (Such a result may not be live across any call site or a fatal compiler
  error will result.)
*)

let lub_component comp1 comp2 =
  match comp1, comp2 with
  | Int, Int -> Int
  | Int, Val -> Val
  | Int, Addr -> Addr
  | Val, Int -> Val
  | Val, Val -> Val
  | Val, Addr -> Addr
  | Addr, Int -> Addr
  | Addr, Addr -> Addr
  | Addr, Val -> Addr
  | Float, Float -> Float
  | (Int | Addr | Val), Float
  | Float, (Int | Addr | Val) ->
    (* Float unboxing code must be sure to avoid this case. *)
    assert false

let ge_component comp1 comp2 =
  match comp1, comp2 with
  | Int, Int -> true
  | Int, Addr -> false
  | Int, Val -> false
  | Val, Int -> true
  | Val, Val -> true
  | Val, Addr -> false
  | Addr, Int -> true
  | Addr, Addr -> true
  | Addr, Val -> true
  | Float, Float -> true
  | (Int | Addr | Val), Float
  | Float, (Int | Addr | Val) ->
    assert false

let machtype_of_exttype ~size_int = function
  | XInt -> typ_int
  | XInt32 -> typ_int
  | XInt64 -> if size_int = 4 then [|Int;Int|] else typ_int
  | XFloat -> typ_float

let machtype_of_exttype_list ~size_int xtl =
  Array.concat (List.map (machtype_of_exttype ~size_int) xtl)

let negate_integer_comparison = Lambda.negate_integer_comparison

let swap_integer_comparison = Lambda.swap_integer_comparison

let negate_float_comparison = Lambda.negate_float_comparison

let swap_float_comparison = Lambda.swap_float_comparison

let init_label = 99

let label_counter = ref init_label

let set_label l =
  if (l < !label_counter) then begin
    Misc.fatal_errorf "Cannot set label counter to %d, it must be >= %d"
      l !label_counter ()
  end;
  label_counter := l

let cur_label () = !label_counter

let new_label() = incr label_counter; !label_counter

let ccatch (i, ids, e1, e2, dbg) =
  Ccatch(Nonrecursive, [i, ids, e2, dbg], e1)

let reset () =
  label_counter := init_label

let iter_shallow_tail f = function
  | Clet(_, _, body) | Cphantom_let (_, _, body) | Clet_mut(_, _, _, body) ->
      f body;
      true
  | Cifthenelse(_cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg) ->
      f ifso;
      f ifnot;
      true
  | Csequence(_e1, e2) ->
      f e2;
      true
  | Cswitch(_e, _tbl, el, _dbg') ->
      Array.iter (fun (e, _dbg) -> f e) el;
      true
  | Ccatch(_rec_flag, handlers, body) ->
      List.iter (fun (_, _, h, _dbg) -> f h) handlers;
      f body;
      true
  | Ctrywith(e1, _id, e2, _dbg) ->
      f e1;
      f e2;
      true
  | Cexit _ | Cop (Craise _, _, _) ->
      true
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cop _ ->
      false

let rec map_tail f = function
  | Clet(id, exp, body) ->
      Clet(id, exp, map_tail f body)
  | Clet_mut(id, kind, exp, body) ->
      Clet_mut(id, kind, exp, map_tail f body)
  | Cphantom_let(id, exp, body) ->
      Cphantom_let (id, exp, map_tail f body)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
      Cifthenelse
        (
          cond,
          ifso_dbg, map_tail f ifso,
          ifnot_dbg, map_tail f ifnot,
          dbg
        )
  | Csequence(e1, e2) ->
      Csequence(e1, map_tail f e2)
  | Cswitch(e, tbl, el, dbg') ->
      Cswitch(e, tbl, Array.map (fun (e, dbg) -> map_tail f e, dbg) el, dbg')
  | Ccatch(rec_flag, handlers, body) ->
      let map_h (n, ids, handler, dbg) = (n, ids, map_tail f handler, dbg) in
      Ccatch(rec_flag, List.map map_h handlers, map_tail f body)
  | Ctrywith(e1, id, e2, dbg) ->
      Ctrywith(map_tail f e1, id, map_tail f e2, dbg)
  | Cexit _ | Cop (Craise _, _, _) as cmm ->
      cmm
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cop _ as c ->
      f c

let map_shallow f = function
  | Clet (id, e1, e2) ->
      Clet (id, f e1, f e2)
  | Clet_mut (id, kind, e1, e2) ->
      Clet_mut (id, kind, f e1, f e2)
  | Cphantom_let (id, de, e) ->
      Cphantom_let (id, de, f e)
  | Cassign (id, e) ->
      Cassign (id, f e)
  | Ctuple el ->
      Ctuple (List.map f el)
  | Cop (op, el, dbg) ->
      Cop (op, List.map f el, dbg)
  | Csequence (e1, e2) ->
      Csequence (f e1, f e2)
  | Cifthenelse(cond, ifso_dbg, ifso, ifnot_dbg, ifnot, dbg) ->
      Cifthenelse(f cond, ifso_dbg, f ifso, ifnot_dbg, f ifnot, dbg)
  | Cswitch (e, ia, ea, dbg) ->
      Cswitch (e, ia, Array.map (fun (e, dbg) -> f e, dbg) ea, dbg)
  | Ccatch (rf, hl, body) ->
      let map_h (n, ids, handler, dbg) = (n, ids, f handler, dbg) in
      Ccatch (rf, List.map map_h hl, f body)
  | Cexit (n, el) ->
      Cexit (n, List.map f el)
  | Ctrywith (e1, id, e2, dbg) ->
      Ctrywith (f e1, id, f e2, dbg)
  | Cconst_int _
  | Cconst_natint _
  | Cconst_float _
  | Cconst_symbol _
  | Cvar _
    as c ->
      c
