(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                    Marcell Fischbach, University of Siegen             *)
(*                     Benedikt Meurer, University of Siegen              *)
(*                                                                        *)
(*   Copyright 2011 Lehrstuhl für Compilerbau und Softwareanalyse,        *)
(*     Universität Siegen.                                                *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Linear scan register allocation. *)

val allocate_registers
  : (module Proc_intf.S with type addressing_mode = 'a and type specific_operation = 's)
  -> int array
