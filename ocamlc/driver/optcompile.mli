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

(** Native compilation for .ml and .mli files. *)

val interface: source_file:string -> output_prefix:string -> unit

val implementation
  : (module Platform_intf.S
      with type Arch.addressing_mode = 'a
       and type Arch.specific_operation = 's
    )
   -> backend:(module Backend_intf.S)
   -> start_from:Clflags.Compiler_pass.t
   -> source_file:string -> output_prefix:string -> unit

(** {2 Internal functions} **)

val clambda
  : (module Platform_intf.S
      with type Arch.addressing_mode = 'a
       and type Arch.specific_operation = 's
    )
  -> Compile_common.info ->
  (module Backend_intf.S) ->
  Typedtree.implementation -> unit
(** [clambda info typed] applies the regular compilation pipeline to the
    given typechecked implementation and outputs the resulting files.
*)

val flambda
  : (module Platform_intf.S
      with type Arch.addressing_mode = 'a
       and type Arch.specific_operation = 's
    )
  -> Compile_common.info ->
  (module Backend_intf.S) ->
  Typedtree.implementation -> unit
(** [flambda info backend typed] applies the Flambda compilation pipeline to the
    given typechecked implementation and outputs the resulting files.
*)
