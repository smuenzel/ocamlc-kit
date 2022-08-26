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

(** Auxiliary AST types used by parsetree and typedtree.

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

type constant =
    Const_int of int
  | Const_char of char
  | Const_string of string * Location.t * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
[@@deriving sexp_of]

type rec_flag = Nonrecursive | Recursive [@@deriving sexp_of]

type direction_flag = Upto | Downto [@@deriving sexp_of]

(* Order matters, used in polymorphic comparison *)
type private_flag = Private | Public [@@deriving sexp_of]

type mutable_flag = Immutable | Mutable [@@deriving sexp_of]

type virtual_flag = Virtual | Concrete [@@deriving sexp_of]

type override_flag = Override | Fresh [@@deriving sexp_of]

type closed_flag = Closed | Open [@@deriving sexp_of]

type label = string [@@deriving sexp_of]

type arg_label =
    Nolabel
  | Labelled of string (** [label:T -> ...] *)
  | Optional of string (** [?label:T -> ...] *)
[@@deriving sexp_of]

type 'a loc = 'a Location.loc = {
  txt : 'a;
  loc : Location.t;
} [@@deriving sexp_of]


type variance =
  | Covariant
  | Contravariant
  | NoVariance
[@@deriving sexp_of]

type injectivity =
  | Injective
  | NoInjectivity
[@@deriving sexp_of]
