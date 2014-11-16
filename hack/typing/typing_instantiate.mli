(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils
open Typing_defs

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type env   = Typing_env.env
type subst = ty SMap.t

(*****************************************************************************)
(* Builds a substitution out of a list of type parameters and a list of types.
 *
 * Typical use-case: 
 *   class Y<T> { ... }
 *   class X extends Y<int>
 *
 * To build the type of X, we to replace all the occurrences of T in Y by int.
 * The function make_subst, builds the substition (the map associating types
 * to a type parameter name), in this case, it would build the map(T => int).
 *)
(*****************************************************************************)

val make_subst: tparam list -> ty list -> subst
val make_subst_with_this: this:ty -> tparam list -> ty list -> subst

(*****************************************************************************)
(* Primitive instantiating a type.
 * TODO: explain what instantiation is about.
 *)
(*****************************************************************************)

val instantiate     : subst -> env -> ty -> env * ty
val instantiate_ce  : subst -> env -> class_elt -> env * class_elt

val instantiate_ft  : env -> fun_type -> env * fun_type
val instantiate_fun : env -> ty -> Nast.expr list -> env * ty

val instantiate_this : env -> ty -> ty -> env * ty
