(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Module dealing with inheritance.
 * When we want to declare a new class, we first have to retrieve all the
 * types that where inherited from their parents.
 *)
(*****************************************************************************)
open Utils
open Typing_defs
type env = Typing_env.env

type inherited = {
  ih_cstr     : class_elt option * bool;
  ih_consts   : class_elt SMap.t ;
  ih_typeconsts : typeconst_type SMap.t ;
  ih_props    : class_elt SMap.t ;
  ih_sprops   : class_elt SMap.t ;
  ih_methods  : class_elt SMap.t ;
  ih_smethods : class_elt SMap.t ;
}

(* Builds the inherited type *)
val make: Typing_env.env -> Nast.class_ -> env * inherited
