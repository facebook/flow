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

val debug_print_last_pos:
  'a -> unit

val fun_decl:
  Nast.fun_ -> unit

val gconst_decl:
  Nast.gconst -> unit

val fun_def:
  Typing_env.env -> 'a -> Nast.fun_ -> unit
val class_def:
  Typing_env.env -> 'a -> Nast.class_ -> unit
val typedef_def:
  Typing_env.env -> string -> (bool * Nast.tparam list * Nast.hint) -> unit

val expr:
  Typing_env.env -> Nast.expr -> Typing_env.env * Typing_defs.ty

val make_param_ty:
  Typing_env.env -> Typing_reason.t -> Nast.fun_param ->
  Typing_env.env * (string option * Typing_defs.ty)

val make_params:
  Typing_env.env -> bool -> int -> Nast.fun_param list ->
  Typing_env.env * int * Typing_defs.fun_params

val type_param:
  Typing_env.env -> Nast.tparam ->
  Typing_env.env * Typing_defs.tparam

val get_implements:
  with_checks:bool ->
  this:Typing_defs.ty ->
  Typing_env.env ->
  Nast.hint ->
  Typing_env.env * (Typing_defs.ty SMap.t * Typing_defs.ty SMap.t)

val get_self_from_c: Typing_env.env -> Nast.class_ -> Typing_defs.ty

val is_visible:
  Typing_env.env ->
  Typing_defs.visibility ->
  Nast.class_id option -> (string * string) option
