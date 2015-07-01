(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val with_expr_hook:
  (Nast.expr -> Typing_defs.locl Typing_defs.ty -> unit) -> (unit -> 'a) -> 'a

val debug_print_last_pos:
  'a -> unit

val fun_decl:
  Naming.env -> Nast.fun_ -> unit

val gconst_decl:
  TypecheckerOptions.t -> Nast.gconst -> unit

val fun_def:
  Typing_env.env -> Naming.env -> 'a -> Nast.fun_ -> unit
val class_def:
  Typing_env.env -> Naming.env -> 'a -> Nast.class_ -> unit
val typedef_def:
  Typing_env.env -> Nast.typedef -> unit

val expr:
  Typing_env.env -> Nast.expr ->
  Typing_env.env * Typing_defs.locl Typing_defs.ty

val ret_from_fun_kind: Pos.t -> Ast.fun_kind -> Typing_defs.decl Typing_defs.ty

val make_param_ty:
  Typing_env.env -> Typing_reason.t -> Nast.fun_param ->
  Typing_env.env * (string option * Typing_defs.decl Typing_defs.ty)

val make_params:
  Typing_env.env -> bool -> int -> Nast.fun_param list ->
  Typing_env.env * int * Typing_defs.decl Typing_defs.fun_params

val type_param:
  Typing_env.env -> Nast.tparam ->
  Typing_env.env * Typing_defs.tparam

val get_self_from_c:
  Typing_env.env -> Nast.class_ ->
  Typing_defs.decl Typing_defs.ty

val is_visible:
  Typing_env.env ->
  Typing_defs.visibility ->
  Nast.class_id option -> (string * string) option
