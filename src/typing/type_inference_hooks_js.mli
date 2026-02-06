(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type def =
  | Val of Type.t
  | Parent of Type.t
  | Id

type hook_state_t = {
  id_hook: Context.t -> string -> ALoc.t -> bool;
  literal_hook: Context.t -> ALoc.t -> bool;
  jsx_hook: Context.t -> string -> ALoc.t -> bool;
  obj_prop_decl_hook: Context.t -> string -> ALoc.t -> bool;
  obj_to_obj_hook: Context.t -> Type.t -> Type.t -> unit;
  for_ide: bool;
}

val nop_hook_state : hook_state_t

val hook_state : hook_state_t ref

val set_id_hook : (Context.t -> string -> ALoc.t -> bool) -> unit

val set_literal_hook : (Context.t -> ALoc.t -> bool) -> unit

val set_jsx_hook : (Context.t -> string -> ALoc.t -> bool) -> unit

val set_obj_prop_decl_hook : (Context.t -> string -> ALoc.t -> bool) -> unit

val set_obj_to_obj_hook : (Context.t -> Type.t -> Type.t -> unit) -> unit

val set_for_ide : bool -> unit

val with_for_ide : enabled:bool -> (unit -> 'a) -> 'a

val reset_hooks : unit -> unit

val dispatch_id_hook : Context.t -> string -> ALoc.t -> bool

val dispatch_literal_hook : Context.t -> ALoc.t -> bool

val dispatch_jsx_hook : Context.t -> string -> ALoc.t -> bool

val dispatch_obj_prop_decl_hook : Context.t -> string -> ALoc.t -> bool

val dispatch_obj_to_obj_hook : Context.t -> Type.t -> Type.t -> unit

val is_for_ide : unit -> bool
