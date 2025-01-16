(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections
module EnvMap = Env_api.EnvMap

(** New Environment:
    New environment maps locs to types using the ssa builder
 **)

type type_entry =
  | TypeEntry of {
      t: Type.t;
      state: Type.t lazy_t ref;
    }

type t = {
  types: type_entry EnvMap.t;
  tparams: (Subst_name.t * Type.typeparam * Type.t) ALocMap.t;
  class_bindings: Type.class_binding ALocMap.t;
  class_stack: ALoc.t list;
  scope_kind: Name_def.scope_kind;
  hint_map: Type.lazy_hint_t ALocMap.t;
  var_info: Env_api.env_info;
  pred_func_map: Type.pred_funcall_info Lazy.t ALocMap.t;
  name_defs: Name_def.env_entries_map;
}

val initialize : t -> Env_api.def_loc_type -> ALoc.t -> type_entry -> t

val update_reason : t -> Env_api.def_loc_type -> ALoc.t -> Reason.t -> t

val find_write : t -> Env_api.def_loc_type -> ALoc.t -> type_entry option

val find_ordinary_write : t -> ALoc.t -> type_entry option

val empty : Name_def.scope_kind -> t

val with_info :
  Name_def.scope_kind ->
  Type.lazy_hint_t ALocMap.t ->
  Env_api.env_info ->
  Type.pred_funcall_info Lazy.t ALocMap.t ->
  Name_def.env_entries_map ->
  t
