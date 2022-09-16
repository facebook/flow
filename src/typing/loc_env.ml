(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** New Environment:
    New environment maps locs to types using the ssa builder


 **)

open Loc_collections
module EnvMap = Env_api.EnvMap
module EnvSet = Env_api.EnvSet

type t = {
  types: Type.t EnvMap.t;
  tparams: (Subst_name.t * Type.typeparam * Type.t) ALocMap.t;
  class_bindings: Type.class_binding ALocMap.t;
  class_stack: ALoc.t list;
  declare_module_exports_write_loc: ALoc.t option;
  scope_kind: Name_def.scope_kind;
  readable: EnvSet.t;
  under_resolution: EnvSet.t;
  hint_map: Type.lazy_hint_t ALocMap.t;
  var_info: Env_api.env_info;
}

let initialize info def_loc_kind loc t =
  let types =
    EnvMap.update
      (def_loc_kind, loc)
      (function
        | Some _ -> failwith (Utils_js.spf "%s already initialized" (Reason.string_of_aloc loc))
        | None -> Some t)
      info.types
  in
  { info with types }

let update_reason ({ types; _ } as info) def_loc_kind loc reason =
  let f _ = reason in
  let types =
    EnvMap.update
      (def_loc_kind, loc)
      (function
        | Some t -> Some (TypeUtil.mod_reason_of_t f t)
        | None -> failwith "Cannot update reason on non-existent entry")
      types
  in
  { info with types }

let is_readable { readable; _ } def_loc_kind loc = EnvSet.mem (def_loc_kind, loc) readable

let find_write { types; _ } def_loc_kind loc = EnvMap.find_opt (def_loc_kind, loc) types

let find_ordinary_write env loc = find_write env Env_api.OrdinaryNameLoc loc

let empty scope_kind =
  {
    types = EnvMap.empty;
    var_info = Env_api.empty;
    tparams = ALocMap.empty;
    class_bindings = ALocMap.empty;
    class_stack = [];
    declare_module_exports_write_loc = None;
    scope_kind;
    readable = EnvSet.empty;
    hint_map = ALocMap.empty;
    under_resolution = EnvSet.empty;
  }

let with_info scope_kind hint_map var_info =
  let env = empty scope_kind in
  { env with hint_map; var_info }
