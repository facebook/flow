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

type t = {
  types: Type.annotated_or_inferred ALocMap.t;
  tparams: (Subst_name.t * Type.typeparam * Type.t) ALocMap.t;
  function_or_global_this_types: Type.annotated_or_inferred ALocMap.t;
  class_instance_this_types: Type.annotated_or_inferred ALocMap.t;
  class_static_this_types: Type.annotated_or_inferred ALocMap.t;
  class_instance_super_types: Type.annotated_or_inferred ALocMap.t;
  class_static_super_types: Type.annotated_or_inferred ALocMap.t;
  array_provider_types: Type.t ALocMap.t;
  class_bindings: Type.class_binding ALocMap.t;
  class_stack: ALoc.t list;
  return_hint: Type.t Hint_api.hint;
  scope_kind: Scope.var_scope_kind;
  resolved: ALocSet.t;
  var_info: Env_api.env_info;
}

let update_types
    ~update
    ( {
        types;
        function_or_global_this_types;
        class_instance_this_types;
        class_static_this_types;
        class_instance_super_types;
        class_static_super_types;
        _;
      } as info
    ) = function
  | Env_api.OrdinaryNameLoc -> { info with types = update types }
  | Env_api.FunctionOrGlobalThisLoc ->
    { info with function_or_global_this_types = update function_or_global_this_types }
  | Env_api.ClassInstanceThisLoc ->
    { info with class_instance_this_types = update class_instance_this_types }
  | Env_api.ClassStaticThisLoc ->
    { info with class_static_this_types = update class_static_this_types }
  | Env_api.ClassInstanceSuperLoc ->
    { info with class_instance_super_types = update class_instance_super_types }
  | Env_api.ClassStaticSuperLoc ->
    { info with class_static_super_types = update class_static_super_types }

let initialize info def_loc_kind loc t =
  let update =
    ALocMap.update loc (function
        | Some _ -> failwith (Utils_js.spf "%s already initialized" (Reason.string_of_aloc loc))
        | None -> Some t
        )
  in
  update_types ~update info def_loc_kind

let initialize_array_provider info loc t =
  {
    info with
    array_provider_types =
      ALocMap.update
        loc
        (function
          | Some _ -> failwith (Utils_js.spf "%s already initialized" (Reason.string_of_aloc loc))
          | None -> Some t)
        info.array_provider_types;
  }

let update_reason ({ types; _ } as info) loc reason =
  let f _ = reason in
  let types =
    ALocMap.update
      loc
      (function
        | Some (Type.Annotated t) -> Some (Type.Annotated (TypeUtil.mod_reason_of_t f t))
        | Some (Type.Inferred t) -> Some (Type.Inferred (TypeUtil.mod_reason_of_t f t))
        | None -> failwith "Cannot update reason on non-existent entry")
      types
  in
  { info with types }

let find_write
    {
      types;
      function_or_global_this_types;
      class_instance_this_types;
      class_static_this_types;
      class_instance_super_types;
      class_static_super_types;
      _;
    }
    def_loc_kind
    loc =
  let types =
    match def_loc_kind with
    | Env_api.OrdinaryNameLoc -> types
    | Env_api.FunctionOrGlobalThisLoc -> function_or_global_this_types
    | Env_api.ClassInstanceThisLoc -> class_instance_this_types
    | Env_api.ClassStaticThisLoc -> class_static_this_types
    | Env_api.ClassInstanceSuperLoc -> class_instance_super_types
    | Env_api.ClassStaticSuperLoc -> class_static_super_types
  in
  ALocMap.find_opt loc types |> Base.Option.map ~f:TypeUtil.type_t_of_annotated_or_inferred

let find_ordinary_write env loc = find_write env Env_api.OrdinaryNameLoc loc

let find_array_provider env loc = ALocMap.find_opt loc env.array_provider_types

let empty scope_kind =
  {
    types = ALocMap.empty;
    function_or_global_this_types = ALocMap.empty;
    class_instance_this_types = ALocMap.empty;
    class_static_this_types = ALocMap.empty;
    class_instance_super_types = ALocMap.empty;
    class_static_super_types = ALocMap.empty;
    array_provider_types = ALocMap.empty;
    var_info = Env_api.empty;
    resolved = ALocSet.empty;
    tparams = ALocMap.empty;
    return_hint = Hint_api.Hint_None;
    class_bindings = ALocMap.empty;
    class_stack = [];
    scope_kind;
  }

let with_info scope_kind var_info =
  let env = empty scope_kind in
  { env with var_info }
