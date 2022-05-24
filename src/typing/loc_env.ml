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
  tparams: (Subst_name.t * Type.t) ALocMap.t;
  this_types: Type.annotated_or_inferred ALocMap.t;
  super_types: Type.annotated_or_inferred ALocMap.t;
  resolved: ALocSet.t;
  var_info: Env_api.env_info;
}

let update_types ~update ({ types; this_types; super_types; _ } as info) = function
  | Env_api.OrdinaryNameLoc -> { info with types = update types }
  | Env_api.ThisLoc -> { info with this_types = update this_types }
  | Env_api.SuperLoc -> { info with super_types = update super_types }

let initialize info def_loc_kind loc t =
  let update =
    ALocMap.update loc (function
        | Some _ -> failwith (Utils_js.spf "%s already initialized" (Reason.string_of_aloc loc))
        | None -> Some t
        )
  in
  update_types ~update info def_loc_kind

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

let find_write { types; this_types; super_types; _ } def_loc_kind loc =
  let types =
    match def_loc_kind with
    | Env_api.OrdinaryNameLoc -> types
    | Env_api.ThisLoc -> this_types
    | Env_api.SuperLoc -> super_types
  in
  ALocMap.find_opt loc types |> Base.Option.map ~f:TypeUtil.type_t_of_annotated_or_inferred

let find_ordinary_write env loc = find_write env Env_api.OrdinaryNameLoc loc

let empty =
  {
    types = ALocMap.empty;
    this_types = ALocMap.empty;
    super_types = ALocMap.empty;
    var_info = Env_api.empty;
    resolved = ALocSet.empty;
    tparams = ALocMap.empty;
  }

let with_info var_info = { empty with var_info }
