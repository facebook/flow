(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
  var_info: Env_api.env_info;
}

let initialize ({ types; _ } as info) loc t =
  let types =
    ALocMap.update
      loc
      (function
        | Some _ -> failwith (Utils_js.spf "%s already initialized" (ALoc.debug_to_string loc))
        | None -> Some t)
      types
  in
  { info with types }

let find_write { types; _ } loc =
  ALocMap.find_opt loc types |> Base.Option.map ~f:TypeUtil.type_t_of_annotated_or_inferred

let empty = { types = ALocMap.empty; var_info = Env_api.empty }
