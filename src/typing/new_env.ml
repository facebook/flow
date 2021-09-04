(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Loc_collections

(************************)
(* Helpers **************)
(************************)

let _find_var { Env_api.env_values; _ } loc = ALocMap.find loc env_values

let _find_refi { Env_api.refinement_of_id; _ } = refinement_of_id

let _is_provider { Env_api.providers; _ } = Env_api.Provider_api.is_provider providers

let _find_providers { Env_api.providers; _ } loc =
  Env_api.Provider_api.providers_of_def providers loc
  |> Base.Option.value ~default:[]
  |> Base.List.map ~f:Reason.aloc_of_reason

(************************)
(* Variable Declaration *)
(************************)

let initialize_all cx =
  let ({ Loc_env.var_info; _ } as env) = Context.environment cx in
  Base.List.fold
    ~f:(fun env reason ->
      let loc = aloc_of_reason reason in
      let t = Inferred (Tvar.mk cx reason) in
      (* Treat everything as inferred for now for the purposes of annotated vs inferred *)
      Loc_env.initialize env loc t)
    var_info.Env_api.env_entries
    ~init:env
  |> Context.set_environment cx
