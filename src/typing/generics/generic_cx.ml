(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections
open Scope_id

type t = {
  marked_ids: Reason.t option IMap.t ScopeMap.t;
  scope_map: ALocIDSet.t ScopeMap.t;
  blame_reasons: Reason.t IMap.t;
}

let in_scope gcx id loc =
  let scope = ScopeMap.find id gcx.scope_map in
  ALocIDSet.mem loc scope

class tvar_finder =
  object (self)
    inherit [Reason.t option IMap.t * Reason.t IMap.t * Reason.t option] Type_visitor.t

    method! tvar cx pole (marked, blame, reason) r id =
      let (root_id, _) = Context.find_constraints cx id in
      if id <> root_id then
        self#tvar cx pole (marked, blame, reason) r root_id
      else
        (* keeps the old reason *)
        ( IMap.add ~combine:Base.Option.first_some id reason marked,
          Base.Option.value_map
            ~default:blame
            ~f:(fun reason -> IMap.add ~combine:(fun fst _ -> fst) id reason blame)
            reason,
          reason
        )
  end

let new_gcx () =
  { marked_ids = ScopeMap.empty; scope_map = ScopeMap.empty; blame_reasons = IMap.empty }

let add_scope_map gcx id scope = { gcx with scope_map = ScopeMap.add id scope gcx.scope_map }

let base_mark_ids_of_type =
  let finder = new tvar_finder in
  fun reason_opt cx gcx ty scope_id ->
    let marked =
      Base.Option.value ~default:IMap.empty (ScopeMap.find_opt scope_id gcx.marked_ids)
    in
    let (marked, blame_reasons, _) =
      finder#type_ cx Polarity.Positive (marked, gcx.blame_reasons, reason_opt) ty
    in
    { gcx with marked_ids = ScopeMap.add scope_id marked gcx.marked_ids; blame_reasons }

let mark_ids_of_type = base_mark_ids_of_type None

let blame_ids_of_type cx gcx ty scope_id reason =
  base_mark_ids_of_type (Some reason) cx gcx ty scope_id

let get_id_annotation_reason { blame_reasons; _ } id = IMap.find_opt id blame_reasons

let get_marked_ids { marked_ids; _ } = marked_ids
