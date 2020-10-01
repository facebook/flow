(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections
open Scope_id

type t = {
  marked_ids: ISet.t ScopeMap.t;
  scope_map: ALocIDSet.t ScopeMap.t;
}

let in_scope gcx id loc =
  let scope = ScopeMap.find id gcx.scope_map in
  ALocIDSet.mem loc scope

class tvar_finder =
  object (self)
    inherit [ISet.t] Type_visitor.t

    method! tvar cx pole marked r id =
      let (root_id, _) = Context.find_constraints cx id in
      if id <> root_id then
        self#tvar cx pole marked r root_id
      else
        ISet.add id marked
  end

let new_gcx () = { marked_ids = ScopeMap.empty; scope_map = ScopeMap.empty }

let add_scope_map gcx id scope = { gcx with scope_map = ScopeMap.add id scope gcx.scope_map }

let mark_ids_of_type =
  let finder = new tvar_finder in
  fun cx gcx ty scope_id ->
    let marked =
      Base.Option.value ~default:ISet.empty (ScopeMap.find_opt scope_id gcx.marked_ids)
    in
    let marked = finder#type_ cx Polarity.Positive marked ty in
    { gcx with marked_ids = ScopeMap.add scope_id marked gcx.marked_ids }

let get_marked_ids { marked_ids; _ } = marked_ids
