(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Utils_js.LocMap

type scope = int
type use = Loc.t

module Def = struct
  type t = {
    locs: Loc.t list;
    scope: int;
    name: int;
  }
  let mem_loc x t = List.mem x t.locs
end

module Scope = struct
  type t = {
    lexical: bool;
    parent: int option;
  }
end

type info = {
  (* map from identifier locations to their defs and their enclosing scope ids *)
  locals: (Def.t * int) LocMap.t;
  (* map from name ids to globals they conflict with *)
  globals: SSet.t IMap.t;
  (* number of distinct name ids *)
  max_distinct: int;
  (* map of scope ids to local scopes *)
  scopes: Scope.t IMap.t
}

let all_uses { locals; _ } =
  LocMap.fold (fun use _ uses ->
    use::uses
  ) locals []

let def_of_use { locals; _ } use =
  let def, _ = LocMap.find use locals in
  def

let use_is_def info use =
  let def = def_of_use info use in
  Def.mem_loc use def

let uses_of_def { locals; _ } ?(exclude_def=false) def =
  LocMap.fold (fun use (def', _) uses ->
    if exclude_def && Def.mem_loc use def' then uses
    else if Def.(def.locs = def'.locs) then use::uses else uses
  ) locals []

let uses_of_use info ?exclude_def use =
  let def = def_of_use info use in
  uses_of_def info ?exclude_def def

let def_is_unused info def =
  uses_of_def info ~exclude_def:true def = []

let all_defs { locals; _ } =
  LocMap.fold (fun use (def, _) defs ->
    if Def.mem_loc use def then def::defs else defs
  ) locals []

let defs_of_scope info scope =
  let defs = all_defs info in
  List.filter (fun def -> scope = def.Def.scope) defs

let is_local_use { locals; _ } use =
  LocMap.mem use locals
