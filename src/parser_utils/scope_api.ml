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
    name: int;
  }
  let mem_loc x t = List.mem x t.locs
end

module Scope = struct
  type t = {
    lexical: bool;
    parent: int option;
    defs: Def.t SMap.t;
    locals: Def.t LocMap.t;
    globals: SSet.t;
  }
end

type info = {
  (* number of distinct name ids *)
  max_distinct: int;
  (* map of scope ids to local scopes *)
  scopes: Scope.t IMap.t
}

let all_uses { scopes; _ } =
  IMap.fold (fun _ scope acc ->
    LocMap.fold (fun use _ uses ->
      use::uses
    ) scope.Scope.locals acc
  ) scopes []

let def_of_use { scopes; _ } use =
  let def_opt = IMap.fold (fun _ scope acc ->
    match acc with
    | Some _ -> acc
    | None -> LocMap.get use scope.Scope.locals
  ) scopes None in
  match def_opt with
  | Some def -> def
  | None -> failwith "missing def"

let use_is_def info use =
  let def = def_of_use info use in
  Def.mem_loc use def

let uses_of_def { scopes; _ } ?(exclude_def=false) def =
  IMap.fold (fun _ scope acc ->
    LocMap.fold (fun use def' uses ->
      if exclude_def && Def.mem_loc use def' then uses
      else if Def.(def.locs = def'.locs) then use::uses else uses
    ) scope.Scope.locals acc
  ) scopes []

let uses_of_use info ?exclude_def use =
  let def = def_of_use info use in
  uses_of_def info ?exclude_def def

let def_is_unused info def =
  uses_of_def info ~exclude_def:true def = []

let scope info scope_id =
  try IMap.find_unsafe scope_id info.scopes with Not_found ->
    failwith ("Scope " ^ (string_of_int scope_id) ^ " not found")

let is_local_use { scopes; _ } use =
  IMap.exists (fun _ scope ->
    LocMap.mem use scope.Scope.locals
  ) scopes

let rec fold_scope_chain info f scope_id acc =
  let s = scope info scope_id in
  let acc = f scope_id s acc in
  match s.Scope.parent with
  | Some parent_id -> fold_scope_chain info f parent_id acc
  | None -> acc
