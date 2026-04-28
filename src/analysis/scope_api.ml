(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make (L : Loc_sig.S) = struct
  module L = L

  type scope = int

  type use = L.t

  type uses = L.LSet.t

  module Def = struct
    type t = {
      locs: L.t Nel.t;
      name: int;
      actual_name: string;
      kind: Bindings.kind;
    }
    [@@deriving show]

    let compare =
      let rec iter locs1 locs2 =
        match (locs1, locs2) with
        | ([], []) -> 0
        | ([], _) -> -1
        | (_, []) -> 1
        | (loc1 :: locs1, loc2 :: locs2) ->
          let i = L.compare loc1 loc2 in
          if i = 0 then
            iter locs1 locs2
          else
            i
      in
      (fun t1 t2 -> iter (Nel.to_list t1.locs) (Nel.to_list t2.locs))

    let is x t = Nel.exists (L.equal x) t.locs
  end

  module DefMap = WrappedMap.Make (Def)

  type use_def_map = Def.t L.LMap.t [@@deriving show]

  module Scope = struct
    type t = {
      lexical: bool;
      parent: int option;
      defs: Def.t SMap.t;
      locals: use_def_map;
      globals: SSet.t;
      loc: L.t;
    }
    [@@deriving show]
  end

  type info = {
    (* number of distinct name ids *)
    max_distinct: int;
    (* map of scope ids to local scopes *)
    scopes: Scope.t IMap.t;
    flat_use_def: Def.t L.LMap.t; [@opaque]
    flat_def_uses: L.LSet.t DefMap.t; [@opaque]
    use_scope: (int * Scope.t) L.LMap.t; [@opaque]
  }
  [@@deriving show]

  let empty_info =
    {
      max_distinct = 0;
      scopes = IMap.empty;
      flat_use_def = L.LMap.empty;
      flat_def_uses = DefMap.empty;
      use_scope = L.LMap.empty;
    }

  let finalize info =
    let flat_use_def =
      IMap.fold (fun _ scope acc -> L.LMap.union scope.Scope.locals acc) info.scopes L.LMap.empty
    in
    let flat_def_uses =
      L.LMap.fold
        (fun use def def_uses_map ->
          match DefMap.find_opt def def_uses_map with
          | None -> DefMap.add def (L.LSet.singleton use) def_uses_map
          | Some uses -> DefMap.add def (L.LSet.add use uses) def_uses_map)
        flat_use_def
        DefMap.empty
    in
    let use_scope =
      IMap.fold
        (fun scope_id scope acc ->
          L.LMap.fold (fun use _ acc -> L.LMap.add use (scope_id, scope) acc) scope.Scope.locals acc)
        info.scopes
        L.LMap.empty
    in
    { info with flat_use_def; flat_def_uses; use_scope }

  let scope_of_use info use = L.LMap.find_opt use info.use_scope

  let all_uses { scopes; _ } =
    IMap.fold
      (fun _ scope acc ->
        L.LMap.fold (fun use _ uses -> L.LSet.add use uses) scope.Scope.locals acc)
      scopes
      L.LSet.empty

  let defs_of_all_uses info = info.flat_use_def

  let uses_of_all_defs info = info.flat_def_uses

  exception Missing_def of info * use

  let def_of_use_opt info use = L.LMap.find_opt use info.flat_use_def

  let def_of_use info use =
    match def_of_use_opt info use with
    | Some def -> def
    | None -> raise (Missing_def (info, use))

  let use_is_def info use =
    let def = def_of_use info use in
    Def.is use def

  let uses_of_def info ?(exclude_def = false) def =
    match DefMap.find_opt def info.flat_def_uses with
    | None -> L.LSet.empty
    | Some uses ->
      if exclude_def then
        L.LSet.filter (fun use -> not (Def.is use def)) uses
      else
        uses

  let scopes_of_uses_of_def { scopes; _ } def =
    IMap.fold
      (fun scope_id scope acc ->
        if
          L.LMap.exists
            (fun use def' -> (not (Def.is use def')) && Def.compare def def' = 0)
            scope.Scope.locals
        then
          ISet.add scope_id acc
        else
          acc)
      scopes
      ISet.empty

  let uses_of_use info ?exclude_def use =
    try
      let def = def_of_use info use in
      uses_of_def info ?exclude_def def
    with
    | Missing_def _ -> L.LSet.empty

  let def_is_unused info def = L.LSet.is_empty (uses_of_def info ~exclude_def:true def)

  let toplevel_scopes = [0]

  let scope info scope_id =
    try IMap.find scope_id info.scopes with
    | Not_found -> failwith ("Scope " ^ string_of_int scope_id ^ " not found")

  let rec scope_within info scope_id s =
    match s.Scope.parent with
    | None -> false
    | Some p ->
      if p = scope_id then
        true
      else
        scope_within info scope_id (scope info p)

  let scope_of_loc info scope_loc =
    let scopes =
      IMap.fold
        (fun scope_id scope acc ->
          if scope.Scope.loc = scope_loc then
            scope_id :: acc
          else
            acc)
        info.scopes
        []
    in
    List.rev scopes

  let closest_enclosing_scope info loc in_range =
    let (scope_id, _) =
      IMap.fold
        (fun this_scope_id this_scope (prev_scope_id, prev_scope) ->
          if in_range loc this_scope.Scope.loc && in_range this_scope.Scope.loc prev_scope.Scope.loc
          then
            (this_scope_id, this_scope)
          else
            (prev_scope_id, prev_scope))
        info.scopes
        (0, scope info 0)
    in
    scope_id

  let is_local_use info use = L.LMap.mem use info.flat_use_def

  let rec fold_scope_chain info f scope_id acc =
    let s = scope info scope_id in
    let acc = f scope_id s acc in
    match s.Scope.parent with
    | Some parent_id -> fold_scope_chain info f parent_id acc
    | None -> acc
end

module With_Loc = Make (Loc_sig.LocS)
module With_ALoc = Make (Loc_sig.ALocS)
module With_ILoc = Make (Loc_sig.ILocS)
