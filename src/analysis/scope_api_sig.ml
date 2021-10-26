(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module L : Loc_sig.S

  type scope = int

  type use = L.t

  type uses = L.LSet.t

  module Def : sig
    type t = {
      locs: L.t Nel.t;
      name: int;
      actual_name: string;
      kind: Bindings.kind;
    }
    [@@deriving show]

    val compare : t -> t -> int
  end

  module DefMap : WrappedMap_sig.S with type key = Def.t

  type use_def_map = Def.t L.LMap.t [@@deriving show]

  module Scope : sig
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
    max_distinct: int;
    scopes: Scope.t IMap.t;
  }
  [@@deriving show]

  exception Missing_def of info * use

  val toplevel_scopes : scope list

  val scope : info -> scope -> Scope.t

  (* `scope_within info scope_id scope` returns whether `scope` is nested within `scope_id`. *)
  val scope_within : info -> scope -> Scope.t -> bool

  (* List of scopes associated with a loc. The returned list order should be
     based on the scope depth (some nodes such as functions have two scopes
     associated, one for the name and one for the params/body). *)
  val scope_of_loc : info -> L.t -> scope list

  val all_uses : info -> uses

  val defs_of_all_uses : info -> use_def_map

  val uses_of_all_defs : info -> uses DefMap.t

  val def_of_use : info -> use -> Def.t

  val def_of_use_opt : info -> use -> Def.t option

  val use_is_def : info -> use -> bool

  val uses_of_def : info -> ?exclude_def:bool -> Def.t -> uses

  val uses_of_use : info -> ?exclude_def:bool -> use -> uses

  val def_is_unused : info -> Def.t -> bool

  val is_local_use : info -> use -> bool

  val fold_scope_chain : info -> (scope -> Scope.t -> 'a -> 'a) -> scope -> 'a -> 'a

  val build_scope_tree : info -> Scope.t Tree.t

  val compute_free_and_bound_variables : Scope.t Tree.t -> (Def.t SMap.t * SSet.t * SSet.t) Tree.t
end
