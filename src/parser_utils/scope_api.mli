(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type scope = int
type use = Loc.t
type uses = Loc.LocSet.t
module Def: sig
  type t = {
    locs: Loc.t list;
    name: int;
    actual_name: string;
  }
end
module Scope: sig
  type t = {
    lexical: bool;
    parent: int option;
    defs: Def.t SMap.t;
    locals: Def.t Utils_js.LocMap.t;
    globals: SSet.t;
  }
end
type info = {
  max_distinct: int;
  scopes: Scope.t IMap.t;
}

val scope: info -> scope -> Scope.t

val all_uses: info -> uses
val def_of_use: info -> use -> Def.t
val use_is_def: info -> use -> bool
val uses_of_def: info -> ?exclude_def:bool -> Def.t -> uses
val uses_of_use: info -> ?exclude_def:bool -> use -> uses
val def_is_unused: info -> Def.t -> bool
val is_local_use: info -> use -> bool
val fold_scope_chain: info -> (scope -> Scope.t -> 'a -> 'a) -> scope -> 'a -> 'a
