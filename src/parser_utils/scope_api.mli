(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type scope = int
type use = Loc.t
module Def: sig
  type t = {
    loc: Loc.t;
    scope: int;
    name: int;
  }
end
module Scope: sig
  type t = {
    lexical: bool;
    parent: int option;
  }
end
type info = {
  locals: (Def.t * int) Utils_js.LocMap.t;
  globals: SSet.t IMap.t;
  max_distinct: int;
  scopes: Scope.t IMap.t;
}

val all_uses: info -> use list
val def_of_use: info -> use -> Def.t
val use_is_def: info -> use -> bool
val uses_of_def: info -> ?exclude_def:bool -> Def.t -> use list
val uses_of_use: info -> ?exclude_def:bool -> use -> use list
val def_is_unused: info -> Def.t -> bool
val all_defs: info -> Def.t list
val defs_of_scope: info -> scope -> Def.t list
val is_local_use: info -> use -> bool
