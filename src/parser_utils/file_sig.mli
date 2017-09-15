(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {
  module_sig: module_sig;
  declare_modules: (Loc.t * module_sig) SMap.t
}

and module_sig = {
  requires: require SMap.t;
  module_kind: module_kind;
  type_exports: Loc.t SMap.t;
}

and require = {
  loc: Loc.t;
  cjs_requires: Loc.t list;
  es_imports: Loc.t list;
}

and module_kind =
  | CommonJS of { clobbered: Loc.t option }
  | ES of { named: Loc.t SMap.t; batch: Loc.t SMap.t }

val empty_file_sig: t
val empty_module_sig: module_sig

val require_loc_map: module_sig -> Loc.t SMap.t

val program: ast:Loc.t Ast.program -> t
