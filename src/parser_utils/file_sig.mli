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
  requires: Loc.t SMap.t;
  module_kind: module_kind;
  type_exports: Loc.t SMap.t;
}

and module_kind =
  | CommonJS of { clobbered: Loc.t option }
  | ES of { named: Loc.t SMap.t; batch: Loc.t SMap.t }

val empty_file_sig: t
val empty_module_sig: module_sig

val program: ast:Loc.t Ast.program -> t
