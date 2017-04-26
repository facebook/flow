(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val infer_ast:
  metadata: Context.metadata ->
  filename: Loc.filename ->
  Ast.program ->
  require_loc_map: Loc.t SMap.t ->
  Context.t

val infer_lib_file:
  metadata: Context.metadata ->
  exclude_syms:SSet.t ->
  Utils_js.filename ->
  Ast.program ->
  Context.t * string list
