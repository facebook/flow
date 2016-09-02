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
  module_name: Modulename.t ->
  Spider_monkey_ast.program ->
  Context.t

val infer_lib_file:
  metadata: Context.metadata ->
  exclude_syms:SSet.t ->
  Utils_js.filename ->
  Spider_monkey_ast.Statement.t list ->
  Spider_monkey_ast.Comment.t list ->
  Context.t * string list
