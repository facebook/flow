(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val convert_switch :
  placeholder_loc:'loc ->
  'loc ->
  ('loc, 'loc) Flow_ast.Statement.Switch.t ->
  ('loc, 'loc) Flow_ast.Statement.t option
