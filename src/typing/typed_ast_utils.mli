(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_type_at_pos_annotation :
  (Loc.t, Loc.t * Type.t) Flow_ast.program ->
  Loc.t ->
  (Loc.t * Type.TypeScheme.t) option
