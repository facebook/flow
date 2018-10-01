(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocMap = Utils_js.LocMap

val find_type_at_pos_annotation :
  (Loc.t, Loc.t * Type.t) Flow_ast.program ->
  Loc.t ->
  (Loc.t * Type.TypeScheme.t) option

val typed_ast_to_map :
  (Loc.t, Loc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.program ->
  Type.TypeScheme.t LocMap.t

val typed_ast_to_list :
  (Loc.t, Loc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.program ->
  (Loc.t * Type.TypeScheme.t) list
