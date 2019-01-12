(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val find_type_at_pos_annotation :
  (ALoc.t, ALoc.t * Type.t) Flow_ast.program ->
  Loc.t ->
  (Loc.t * Type.TypeScheme.t) option

val typed_ast_to_map :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.program ->
  Type.TypeScheme.t Utils_js.LocMap.t

val typed_ast_to_list :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.program ->
  (Loc.t * Type.TypeScheme.t) list

val error_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
val unimplemented_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
val unchecked_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
val unreachable_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
