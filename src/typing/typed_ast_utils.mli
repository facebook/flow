(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* It's convenient to use Loc.t here, since this is usually called in direct response to a user
 * query, where the user has provided a concrete location. In contrast, the other functions in this
 * module simply reduce the information available in the typed AST. *)
val find_type_at_pos_annotation :
  (ALoc.t, ALoc.t * Type.t) Flow_ast.program ->
  Loc.t ->
  (Loc.t * Type.TypeScheme.t) option

val typed_ast_to_map :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.program ->
  Type.TypeScheme.t Loc_collections.ALocMap.t

val typed_ast_to_list :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.program ->
  (ALoc.t * Type.TypeScheme.t) list

val error_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
val unimplemented_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
val unchecked_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
val unreachable_mapper: (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
