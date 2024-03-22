(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

type available_ast =
  | Typed_ast of (ALoc.t, ALoc.t * Type.t) Ast.Program.t
  | ALoc_ast of (ALoc.t, ALoc.t) Ast.Program.t

val typed_ast_of_available_ast : available_ast -> (ALoc.t, ALoc.t * Type.t) Ast.Program.t option

val polarity : 'L Flow_ast.Variance.t option -> Polarity.t

val typed_ast_to_map :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.Program.t ->
  Type.t Loc_collections.ALocMap.t

val typed_ast_to_list :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.Program.t -> (ALoc.t * Type.t) list

val error_mapper : (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper

val placeholder_mapper :
  Context.t -> (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper

val unimplemented_mapper :
  (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper

val unchecked_mapper : (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper

val untyped_ast_mapper :
  (ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t) Flow_polymorphic_ast_mapper.mapper
