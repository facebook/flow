(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class type_parameter_mapper :
  object
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    method on_loc_annot : ALoc.t -> ALoc.t

    method on_type_annot : ALoc.t * Type.t -> ALoc.t * Type.t

    method annot_with_tparams : 'a. ((ALoc.t * string) list -> 'a) -> 'a
  end

val find_exact_match_annotation :
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t -> ALoc.t -> Type.TypeScheme.t option
(**
 * Return the first typed AST entry that exactly matches the (abstract) location
 * passed as input.
 *
 *)

val find_type_at_pos_annotation :
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t -> Loc.t -> (Loc.t * Type.TypeScheme.t) option
(**
 * Find the first typed AST entry for "type-at-pos" related queries. A query
 * succeeds if the location is within the range of a symbol in the AST. The kinds
 * of symbols handled here are:
 *  - identifiers
 *  - literal object keys
 *  - `this`, `super`
 *  - private property names
 *
 * The first part of the return is the full span of the matching symbol.
 *
 * It's convenient to use Loc.t as the input query, since this is usually called
 * in direct response to a client query, which are typically concrete locations.
 *)

val typed_ast_to_map :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.Program.t ->
  Type.TypeScheme.t Loc_collections.ALocMap.t

val typed_ast_to_list :
  (ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.Program.t ->
  (ALoc.t * Type.TypeScheme.t) list

val error_mapper : (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper

val unimplemented_mapper :
  (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper

val unchecked_mapper : (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper

val unreachable_mapper :
  (ALoc.t, ALoc.t, ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.mapper
