(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val type_at_pos :
  cx:Context.t ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  omit_targ_defaults:bool ->
  max_depth:int ->
  verbose_normalizer:bool ->
  no_typed_ast_for_imports:bool ->
  include_refs:(ALoc.t -> Loc.t) option ->
  include_refinement_info:(ALoc.t -> Loc.t) option ->
  File_key.t ->
  int ->
  int ->
  (Loc.t * Ty.type_at_pos_result option * Loc.t list * (Loc.t * Refinement_invalidation.reason) list)
  * (string * Hh_json.json) list

val batched_type_at_pos_from_special_comments :
  cx:Context.t ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  omit_targ_defaults:bool ->
  max_depth:int ->
  verbose_normalizer:bool ->
  no_typed_ast_for_imports:bool ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  File_key.t ->
  ( Loc.t
  * Loc.t
  * Ty.type_at_pos_result option
  * Loc.t list
  * (Loc.t * Refinement_invalidation.reason) list
  )
  list
  * Hh_json.json

val dump_types :
  evaluate_type_destructors:Ty_normalizer_env.evaluate_type_destructors_mode ->
  Context.t ->
  File_sig.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  (Loc.t * string) list

val coverage :
  cx:Context.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  force:bool ->
  File_key.t ->
  string ->
  (Loc.t * Coverage.Kind.t) list
