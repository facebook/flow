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
  File_key.t ->
  int ->
  int ->
  (Loc.t * Ty.type_at_pos_result option) * (string * Hh_json.json) list

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
