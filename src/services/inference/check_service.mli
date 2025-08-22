(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type resolved_module = Parsing_heaps.dependency_addr Parsing_heaps.resolved_module'

type check_file =
  only_support_flow_fixme_and_expected_error:bool ->
  File_key.t ->
  resolved_module Flow_import_specifier.Map.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  File_sig.t ->
  Docblock.t ->
  ALoc.table Lazy.t ->
  FindRefsTypes.request ->
  Context.t
  * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t
  * (FindRefsTypes.single_ref list, string) result

type compute_env =
  File_key.t ->
  resolved_module Flow_import_specifier.Map.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Docblock.t ->
  ALoc.table Lazy.t ->
  Context.t * (ALoc.t, ALoc.t) Flow_ast.Program.t

type check_file_and_comp_env = {
  check_file: check_file;
  compute_env: compute_env;
}

val mk_check_file :
  reader:Abstract_state_reader.t ->
  options:Options.t ->
  master_cx:Context.master_context ->
  cache:Check_cache.t ->
  unit ->
  check_file_and_comp_env
