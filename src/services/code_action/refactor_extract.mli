(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type refactor = {
  title: string;
  new_ast: (Loc.t, Loc.t) Flow_ast.Program.t;
  added_imports: (string * Autofix_imports.bindings) list;
}

val provide_available_refactors :
  tokens:Parser_env.token_sink_result list ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  cx:Context.t ->
  file:File_key.t ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_name:(File_key.t -> string option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  support_experimental_snippet_text_edit:bool ->
  extract_range:Loc.t ->
  refactor list
