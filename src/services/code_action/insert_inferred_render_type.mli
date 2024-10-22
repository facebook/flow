(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val insert_render_type_at_loc :
  ?remote_converter:Insert_type_imports.ImportsHelper.remote_converter ->
  cx:Context.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_module_info:(File_key.t -> Haste_module_info.t option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t_out) Flow_ast.Program.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t option
