(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module LocSet = Loc_collections.LocSet

val set_of_fixable_signature_verification_locations : File_sig.tolerable_error list -> LocSet.t

val fix_signature_verification_error_at_loc :
  ?remote_converter:Insert_type_imports.ImportsHelper.remote_converter ->
  cx:Context.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_name:(File_key.t -> string option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Loc.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t

val fix_signature_verification_errors :
  file_key:File_key.t ->
  cx:Context.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  file_options:Files.options ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_name:(File_key.t -> string option) ->
  get_type_sig:(File_key.t -> Type_sig_collections.Locs.index Packed_type_sig.Module.t option) ->
  file_sig:File_sig.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  LocSet.t ->
  (Loc.t * (Loc.t, Loc.t) Flow_ast.Program.t') * string list
