(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val parse_and_pack_builtins :
  Type_sig_options.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t list ->
  Type_sig_collections.Locs.index Type_sig.errno list
  * Loc.t Type_sig_collections.Locs.t
  * Type_sig_collections.Locs.index Packed_type_sig.Builtins.t

val parse_and_pack_module :
  strict:bool ->
  platform_availability_set:Platform_set.t option ->
  Type_sig_options.t ->
  File_key.t option ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Type_sig_collections.Locs.index Type_sig.errno list
  * Loc.t Type_sig_collections.Locs.t
  * Type_sig_collections.Locs.index Packed_type_sig.Module.t
