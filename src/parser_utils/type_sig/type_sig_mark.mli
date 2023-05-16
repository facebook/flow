(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mark_binding : locs_to_dirtify:Loc.t list -> Loc.t Type_sig_parse.binding_node -> unit

val mark_exports :
  locs_to_dirtify:Loc.t list ->
  Loc.t Type_sig_collections.Locs.node ->
  Loc.t Type_sig_parse.exports ->
  unit

val mark_builtin_module :
  Loc.t Type_sig_collections.Locs.node * Loc.t Type_sig_parse.exports -> unit
