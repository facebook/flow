(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val jsdoc_of_getdef_loc :
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  reader:Parsing_heaps.Reader.reader ->
  Loc.t ->
  Jsdoc.t option

val documentation_of_jsdoc : Jsdoc.t -> string option

val def_loc_to_comment_loc_map : (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t Loc_sig.LocS.LMap.t

val hardcoded_documentation_at_loc : (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> string option

val module_doc_loc : (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t option
