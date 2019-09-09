(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type patch = (int * int * string) list

type loc_patch = (Loc.t * string) list

val show_patch : patch -> string

val mk_loc_patch_ast_differ :
  Flow_ast_differ.node Flow_ast_differ.change list -> (Loc.t, Loc.t) Flow_ast.program -> loc_patch

val mk_patch_ast_differ :
  Flow_ast_differ.node Flow_ast_differ.change list ->
  (Loc.t, Loc.t) Flow_ast.program ->
  string ->
  patch

val mk_patch_ast_differ_unsafe :
  Flow_ast_differ.node Flow_ast_differ.change list ->
  (Loc.t, Loc.t) Flow_ast.program ->
  File_input.t ->
  patch

val print : patch -> string -> string

val print_unsafe : patch -> File_input.t -> string
