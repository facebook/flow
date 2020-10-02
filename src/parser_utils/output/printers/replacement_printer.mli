(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type patch = (int * int * string) list

type loc_patch = (Loc.t * string) list

val show_patch : patch -> string

val mk_loc_patch_ast_differ :
  ?opts:Js_layout_generator.opts -> Flow_ast_differ.node Flow_ast_differ.change list -> loc_patch

val mk_patch_ast_differ :
  ?opts:Js_layout_generator.opts ->
  Flow_ast_differ.node Flow_ast_differ.change list ->
  string ->
  patch

val mk_patch_ast_differ_unsafe :
  ?opts:Js_layout_generator.opts ->
  Flow_ast_differ.node Flow_ast_differ.change list ->
  File_input.t ->
  patch

val print : patch -> string -> string

val print_unsafe : patch -> File_input.t -> string
