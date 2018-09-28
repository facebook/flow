(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type patch = (int * int * string) list

val show_patch: patch -> string

val mk_patch : Mapper_differ.t -> (Loc.t, Loc.t) Flow_ast.program -> string -> patch

val mk_patch_ast_differ : Flow_ast_differ.node Flow_ast_differ.change list ->
  (Loc.t, Loc.t) Flow_ast.program -> string -> patch

val print : patch -> string -> string
