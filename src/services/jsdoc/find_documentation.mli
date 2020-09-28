(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val jsdoc_of_getdef_loc :
  ?current_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  reader:Parsing_heaps.Reader.reader ->
  Loc.t ->
  Jsdoc.t option

val documentation_of_jsdoc : Jsdoc.t -> string option
