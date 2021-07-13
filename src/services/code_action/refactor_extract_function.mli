(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val provide_available_refactors :
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  parsing_heap_reader:Parsing_heaps.Reader.reader ->
  extract_range:Loc.t ->
  (string * (Loc.t, Loc.t) Flow_ast.Program.t) list
