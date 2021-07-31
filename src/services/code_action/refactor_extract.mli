(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type refactor = {
  title: string;
  new_ast: (Loc.t, Loc.t) Flow_ast.Program.t;
  added_imports: (string * Autofix_imports.bindings) list;
}

val provide_available_refactors :
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  full_cx:Context.t ->
  file:File_key.t ->
  file_sig:File_sig.With_ALoc.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  reader:Parsing_heaps.Reader.reader ->
  support_experimental_snippet_text_edit:bool ->
  extract_range:Loc.t ->
  refactor list
