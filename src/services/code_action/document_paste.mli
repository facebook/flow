(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

val prepare_document_paste :
  Context.t ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  ast:(Loc.t, Loc.t) Ast.Program.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Ast.Program.t ->
  ranges:Loc.t list ->
  Lsp.DocumentPaste.import_item list

val provide_document_paste_edits :
  layout_options:Js_layout_generator.opts ->
  module_system_info:Lsp_module_system_info.t ->
  src_dir:string option ->
  (Loc.t, Loc.t) Ast.Program.t ->
  Lsp.DocumentPaste.import_item list ->
  (Loc.t * string) list
