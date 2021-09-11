(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val provide_symbol_information :
  uri:Lsp.DocumentUri.t -> (Loc.t, Loc.t) Flow_ast.Program.t -> Lsp.SymbolInformation.t list
