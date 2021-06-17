(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc

val extract_statements :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Statement.t list option

val collect_relevant_defs_with_scope :
  scope_info:Scope_api.info ->
  extracted_statements_loc:Loc.t ->
  (Scope_api.Def.t * Scope_api.Scope.t) list

val create_extracted_function :
  (Loc.t, Loc.t) Flow_ast.Statement.t list -> (Loc.t, Loc.t) Flow_ast.Function.t

val provide_available_refactors :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (string * (Loc.t, Loc.t) Flow_ast.Program.t) list
