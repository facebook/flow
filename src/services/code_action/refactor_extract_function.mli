(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc

val extract_statements :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Statement.t list option

(* Find the smallest containing class of the extracted statements.
   This is the only valid extraction location
   if we want to extract to a method and call this.newMethod(); *)
val find_closest_enclosing_class_scope :
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  extracted_statements_loc:Loc.t ->
  (string option * Loc.t) option

val collect_relevant_defs_with_scope :
  scope_info:Scope_api.info ->
  extracted_statements_loc:Loc.t ->
  (Scope_api.Def.t * Scope_api.Scope.t) list

(* After moving extracted statements into a function into another scope, some variables might
   become undefined since original definition exists in inner scopes.
   This function computes such list from the scope information of definitions and the location
   of the scope to put the extracted function. *)
val undefined_variables_after_extraction :
  scope_info:Scope_api.info ->
  relevant_defs_with_scope:(Scope_api.Def.t * Scope_api.Scope.t) list ->
  new_function_target_scope_loc:Loc.t ->
  extracted_statements_loc:Loc.t ->
  string list

val collect_escaping_local_defs :
  scope_info:Scope_api.info -> extracted_statements_loc:Loc.t -> string list

val provide_available_refactors :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (string * (Loc.t, Loc.t) Flow_ast.Program.t) list
