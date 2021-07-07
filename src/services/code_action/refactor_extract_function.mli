(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc
module Ssa_api = Ssa_api.With_Loc

val extract_statements :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Statement.t list option

(* Find the smallest containing class of the extracted statements.
   This is the only valid extraction location
   if we want to extract to a method and call this.newMethod(); *)
val find_closest_enclosing_class_scope :
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  extracted_statements_loc:Loc.t ->
  (string option * Loc.t) option

type relevant_defs = {
  (* All the definitions that are used by the extracted statements, along with their scopes. *)
  defs_with_scopes_of_local_uses: (Scope_api.Def.t * Scope_api.Scope.t) list;
  (* All the variables that have been reassigned within the extracted statements that
     would be shadowed after refactor. *)
  vars_with_shadowed_local_reassignments: string list;
}

(* Finding lists of definitions relevant to refactor analysis.
   See the type definition of `relevant_defs` for more information. *)
val collect_relevant_defs_with_scope :
  scope_info:Scope_api.info ->
  ssa_values:Ssa_api.values ->
  extracted_statements_loc:Loc.t ->
  relevant_defs

(* After moving extracted statements into a function into another scope, some variables might
   become undefined since original definition exists in inner scopes.
   This function computes such list from the scope information of definitions and the location
   of the scope to put the extracted function. *)
val undefined_variables_after_extraction :
  scope_info:Scope_api.info ->
  defs_with_scopes_of_local_uses:(Scope_api.Def.t * Scope_api.Scope.t) list ->
  new_function_target_scope_loc:Loc.t ->
  extracted_statements_loc:Loc.t ->
  string list

type escaping_definitions = {
  (* A list of variable names that are defined inside the extracted statements,
     but have uses outside of them.  *)
  escaping_variables: string list;
  (* Whether any of the escaping variables has another write outside of extracted statements. *)
  has_external_writes: bool;
}

val collect_escaping_local_defs :
  scope_info:Scope_api.info ->
  ssa_values:Ssa_api.values ->
  extracted_statements_loc:Loc.t ->
  escaping_definitions

val provide_available_refactors :
  (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (string * (Loc.t, Loc.t) Flow_ast.Program.t) list
