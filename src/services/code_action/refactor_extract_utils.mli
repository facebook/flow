(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Scope_api = Scope_api.With_Loc
module Ssa_api = Ssa_api.With_Loc

module StatementsExtractor : sig
  val extract :
    (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> (Loc.t, Loc.t) Flow_ast.Statement.t list option
end

module InformationCollectors : sig
  type t = {
    has_unwrapped_control_flow: bool;
    async_function: bool;
    in_class: bool;
  }

  val collect_statements_information : (Loc.t, Loc.t) Flow_ast.Statement.t list -> t
end

module LocationCollectors : sig
  (* Find locations to insert `newFunction` definitions. *)
  val collect_function_inserting_locs :
    ast:(Loc.t, Loc.t) Flow_ast.Program.t -> extracted_statements_loc:Loc.t -> (string * Loc.t) list

  (* Find the smallest containing class of the extracted statements.
     This is the only valid extraction location
     if we want to extract to a method and call this.newMethod(); *)
  val find_closest_enclosing_class_scope :
    ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
    extracted_statements_loc:Loc.t ->
    (string option * Loc.t) option
end

module RefactorProgramMappers : sig
  val extract_to_function :
    target_body_loc:Loc.t ->
    extracted_statements_loc:Loc.t ->
    function_call_statements:(Loc.t, Loc.t) Flow_ast_mapper.Ast.Statement.t list ->
    function_declaration_statement:(Loc.t, Loc.t) Flow_ast.Statement.t ->
    (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
    (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t

  val extract_to_method :
    target_body_loc:Loc.t ->
    extracted_statements_loc:Loc.t ->
    function_call_statements:(Loc.t, Loc.t) Flow_ast_mapper.Ast.Statement.t list ->
    method_declaration:(Loc.t, Loc.t) Flow_ast_mapper.Ast.Class.Body.element ->
    (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t ->
    (Loc.t, Loc.t) Flow_ast_mapper.Ast.Program.t
end

module VariableAnalysis : sig
  type relevant_defs = {
    (* All the definitions that are used by the extracted statements, along with their scopes. *)
    defs_with_scopes_of_local_uses: (Scope_api.Def.t * Scope_api.Scope.t) list;
    (* All the variables that have been reassigned within the extracted statements that
       would be shadowed after refactor. *)
    vars_with_shadowed_local_reassignments: (string * Loc.t) list;
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
    (string * Loc.t) list

  type escaping_definitions = {
    (* A list of variable names that are defined inside the extracted statements,
       but have uses outside of them. *)
    escaping_variables: (string * Loc.t) list;
    (* Whether any of the escaping variables has another write outside of extracted statements. *)
    has_external_writes: bool;
  }

  val collect_escaping_local_defs :
    scope_info:Scope_api.info ->
    ssa_values:Ssa_api.values ->
    extracted_statements_loc:Loc.t ->
    escaping_definitions
end

module TypeSynthesizer : sig
  (* An object of all the information needed to provide and transform parameter type annotations. *)
  type synthesizer_context

  val create_synthesizer_context :
    full_cx:Context.t ->
    file:File_key.t ->
    file_sig:File_sig.With_ALoc.t ->
    typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_polymorphic_ast_mapper.Ast.Program.t ->
    reader:Parsing_heaps.Reader.reader ->
    locs:Loc_collections.LocSet.t ->
    synthesizer_context

  type type_synthesizer_with_import_adder = {
    type_synthesizer: Loc.t -> (Loc.t, Loc.t) Flow_ast.Type.t option;
    added_imports: unit -> (string * Autofix_imports.bindings) list;
  }

  val create_type_synthesizer_with_import_adder :
    synthesizer_context -> type_synthesizer_with_import_adder
end
