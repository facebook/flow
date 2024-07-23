(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module InsertionPointCollectors : sig
  type function_insertion_point = {
    function_name: string;
    body_loc: Loc.t;
    is_method: bool;
    tparams_rev: Type.typeparam list;
  }

  type class_insertion_point = {
    class_name: string option;
    body_loc: Loc.t;
    tparams_rev: Type.typeparam list;
  }

  (* Find locations to insert `newFunction`/`newMethod` definitions. *)
  val collect_function_method_inserting_points :
    typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
    loc_of_aloc:(ALoc.t -> Loc.t) ->
    extracted_loc:Loc.t ->
    function_insertion_point list

  (* Find the smallest containing class of the extracted statements.
     This is the only valid extraction location
     if we want to extract to a method and call this.newMethod(); *)
  val find_closest_enclosing_class :
    typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
    loc_of_aloc:(ALoc.t -> Loc.t) ->
    extracted_loc:Loc.t ->
    class_insertion_point option
end

module AstExtractor : sig
  type constant_insertion_point = {
    title: string;
    function_body_loc: Loc.t option;
    statement_loc: Loc.t;
  }
  [@@deriving show]

  type expression_with_constant_insertion_points = {
    constant_insertion_points: constant_insertion_point Nel.t;
    expression: (Loc.t, Loc.t) Flow_ast.Expression.t;
  }

  type type_with_statement_loc = {
    directly_containing_statement_loc: Loc.t;
    type_: (Loc.t, Loc.t) Flow_ast.Type.t;
  }

  type extracted = {
    extracted_statements: (Loc.t, Loc.t) Flow_ast.Statement.t list option;
    extracted_expression: expression_with_constant_insertion_points option;
    extracted_type: type_with_statement_loc option;
  }

  val tokens :
    ?parse_options:Parser_env.parse_options ->
    File_key.t option ->
    string ->
    Parser_env.token_sink_result list

  val extract :
    Parser_env.token_sink_result list -> (Loc.t, Loc.t) Flow_ast.Program.t -> Loc.t -> extracted
end
