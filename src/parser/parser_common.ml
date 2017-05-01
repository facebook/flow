(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Parser_env
open Ast
module Error = Parse_error

module type PARSER = sig
  val program : env -> program
  val statement : env -> Statement.t
  val statement_list_item : ?decorators:Expression.t list -> env -> Statement.t
  val statement_list : term_fn:(Token.t -> bool) -> env -> Statement.t list
  val statement_list_with_directives : term_fn:(Token.t -> bool) -> env -> Statement.t list * bool
  val module_body : term_fn:(Token.t -> bool) -> env -> Statement.t list
  val expression : env -> Expression.t
  val conditional : env -> Expression.t
  val assignment : env -> Expression.t
  val left_hand_side : env -> Expression.t
  val object_initializer : env -> Loc.t * Expression.Object.t
  val array_initializer : env -> Loc.t * Expression.Array.t
  val identifier : ?restricted_error:Error.t -> env -> Identifier.t
  val identifier_or_reserved_keyword : env -> (Identifier.t * (Loc.t * Error.t) option)
  val identifier_with_type : env -> ?no_optional:bool -> Error.t -> Loc.t * Pattern.Identifier.t
  val block_body : env -> Loc.t * Statement.Block.t
  val function_block_body : env -> Loc.t * Statement.Block.t * bool
  val jsx_element : env -> Loc.t * JSX.element
  val pattern : env -> Error.t -> Pattern.t
  val pattern_from_expr : env -> Expression.t -> Pattern.t
  val object_key : env -> Loc.t * Expression.Object.Property.key
  val class_declaration : env -> Expression.t list -> Statement.t
  val class_expression : env -> Expression.t
  val is_assignable_lhs : Expression.t -> bool
end

let with_loc fn env =
  let start_loc = Peek.loc env in
  let result = fn env in
  let end_loc = match last_loc env with
  | Some loc -> loc
  | None ->
      error env (Error.Assertion "did not consume any tokens");
      Peek.loc env
  in
  Loc.btwn start_loc end_loc, result
