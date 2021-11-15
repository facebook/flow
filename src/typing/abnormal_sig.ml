(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module Env : Env_sig.S

  type t =
    | Return
    | Throw
    | Break of string option
    | Continue of string option

  type payload =
    | Expr of ALoc.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t
    | Stmt of (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t
    | Stmts of (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list

  exception Exn of payload * t

  val throw_stmt_control_flow_exception : (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t -> t -> 'a

  val throw_stmts_control_flow_exception :
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list -> t -> 'a

  val throw_expr_control_flow_exception :
    ALoc.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t -> t -> 'a

  val check_stmt_control_flow_exception :
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t * t option ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t

  val catch_stmt_control_flow_exception :
    (unit -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t) ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t * t option

  val catch_stmts_control_flow_exception :
    (unit -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list) ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t list * t option

  val catch_expr_control_flow_exception :
    (unit -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t) ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t * t option

  val ignore_break_to_label : string option -> 'a * t option -> 'a * t option

  val ignore_break_or_continue_to_label : string option -> 'a * t option -> 'a * t option

  val save : ?env:Env.t -> t -> unit

  val swap_saved : t -> Env.t option -> Env.t option

  val clear_saved : t -> Env.t option
end
