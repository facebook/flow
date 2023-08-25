(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t = Throw

type payload = ALoc.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

val throw_expr_control_flow_exception :
  ALoc.t -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t -> 'a

val catch_stmt_control_flow_exception :
  (unit -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t) ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Statement.t * t option

val catch_expr_control_flow_exception :
  (unit -> (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t) ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t * t option

val try_with_abnormal_exn : f:(unit -> 'a) -> on_abnormal_exn:(payload * t -> 'a) -> unit -> 'a
