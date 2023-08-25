(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* we model abnormal control flows using exceptions during traversal *)

type t = Throw

type payload = ALoc.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Expression.t

exception Exn of payload

let throw_expr_control_flow_exception loc expr = raise (Exn (loc, expr))

let (catch_stmt_control_flow_exception, catch_expr_control_flow_exception) =
  let catch_control_flow_exception p f =
    try
      let res = f () in
      (res, None)
    with
    | Exn payload -> (p payload, Some Throw)
    | exn -> raise exn
  in
  ( catch_control_flow_exception (function (loc, exp) ->
        ( loc,
          Flow_ast.Statement.Expression
            { Flow_ast.Statement.Expression.expression = exp; directive = None; comments = None }
        )
        ),
    catch_control_flow_exception (function (_, exp) -> exp)
  )

(********************************************************************)

let try_with_abnormal_exn ~f ~on_abnormal_exn () =
  try f () with
  | Exn payload -> on_abnormal_exn (payload, Throw)
  | exc -> raise exc
