(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let toplevels statement cx stmts =
  (* Find the first statement that causes abnormal control flow in the *original* ordering *)
  let (abnormal, stmts) =
    Base.List.fold_mapi stmts ~init:None ~f:(fun i acc stmt ->
        match stmt with
        | (loc, Ast.Statement.Empty empty) -> (acc, (loc, Ast.Statement.Empty empty))
        | _ ->
          let (stmt, abnormal) =
            Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx stmt)
          in
          let acc =
            match (abnormal, acc) with
            | (None, _) -> acc
            | (Some _, Some (n, _)) when n < i -> acc
            | (Some abnormal, _) -> Some (i, abnormal)
          in
          (acc, stmt)
    )
  in
  match abnormal with
  | Some (_, abnormal) -> (stmts, Some abnormal)
  | None -> (stmts, None)
