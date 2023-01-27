(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* accumulates a list of previous statements' ASTs in reverse order *)
(* can raise Abnormal.(Exn (Stmts _, _)). *)
module Ast = Flow_ast
module Flow = Flow_js

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
  (* If there was any abnormal control flow, add errors on any statements that are
     lexically after the place where abnormal control was raised *)
  match abnormal with
  | Some (n, abnormal) ->
    let warn_unreachable loc = Flow.add_output cx (Error_message.EUnreachable loc) in
    Base.List.iteri
      ~f:
        (fun i -> function
          | (_, Ast.Statement.Empty _)
          | (_, Ast.Statement.FunctionDeclaration _) ->
            ()
          | (_, Ast.Statement.VariableDeclaration d) when i > n ->
            Ast.Statement.VariableDeclaration.(
              d.declarations
              |> List.iter
                   Declarator.(
                     function
                     | (_, { init = Some ((loc, _), _); _ }) -> warn_unreachable loc
                     | _ -> ()
                   )
            )
          | (loc, _) when i > n -> warn_unreachable loc
          | _ -> ())
      stmts;
    Abnormal.throw_stmts_control_flow_exception stmts abnormal
  | None -> stmts
