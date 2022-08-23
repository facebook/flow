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
  let stmts = Base.List.mapi stmts ~f:(fun i s -> (i, s)) in
  (* Find the first statement that causes abnormal control flow in the *original* ordering *)
  let (rev_acc, abnormal) =
    Base.List.fold
      ~init:([], None)
      ~f:(fun (acc, acc_abnormal) (i, stmt) ->
        match stmt with
        | (loc, Ast.Statement.Empty empty) ->
          ((i, (loc, Ast.Statement.Empty empty)) :: acc, acc_abnormal)
        | stmt ->
          let (stmt, acc_abnormal) =
            match Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx stmt) with
            | (stmt, Some abnormal) ->
              let abnormal =
                match acc_abnormal with
                | Some (n, _) when n < i -> acc_abnormal
                | _ -> Some (i, abnormal)
              in
              (stmt, abnormal)
            | (stmt, None) -> (stmt, acc_abnormal)
          in
          ((i, stmt) :: acc, acc_abnormal))
      stmts
  in
  let stmts = Base.List.rev_map rev_acc ~f:snd in
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
