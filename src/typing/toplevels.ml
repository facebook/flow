(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* accumulates a list of previous statements' ASTs in reverse order *)
(* can raise Abnormal.(Exn (Stmts _, _)). *)
module Ast = Flow_ast
module Flow = Flow_js
module Tast_utils = Typed_ast_utils

module type Ordering = sig
  type t

  type loc

  val make : (loc, loc) Ast.Statement.t list -> t

  val compare : t -> (loc, loc) Ast.Statement.t -> (loc, loc) Ast.Statement.t -> int
end

module Toplevels (Order : Ordering with type loc = ALoc.t) = struct
  let toplevels statement cx stmts =
    let ordering = Order.make stmts in
    (* Enumerate and sort statements using the order specified *)
    let stmts =
      Base.List.mapi stmts ~f:(fun i s -> (i, s))
      |> Base.List.sort ~compare:(fun a b -> Order.compare ordering (snd a) (snd b))
    in
    (* Check the statement in the new order, but also find the first
       statement that causes abnormal control flow in the *original*
       ordering *)
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
    (* Undo the reordering of the now-checked statements *)
    let stmts =
      List.rev rev_acc
      |> Base.List.sort ~compare:(fun a b -> Stdlib.compare (fst a) (fst b))
      |> Base.List.map ~f:snd
    in
    (* If there was any abnormal control flow, add errors on any statements that are
       lexically after the place where abnormal control was raised *)
    match abnormal with
    | Some (n, abnormal) ->
      let warn_unreachable loc = Flow.add_output cx (Error_message.EUnreachable loc) in
      Base.List.iteri
        ~f:(fun i -> function
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
                     | _ -> ()))
          | (loc, _) when i > n -> warn_unreachable loc
          | _ -> ())
        stmts;
      Abnormal.throw_stmts_control_flow_exception stmts abnormal
    | None -> stmts
end

module LexicalOrdering : Ordering with type loc = ALoc.t = struct
  type t = unit

  type loc = ALoc.t

  let make _ = ()

  let compare _ (l1, _) (l2, _) = Loc.compare (ALoc.to_loc_exn l1) (ALoc.to_loc_exn l2)
end

module LexicalToplevels = Toplevels (LexicalOrdering)
include LexicalToplevels
