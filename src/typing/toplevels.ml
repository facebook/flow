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
  let toplevels statement =
    let rec loop acc cx = function
      | [] -> List.rev acc
      | (loc, Ast.Statement.Empty empty) :: stmts ->
        loop ((loc, Ast.Statement.Empty empty) :: acc) cx stmts
      | stmt :: stmts ->
        (match Abnormal.catch_stmt_control_flow_exception (fun () -> statement cx stmt) with
        | (stmt, Some abnormal) ->
          (* control flow exit out of a flat list:
           check for unreachable code and rethrow *)
          let warn_unreachable loc = Flow.add_output cx (Error_message.EUnreachable loc) in
          let rest =
            Base.List.map
              ~f:
                (let open Ast.Statement in
                fun stmt ->
                  match stmt with
                  | (_, Empty _) as stmt -> stmt
                  (* function declarations are hoisted, so not unreachable *)
                  | (_, FunctionDeclaration _) -> statement cx stmt
                  (* variable declarations are hoisted, but associated assignments are
                   not, so skip variable declarations with no assignments.
                   Note: this does not seem like a practice anyone would use *)
                  | (_, VariableDeclaration d) as stmt ->
                    VariableDeclaration.(
                      d.declarations
                      |> List.iter
                           Declarator.(
                             function
                             | (_, { init = Some (loc, _); _ }) -> warn_unreachable loc
                             | _ -> ()));
                    Tast_utils.unreachable_mapper#statement stmt
                  | (loc, _) as stmt ->
                    warn_unreachable loc;
                    Tast_utils.unreachable_mapper#statement stmt)
              stmts
          in
          Abnormal.throw_stmts_control_flow_exception (List.rev_append acc (stmt :: rest)) abnormal
        | (stmt, None) -> loop (stmt :: acc) cx stmts)
    in
    fun cx statements ->
      let ordering = Order.make statements in
      Base.List.sort statements (Order.compare ordering) |> loop [] cx
end

module LexicalOrdering : Ordering with type loc = ALoc.t = struct
  type t = unit

  type loc = ALoc.t

  let make _ = ()

  let compare _ (l1, _) (l2, _) = Loc.compare (ALoc.to_loc_exn l1) (ALoc.to_loc_exn l2)
end

module LexicalToplevels = Toplevels (LexicalOrdering)
include LexicalToplevels
