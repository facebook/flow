(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class statements_collector (extract_range : Loc.t) =
  object (_this)
    inherit
      [(Loc.t, Loc.t) Flow_ast.Statement.t list, Loc.t] Flow_ast_visitor.visitor ~init:[] as super

    method! statement stmt =
      let (statement_loc, _) = stmt in
      if Loc.contains statement_loc extract_range then
        (* If the range is completely contained in the statement,
           we should recursve deeper to find smaller nested statements that are contained in the range. *)
        super#statement stmt
      else if Loc.contains extract_range statement_loc then
        let () = super#set_acc (stmt :: acc) in
        (* If the statement is already completely contained in the range, do not recursve deeper
           to collect more nested ones. *)
        stmt
      else
        (* If all other cases (disjoint, intersect), we should not go deeper to avoid partially
           collecting statements from a nested block. *)
        stmt
  end

let extract_statements ast extract_range =
  let collector = new statements_collector extract_range in
  collector#eval collector#program ast |> List.rev

let create_extracted_function statements =
  let id = Some (Ast_builder.Identifiers.identifier "newFunction") in
  (* TODO: add parameters from locally undefined variables within statements *)
  let params = Ast_builder.Functions.params [] in
  let body = Ast_builder.Functions.body statements in
  (* TODO: make it async if body contains await *)
  (* TODO: make it a generator if body contains yield *)
  Ast_builder.Functions.make ~id ~params ~body ()
