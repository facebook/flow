(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Collect all statements that are completely within the selection. *)
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

let union_loc acc loc =
  match acc with
  | None -> Some loc
  | Some existing_loc -> Some (Loc.btwn existing_loc loc)

(* Compute the union of all the statement locations that are touched by the selection. *)
class touched_statements_loc_visitor (extract_range : Loc.t) =
  object (this)
    inherit [Loc.t option, Loc.t] Flow_ast_visitor.visitor ~init:None as super

    method private union_loc loc = super#set_acc (union_loc acc loc)

    method! statement stmt =
      let (statement_loc, _) = stmt in
      if Loc.contains statement_loc extract_range then
        super#statement stmt
      else if Loc.intersects extract_range statement_loc then
        let () = this#union_loc statement_loc in
        stmt
      else
        stmt
  end

let extract_statements ast extract_range =
  let collector = new statements_collector extract_range in
  collector#eval collector#program ast |> List.rev

let allow_refactor_extraction ast extract_range extracted_statements =
  let visitor = new touched_statements_loc_visitor extract_range in
  let touched_range = visitor#eval visitor#program ast in
  let selected_range = extracted_statements |> List.map fst |> List.fold_left union_loc None in
  selected_range = touched_range

let create_extracted_function statements =
  let id = Some (Ast_builder.Identifiers.identifier "newFunction") in
  (* TODO: add parameters from locally undefined variables within statements *)
  let params = Ast_builder.Functions.params [] in
  let body = Ast_builder.Functions.body statements in
  (* TODO: make it async if body contains await *)
  (* TODO: make it a generator if body contains yield *)
  Ast_builder.Functions.make ~id ~params ~body ()
