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
      if Loc.contains extract_range statement_loc then
        let () = super#set_acc (stmt :: acc) in
        (* If the statement is already completely contained in the range, do not recursve deeper
           to collect more nested ones. *)
        stmt
      else if Loc.contains statement_loc extract_range then
        (* If the range is completely contained in the statement,
           we should recursve deeper to find smaller nested statements that are contained in the range. *)
        super#statement stmt
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

class new_function_call_replacer insert_new_function_call_loc rest_statements_loc_union =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    method private should_be_replaced_by_function_call loc =
      Loc.equal insert_new_function_call_loc loc

    method private inserted_function_call_loc () =
      match rest_statements_loc_union with
      | None -> insert_new_function_call_loc
      | Some rest_union -> Loc.btwn insert_new_function_call_loc rest_union

    method private should_be_replaced_by_empty loc =
      match rest_statements_loc_union with
      | Some rest_statements_loc_union -> Loc.contains rest_statements_loc_union loc
      | None -> false

    method! statement_fork_point stmt =
      let (statement_loc, _) = stmt in
      let open Ast_builder in
      if this#should_be_replaced_by_function_call statement_loc then
        [
          Statements.expression
            ~loc:(this#inserted_function_call_loc ())
            (Expressions.call (Expressions.identifier "newFunction"));
        ]
      else if this#should_be_replaced_by_empty statement_loc then
        []
      else
        super#statement_fork_point stmt
  end

let extract_statements ast extract_range =
  let collector = new statements_collector extract_range in
  collector#eval collector#program ast |> List.rev

let allow_refactor_extraction ast extract_range extracted_statements_locations =
  let visitor = new touched_statements_loc_visitor extract_range in
  let touched_range = visitor#eval visitor#program ast in
  let selected_range = List.fold_left union_loc None extracted_statements_locations in
  selected_range = touched_range

let create_extracted_function statements =
  let id = Some (Ast_builder.Identifiers.identifier "newFunction") in
  (* TODO: add parameters from locally undefined variables within statements *)
  let params = Ast_builder.Functions.params [] in
  let body = Ast_builder.Functions.body statements in
  (* TODO: make it async if body contains await *)
  (* TODO: make it a generator if body contains yield *)
  Ast_builder.Functions.make ~id ~params ~body ()

let replace_statements_with_new_function_call ast extracted_statements_locations =
  match extracted_statements_locations with
  | [] -> None
  | insert_new_function_call_loc :: rest_statements_locations ->
    let rest_statements_loc_union = List.fold_left union_loc None rest_statements_locations in
    let replacer =
      new new_function_call_replacer insert_new_function_call_loc rest_statements_loc_union
    in
    Some (replacer#program ast)

let provide_available_refactor ast extract_range =
  let extracted_statements = extract_statements ast extract_range in
  let extracted_statements_locations = List.map fst extracted_statements in
  if allow_refactor_extraction ast extract_range extracted_statements_locations then
    replace_statements_with_new_function_call ast extracted_statements_locations
  else
    None
