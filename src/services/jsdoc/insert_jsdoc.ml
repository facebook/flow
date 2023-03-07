(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type target_loc =
  | LocOfTarget of Loc.t
  | LocOfComment of Loc.t

class mapper ~use_snippets target_loc =
  let covers_target loc =
    match target_loc with
    | LocOfTarget target_loc -> Reason.in_range target_loc loc
    | LocOfComment _ -> false
  in
  let contains_comment_target comments =
    match target_loc with
    | LocOfComment target_loc ->
      Base.List.exists ~f:(fun (loc, _) -> Loc.equal target_loc loc) comments
    | LocOfTarget _ -> false
  in
  let string_of_stub stub =
    let trailing_space =
      match target_loc with
      | LocOfTarget _ -> true
      | LocOfComment _ -> false
    in
    let s = Jsdoc_stub.string_of_stub ~use_snippets stub in
    if trailing_space then
      s ^ " "
    else
      s
  in
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    val mutable inserted_stub : string option = None

    method get_inserted_stub = inserted_stub

    method comments_of_stub stub =
      Some
        {
          Flow_ast.Syntax.leading = [Ast_builder.Comments.block stub];
          trailing = [];
          internal = ();
        }

    method! function_declaration loc decl =
      let decl_with_stub () =
        let stub = decl |> Jsdoc_stub.stub_for_function |> string_of_stub in
        inserted_stub <- Some stub;
        let comments = this#comments_of_stub stub in
        { decl with Flow_ast.Function.comments }
      in
      match decl with
      | { Flow_ast.Function.id = Some (loc, _); comments = None; _ } when covers_target loc ->
        decl_with_stub ()
      | { Flow_ast.Function.comments = Some { Flow_ast.Syntax.leading; _ }; _ }
        when contains_comment_target leading ->
        decl_with_stub ()
      | _ -> super#function_declaration loc decl

    method! declare_function loc decl =
      let decl_with_stub () =
        match decl with
        | {
         Flow_ast.Statement.DeclareFunction.annot = (_, (_, Flow_ast.Type.Function func_type));
         _;
        } -> begin
          match Ast_builder.Functions.of_type func_type with
          | Some func ->
            let stub = func |> Jsdoc_stub.stub_for_function |> string_of_stub in
            inserted_stub <- Some stub;
            let leading = [Ast_builder.Comments.block stub] in
            {
              decl with
              Flow_ast.Statement.DeclareFunction.comments =
                Some { Flow_ast.Syntax.leading; trailing = []; internal = () };
            }
          | _ -> super#declare_function loc decl
        end
        | _ -> super#declare_function loc decl
      in
      match decl with
      | { Flow_ast.Statement.DeclareFunction.id = (loc, _); comments = None; _ }
        when covers_target loc ->
        decl_with_stub ()
      | { Flow_ast.Statement.DeclareFunction.comments = Some { Flow_ast.Syntax.leading; _ }; _ }
        when contains_comment_target leading ->
        decl_with_stub ()
      | _ -> super#declare_function loc decl
  end

let insert ~use_snippets target_loc ast =
  let mapper = new mapper ~use_snippets target_loc in
  let ast' = mapper#program ast in
  Base.Option.map mapper#get_inserted_stub ~f:(fun inserted_stub -> (ast', inserted_stub))

let insert_stub_for_target target_loc = insert (LocOfTarget target_loc)

let insert_stub_in_comment comment_loc = insert (LocOfComment comment_loc)
