(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper comment_loc =
  let contains_comment = Base.List.exists ~f:(fun (loc, _) -> Loc.equal comment_loc loc) in
  object
    inherit [Loc.t] Flow_ast_mapper.mapper as super

    val mutable inserted_stub : string option = None

    method get_inserted_stub = inserted_stub

    method! function_declaration loc decl =
      match decl with
      | { Flow_ast.Function.comments = Some { Flow_ast.Syntax.leading; _ }; _ }
        when contains_comment leading ->
        let stub = decl |> Jsdoc_stub.stub_for_function |> Jsdoc_stub.string_of_stub in
        inserted_stub <- Some stub;
        let leading = [Ast_builder.Comments.block stub] in
        {
          decl with
          Flow_ast.Function.comments =
            Some { Flow_ast.Syntax.leading; trailing = []; internal = () };
        }
      | _ -> super#function_declaration loc decl

    method! declare_function loc decl =
      match decl with
      | {
       Flow_ast.Statement.DeclareFunction.annot = (_, (_, Flow_ast.Type.Function func_type));
       comments = Some { Flow_ast.Syntax.leading; _ };
       _;
      }
        when contains_comment leading -> begin
        match Ast_builder.Functions.of_type func_type with
        | Some func ->
          let stub = func |> Jsdoc_stub.stub_for_function |> Jsdoc_stub.string_of_stub in
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
  end

let insert_stub_in_comment comment_loc ast =
  let mapper = new mapper comment_loc in
  let ast' = mapper#program ast in
  Base.Option.map mapper#get_inserted_stub ~f:(fun inserted_stub -> (ast', inserted_stub))
