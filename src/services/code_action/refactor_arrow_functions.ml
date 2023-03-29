(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper scope_loc =
  object
    inherit Flow_ast_contains_mapper.mapper scope_loc as super

    val mutable title : string option = None

    method get_title = title

    method! arrow_function loc expr =
      let { Flow_ast.Function.body; _ } = expr in
      match body with
      | Flow_ast.Function.BodyBlock
          ( body_loc,
            {
              Flow_ast.Statement.Block.body =
                [
                  ( _,
                    Flow_ast.Statement.Return
                      { Flow_ast.Statement.Return.argument = Some return_expr; _ }
                  );
                ];
              _;
            }
          )
        when Loc.equal body_loc scope_loc ->
        title <- Some "Remove braces from arrow function";
        { expr with Flow_ast.Function.body = Ast_builder.Functions.body_expression return_expr }
      | Flow_ast.Function.BodyExpression ((body_loc, _) as body_expr)
        when Loc.equal body_loc scope_loc ->
        title <- Some "Add braces to arrow function";
        {
          expr with
          Flow_ast.Function.body =
            Ast_builder.Functions.body [Ast_builder.Statements.return (Some body_expr)];
        }
      | _ -> super#arrow_function loc expr
  end

module Scope_api = Scope_api.With_Loc

let add_or_remove_braces ~ast ~scope_info loc =
  let enclosing_scope_loc =
    let scope_id = Scope_api.closest_enclosing_scope scope_info loc Reason.in_range in
    let { Scope_api.Scope.loc = scope_loc; _ } = Scope_api.scope scope_info scope_id in
    scope_loc
  in
  let mapper = new mapper enclosing_scope_loc in
  let ast' = mapper#program ast in
  Base.Option.map mapper#get_title ~f:(fun title -> (ast', title))
