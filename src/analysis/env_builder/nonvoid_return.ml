(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

class return_finder =
  object (this)
    inherit [bool, ALoc.t] Flow_ast_visitor.visitor ~init:false as super

    method! return _ ({ Ast.Statement.Return.argument; _ } as node) =
      if Base.Option.is_some argument then this#set_acc true;
      node

    method! call _loc expr =
      let open Ast.Expression.Call in
      let { callee; arguments; _ } = expr in
      ( if Flow_ast_utils.is_call_to_invariant callee then
        match arguments with
        (* invariant() and invariant(false, ...) are treated like throw *)
        | (_, { Ast.Expression.ArgList.arguments = []; comments = _ })
        | ( _,
            {
              Ast.Expression.ArgList.arguments =
                Ast.Expression.Expression
                  (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Boolean false; _ })
                :: _;
              comments = _;
            }
          ) ->
          this#set_acc true
        | _ -> ()
      );
      expr

    method! throw _loc stmt =
      this#set_acc true;
      stmt

    method! function_body_any body =
      begin
        match body with
        (* If it's a body expression, some value is implicitly returned *)
        | Ast.Function.BodyExpression _ -> this#set_acc true
        | _ -> ()
      end;
      super#function_body_any body

    (* Any returns in these constructs would be for nested function definitions, so we short-circuit
     *)
    method! class_ _ x = x

    method! function_declaration _ x = x

    method! function_expression _ x = x

    method! arrow_function _ x = x
  end

let might_have_nonvoid_return loc function_ast =
  let finder = new return_finder in
  finder#eval (finder#function_ loc) function_ast
