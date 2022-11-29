(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

class mapper target =
  object (this)
    inherit [Loc.t] Flow_ast_contains_mapper.mapper as super

    method private target_contained_by loc = Loc.contains loc target

    method private is_target loc = Loc.equal target loc

    method loc_annot_contains_target = this#target_contained_by

    method! statement stmt =
      let open Flow_ast in
      match stmt with
      | (loc, Statement.Expression { Statement.Expression.expression; directive; comments })
        when this#is_target loc ->
        ( loc,
          Statement.Expression
            {
              Statement.Expression.expression =
                ( loc,
                  Expression.Unary
                    {
                      Expression.Unary.operator = Expression.Unary.Await;
                      argument = expression;
                      comments = None;
                    }
                );
              directive;
              comments;
            }
        )
      | _ -> super#statement stmt
  end

let insert_await ast loc =
  let mapper = new mapper loc in
  mapper#program ast
