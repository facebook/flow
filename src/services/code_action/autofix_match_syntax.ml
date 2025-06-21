(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type kind =
  | ObjShorthandToConst
  | ObjShorthandToReference
  | InvalidMatchStatementBody
  | InvalidBindingKind

class mapper target_loc ~kind =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    method! match_object_pattern_property prop =
      let open Flow_ast.MatchPattern.ObjectPattern in
      let (loc, _) = prop in
      if not @@ this#is_target loc then
        super#match_object_pattern_property prop
      else
        match prop with
        | (_, Property.Valid _) -> super#match_object_pattern_property prop
        | (_, Property.InvalidShorthand id) ->
          let key = Property.Identifier id in
          (match kind with
          | ObjShorthandToConst ->
            let pattern =
              ( Loc.none,
                Flow_ast.MatchPattern.BindingPattern
                  {
                    Flow_ast.MatchPattern.BindingPattern.kind = Flow_ast.Variable.Const;
                    id;
                    comments = None;
                  }
              )
            in
            (Loc.none, Property.Valid { Property.key; pattern; shorthand = true; comments = None })
          | ObjShorthandToReference ->
            let pattern = (Loc.none, Flow_ast.MatchPattern.IdentifierPattern id) in
            (Loc.none, Property.Valid { Property.key; pattern; shorthand = false; comments = None })
          | _ -> super#match_object_pattern_property prop)

    method! match_statement loc x =
      match kind with
      | InvalidMatchStatementBody when this#target_contained_by loc ->
        let on_case_body stmt =
          let (stmt_loc, _) = stmt in
          let body =
            if not @@ this#is_target stmt_loc then
              stmt
            else
              ( Loc.none,
                Flow_ast.Statement.Block { Flow_ast.Statement.Block.body = [stmt]; comments = None }
              )
          in
          this#statement body
        in
        this#match_ loc ~on_case_body x
      | _ -> super#match_statement loc x

    method! match_pattern pattern =
      let open Flow_ast.MatchPattern in
      let pattern =
        match pattern with
        | (loc, BindingPattern binding) when kind = InvalidBindingKind && this#is_target loc ->
          let { BindingPattern.kind = _; id; comments } = binding in
          (loc, BindingPattern { BindingPattern.kind = Flow_ast.Variable.Const; id; comments })
        | _ -> pattern
      in
      super#match_pattern pattern
  end

let convert_object_shorthand_to_const ast loc =
  let mapper = new mapper loc ~kind:ObjShorthandToConst in
  mapper#program ast

let convert_object_shorthand_to_reference ast loc =
  let mapper = new mapper loc ~kind:ObjShorthandToReference in
  mapper#program ast

let fix_invalid_match_statement_body ast loc =
  let mapper = new mapper loc ~kind:InvalidMatchStatementBody in
  mapper#program ast

let fix_invalid_binding_kind ast loc =
  let mapper = new mapper loc ~kind:InvalidBindingKind in
  mapper#program ast
