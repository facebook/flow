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
  | InvalidWildcardSyntax
  | InvalidCaseSyntax

let is_invalid_case_syntax invalid_syntax =
  let {
    Flow_ast.Match.Case.InvalidSyntax.invalid_prefix_case;
    invalid_infix_colon;
    invalid_suffix_semicolon;
  } =
    invalid_syntax
  in
  Base.Option.is_some invalid_prefix_case
  || Base.Option.is_some invalid_infix_colon
  || Base.Option.is_some invalid_suffix_semicolon

let empty_invalid_case_syntax =
  {
    Flow_ast.Match.Case.InvalidSyntax.invalid_prefix_case = None;
    invalid_infix_colon = None;
    invalid_suffix_semicolon = None;
  }

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
        | (loc, WildcardPattern wildcard) when kind = InvalidWildcardSyntax && this#is_target loc ->
          let { WildcardPattern.comments; invalid_syntax_default_keyword = _ } = wildcard in
          (loc, WildcardPattern { WildcardPattern.comments; invalid_syntax_default_keyword = false })
        | _ -> pattern
      in
      super#match_pattern pattern

    method! match_ loc ~on_case_body x =
      let open Flow_ast.Match in
      let { match_keyword_loc; _ } = x in
      let x =
        if kind = InvalidCaseSyntax && this#is_target match_keyword_loc then
          let { arg; cases; match_keyword_loc; comments } = x in
          let cases =
            Base.List.map cases ~f:(fun case ->
                let (loc, { Case.pattern; body; guard; comments; invalid_syntax }) = case in
                if is_invalid_case_syntax invalid_syntax then
                  let invalid_syntax = empty_invalid_case_syntax in
                  (loc, { Case.pattern; body; guard; comments; invalid_syntax })
                else
                  case
            )
          in
          { arg; cases; match_keyword_loc; comments }
        else
          x
      in
      super#match_ loc ~on_case_body x

    method! match_case ~on_case_body case =
      let open Flow_ast.Match.Case in
      let (loc, { pattern; body; guard; comments; invalid_syntax }) = case in
      let case =
        if
          kind = InvalidCaseSyntax
          && this#target_contained_by loc
          && is_invalid_case_syntax invalid_syntax
        then
          let invalid_syntax = empty_invalid_case_syntax in
          (loc, { pattern; body; guard; comments; invalid_syntax })
        else
          case
      in
      super#match_case ~on_case_body case
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

let fix_invalid_wildcard_syntax ast loc =
  let mapper = new mapper loc ~kind:InvalidWildcardSyntax in
  mapper#program ast

let fix_invalid_case_syntax ast loc =
  let mapper = new mapper loc ~kind:InvalidCaseSyntax in
  mapper#program ast
