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
  | NonExhaustiveObjectPattern of {
      add_rest: bool;
      missing_props: string list;
    }
  | NotExhaustive of (Loc.t, Loc.t) Flow_ast.MatchPattern.t list
  | UnusedPattern

let is_valid_ident_name = Parser_flow.string_is_valid_identifier_name

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
      let { Flow_ast.Match.match_keyword_loc; _ } = x in
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
      | NotExhaustive patterns_to_add when this#is_target match_keyword_loc ->
        let { Flow_ast.Match.arg; cases; match_keyword_loc; comments } = x in
        let new_cases =
          Base.List.map patterns_to_add ~f:(fun pattern ->
              let body =
                ( Loc.none,
                  Flow_ast.Statement.Block { Flow_ast.Statement.Block.body = []; comments = None }
                )
              in
              ( Loc.none,
                {
                  Flow_ast.Match.Case.pattern;
                  body;
                  guard = None;
                  comments = None;
                  invalid_syntax = empty_invalid_case_syntax;
                }
              )
          )
        in
        let x = { Flow_ast.Match.arg; cases = cases @ new_cases; match_keyword_loc; comments } in
        super#match_statement loc x
      | _ -> super#match_statement loc x

    method! match_expression loc x =
      let { Flow_ast.Match.match_keyword_loc; _ } = x in
      match kind with
      | NotExhaustive patterns_to_add when this#is_target match_keyword_loc ->
        let { Flow_ast.Match.arg; cases; match_keyword_loc; comments } = x in
        let new_cases =
          Base.List.map patterns_to_add ~f:(fun pattern ->
              let body =
                ( Loc.none,
                  Flow_ast.Expression.Identifier
                    (Loc.none, { Flow_ast.Identifier.name = "undefined"; comments = None })
                )
              in
              ( Loc.none,
                {
                  Flow_ast.Match.Case.pattern;
                  body;
                  guard = None;
                  comments = None;
                  invalid_syntax = empty_invalid_case_syntax;
                }
              )
          )
        in
        let x = { Flow_ast.Match.arg; cases = cases @ new_cases; match_keyword_loc; comments } in
        super#match_expression loc x
      | _ -> super#match_expression loc x

    method! match_pattern pattern =
      let open Flow_ast.MatchPattern in
      let pattern =
        match (pattern, kind) with
        | ((loc, BindingPattern binding), InvalidBindingKind) when this#is_target loc ->
          let { BindingPattern.kind = _; id; comments } = binding in
          (loc, BindingPattern { BindingPattern.kind = Flow_ast.Variable.Const; id; comments })
        | ((loc, WildcardPattern wildcard), InvalidWildcardSyntax) when this#is_target loc ->
          let { WildcardPattern.comments; invalid_syntax_default_keyword = _ } = wildcard in
          (loc, WildcardPattern { WildcardPattern.comments; invalid_syntax_default_keyword = false })
        | ( (loc, ObjectPattern { ObjectPattern.properties; rest; comments }),
            NonExhaustiveObjectPattern { add_rest; missing_props }
          )
          when this#is_target loc ->
          let rest =
            if add_rest then
              Some (Loc.none, { RestPattern.argument = None; comments = None })
            else
              rest
          in
          let properties =
            if Base.List.is_empty missing_props then
              properties
            else
              let added_props =
                Base.List.map missing_props ~f:(fun name ->
                    let key =
                      if is_valid_ident_name name then
                        ObjectPattern.Property.Identifier
                          (Loc.none, { Flow_ast.Identifier.name; comments = None })
                      else
                        ObjectPattern.Property.StringLiteral
                          ( Loc.none,
                            {
                              Flow_ast.StringLiteral.value = name;
                              raw = "" (* Raw unused *);
                              comments = None;
                            }
                          )
                    in
                    let pattern =
                      ( Loc.none,
                        WildcardPattern
                          {
                            WildcardPattern.comments = None;
                            invalid_syntax_default_keyword = false;
                          }
                      )
                    in
                    ( Loc.none,
                      ObjectPattern.Property.Valid
                        { ObjectPattern.Property.key; pattern; shorthand = false; comments = None }
                    )
                )
              in
              properties @ added_props
          in
          (loc, ObjectPattern { ObjectPattern.properties; rest; comments })
        | ( (loc, ObjectPattern { ObjectPattern.properties; rest = Some (rest_loc, _); comments }),
            UnusedPattern
          )
          when this#is_target rest_loc ->
          (loc, ObjectPattern { ObjectPattern.properties; rest = None; comments })
        | ( (loc, ArrayPattern { ArrayPattern.elements; rest = Some (rest_loc, _); comments }),
            UnusedPattern
          )
          when this#is_target rest_loc ->
          (loc, ArrayPattern { ArrayPattern.elements; rest = None; comments })
        | ((loc, OrPattern { OrPattern.patterns; comments }), UnusedPattern)
          when this#target_contained_by loc ->
          let (patterns_rev, changed) =
            Base.List.fold patterns ~init:([], false) ~f:(fun (patterns_rev, changed) pattern ->
                let (pattern_loc, _) = pattern in
                if this#is_target pattern_loc then
                  (patterns_rev, true)
                else if changed then
                  (* Update loc for subsequent patterns so that no newline appears. *)
                  let pattern = (Loc.none, snd pattern) in
                  (pattern :: patterns_rev, changed)
                else
                  (pattern :: patterns_rev, changed)
            )
          in
          if changed then
            match patterns_rev with
            | [single_pattern] -> single_pattern
            | _ -> (loc, OrPattern { OrPattern.patterns = Base.List.rev patterns_rev; comments })
          else
            pattern
        | _ -> pattern
      in
      super#match_pattern pattern

    method! match_ loc ~on_case_body x =
      let open Flow_ast.Match in
      let { match_keyword_loc; _ } = x in
      let x =
        match kind with
        | InvalidCaseSyntax when this#is_target match_keyword_loc ->
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
        | UnusedPattern when this#target_contained_by loc ->
          let { arg; cases; match_keyword_loc; comments } = x in
          let (cases_rev, changed) =
            Base.List.fold cases ~init:([], false) ~f:(fun (cases_rev, changed) case ->
                let (_, { Case.pattern = (pattern_loc, _); _ }) = case in
                if this#is_target pattern_loc then
                  (cases_rev, true)
                else if changed then
                  (* Update loc for subsequent cases so that no newline appears. *)
                  let case = (Loc.none, snd case) in
                  (case :: cases_rev, changed)
                else
                  (case :: cases_rev, changed)
            )
          in
          if changed then
            { arg; cases = Base.List.rev cases_rev; match_keyword_loc; comments }
          else
            x
        | _ -> x
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

let fix_non_exhaustive_object_pattern ~add_rest missing_props ast loc =
  let mapper = new mapper loc ~kind:(NonExhaustiveObjectPattern { add_rest; missing_props }) in
  mapper#program ast

let fix_not_exhaustive patterns_to_add ast loc =
  let mapper = new mapper loc ~kind:(NotExhaustive patterns_to_add) in
  mapper#program ast

let remove_unused_pattern ast loc =
  let mapper = new mapper loc ~kind:UnusedPattern in
  mapper#program ast
