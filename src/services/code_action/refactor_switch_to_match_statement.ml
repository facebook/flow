(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception UnableToConvertSwitch

let pattern_of_expression = function
  | Flow_ast.Expression.NumberLiteral lit -> Flow_ast.MatchPattern.NumberPattern lit
  | Flow_ast.Expression.BigIntLiteral lit -> Flow_ast.MatchPattern.BigIntPattern lit
  | Flow_ast.Expression.StringLiteral lit -> Flow_ast.MatchPattern.StringPattern lit
  | Flow_ast.Expression.BooleanLiteral lit -> Flow_ast.MatchPattern.BooleanPattern lit
  | Flow_ast.Expression.NullLiteral lit -> Flow_ast.MatchPattern.NullPattern lit
  | Flow_ast.Expression.Identifier id -> Flow_ast.MatchPattern.IdentifierPattern id
  | Flow_ast.Expression.Unary unary ->
    let { Flow_ast.Expression.Unary.operator; argument; comments } = unary in
    (match (operator, argument) with
    | (Flow_ast.Expression.Unary.Plus, (loc, Flow_ast.Expression.NumberLiteral lit)) ->
      Flow_ast.MatchPattern.UnaryPattern
        Flow_ast.MatchPattern.UnaryPattern.
          { operator = Plus; argument = (loc, NumberLiteral lit); comments }
    | (Flow_ast.Expression.Unary.Minus, (loc, Flow_ast.Expression.NumberLiteral lit)) ->
      Flow_ast.MatchPattern.UnaryPattern
        Flow_ast.MatchPattern.UnaryPattern.
          { operator = Minus; argument = (loc, NumberLiteral lit); comments }
    | (Flow_ast.Expression.Unary.Minus, (loc, Flow_ast.Expression.BigIntLiteral lit)) ->
      Flow_ast.MatchPattern.UnaryPattern
        Flow_ast.MatchPattern.UnaryPattern.
          { operator = Minus; argument = (loc, BigIntLiteral lit); comments }
    | _ -> raise UnableToConvertSwitch)
  | Flow_ast.Expression.Member member ->
    let rec convert_member member =
      let { Flow_ast.Expression.Member._object; property; comments } = member in
      let match_property =
        match property with
        | Flow_ast.Expression.Member.PropertyIdentifier id ->
          Flow_ast.MatchPattern.MemberPattern.PropertyIdentifier id
        | Flow_ast.Expression.Member.PropertyExpression (loc, Flow_ast.Expression.StringLiteral lit)
          ->
          Flow_ast.MatchPattern.MemberPattern.PropertyString (loc, lit)
        | Flow_ast.Expression.Member.PropertyExpression (loc, Flow_ast.Expression.NumberLiteral lit)
          ->
          Flow_ast.MatchPattern.MemberPattern.PropertyNumber (loc, lit)
        | Flow_ast.Expression.Member.PropertyExpression (loc, Flow_ast.Expression.BigIntLiteral lit)
          ->
          Flow_ast.MatchPattern.MemberPattern.PropertyBigInt (loc, lit)
        | _ -> raise UnableToConvertSwitch
      in
      let base =
        match _object with
        | (_, Flow_ast.Expression.Identifier id) ->
          Flow_ast.MatchPattern.MemberPattern.BaseIdentifier id
        | (_, Flow_ast.Expression.Member member) ->
          Flow_ast.MatchPattern.MemberPattern.BaseMember (convert_member member)
        | _ -> raise UnableToConvertSwitch
      in
      (Loc.none, { Flow_ast.MatchPattern.MemberPattern.base; property = match_property; comments })
    in
    Flow_ast.MatchPattern.MemberPattern (convert_member member)
  | _ -> raise UnableToConvertSwitch

let is_block_scope_variable = function
  | ( _,
      Flow_ast.Statement.VariableDeclaration
        {
          Flow_ast.Statement.VariableDeclaration.kind =
            Flow_ast.Variable.Let | Flow_ast.Variable.Const;
          _;
        }
    ) ->
    true
  | _ -> false

type output_kind =
  | StatementOutput
  | ReturnOutput
  | AssignOutput of { name: string }
  | UnknownOutput

let assign_output_kind name = function
  | UnknownOutput -> AssignOutput { name }
  | AssignOutput { name = existing_name } when existing_name = name -> AssignOutput { name }
  | _ -> StatementOutput

let convert_switch loc switch =
  let { Flow_ast.Statement.Switch.discriminant; cases; comments; exhaustive_out = _ } = switch in
  let num_cases = Base.List.length cases in
  let (match_cases_rev, patterns_acc, output_kind) =
    Base.List.foldi
      cases
      ~init:([], [], UnknownOutput)
      ~f:(fun i (cases_acc, patterns_acc, output_kind) case ->
        let last_case = i + 1 = num_cases in
        let (_, { Flow_ast.Statement.Switch.Case.test; consequent; comments }) = case in
        let pattern =
          (* We only allow the wildcard at the end. *)
          match test with
          | None when not last_case -> raise UnableToConvertSwitch
          | None ->
            ( Loc.none,
              Flow_ast.MatchPattern.WildcardPattern
                {
                  Flow_ast.MatchPattern.WildcardPattern.comments;
                  invalid_syntax_default_keyword = false;
                }
            )
          | Some (loc, expr) -> (loc, pattern_of_expression expr)
        in
        match consequent with
        | []
        | [(_, Flow_ast.Statement.Empty _)]
          when not last_case ->
          (* There was no `case` body, so we accumulate patterns to build up
             a future "or" pattern. *)
          (cases_acc, pattern :: patterns_acc, output_kind)
        | stmts ->
          let body_stmts =
            match stmts with
            | [(_, Flow_ast.Statement.Block { Flow_ast.Statement.Block.body; _ })] -> body
            | _ ->
              (* If we have `let` or `const` variables not within a block, give up. *)
              if Base.List.exists stmts ~f:is_block_scope_variable then raise UnableToConvertSwitch;
              stmts
          in
          let (stmts, output_kind) =
            match Base.List.rev body_stmts with
            | [] when last_case ->
              (* We allow empty last cases. *)
              (stmts, StatementOutput)
            | [] ->
              (* Valid empty cases already handled above, we don't handle the rest. *)
              raise UnableToConvertSwitch
            (* `return expr;` *)
            | [(_, Flow_ast.Statement.Return { Flow_ast.Statement.Return.argument = Some _; _ })] ->
              let output_kind =
                match output_kind with
                | UnknownOutput
                | ReturnOutput ->
                  ReturnOutput
                | _ -> StatementOutput
              in
              (* Single return-with-arg statement body.
                 This could be turned into expression that is returned. *)
              (body_stmts, output_kind)
            (* `name = expr; break;` *)
            | [
             (_, Flow_ast.Statement.Break { Flow_ast.Statement.Break.label = None; _ });
             ( ( _,
                 Flow_ast.Statement.Expression
                   {
                     Flow_ast.Statement.Expression.expression =
                       ( _,
                         Flow_ast.Expression.Assignment
                           {
                             Flow_ast.Expression.Assignment.operator = None;
                             left =
                               ( _,
                                 Flow_ast.Pattern.Identifier
                                   {
                                     Flow_ast.Pattern.Identifier.name =
                                       (_, { Flow_ast.Identifier.name; _ });
                                     _;
                                   }
                               );
                             _;
                           }
                       );
                     _;
                   }
               ) as assignment_stmt
             );
            ] ->
              (* Single assignment followed by break as the statement body.
                 This could be turned into expression that is assigned. *)
              ([assignment_stmt], assign_output_kind name output_kind)
            (* `name = expr;` when the last case  *)
            | [
             ( _,
               Flow_ast.Statement.Expression
                 {
                   Flow_ast.Statement.Expression.expression =
                     ( _,
                       Flow_ast.Expression.Assignment
                         {
                           Flow_ast.Expression.Assignment.operator = None;
                           left =
                             ( _,
                               Flow_ast.Pattern.Identifier
                                 {
                                   Flow_ast.Pattern.Identifier.name =
                                     (_, { Flow_ast.Identifier.name; _ });
                                   _;
                                 }
                             );
                           _;
                         }
                     );
                   _;
                 }
             );
            ]
              when last_case ->
              (* Single assignment statement body, no break but it's the last case.
                 This could be turned into expression that is assigned. *)
              (body_stmts, assign_output_kind name output_kind)
            | last_stmt :: stmts_rest_rev ->
              (match last_stmt with
              (* `break;` *)
              | (_, Flow_ast.Statement.Break { Flow_ast.Statement.Break.label = None; _ }) ->
                (* Drop that last `break`. *)
                (Base.List.rev stmts_rest_rev, StatementOutput)
              (* `return` | `throw` *)
              | (_, Flow_ast.Statement.Return _)
              | (_, Flow_ast.Statement.Throw _) ->
                (body_stmts, StatementOutput)
              | _ when last_case ->
                (* It is OK if the last case doesn't exit. *)
                (body_stmts, StatementOutput)
              | _ ->
                (* We don't support cases that don't end with an exit otherwise. *)
                raise UnableToConvertSwitch)
          in
          let body =
            ( Loc.none,
              Flow_ast.Statement.Block { Flow_ast.Statement.Block.body = stmts; comments = None }
            )
          in
          let pattern =
            match patterns_acc with
            | [] -> pattern
            | _ ->
              (* If we've accumulated patterns from previous cases with no body,
                 we turn the patterns into an "or" pattern. *)
              ( Loc.none,
                Flow_ast.MatchPattern.OrPattern
                  {
                    Flow_ast.MatchPattern.OrPattern.patterns =
                      Base.List.rev (pattern :: patterns_acc);
                    comments = None;
                  }
              )
          in
          let match_case =
            ( Loc.none,
              {
                Flow_ast.Match.Case.pattern;
                body;
                guard = None;
                comments;
                invalid_syntax = Autofix_match_syntax.empty_invalid_case_syntax;
              }
            )
          in
          (match_case :: cases_acc, [], output_kind)
    )
  in
  if not @@ Base.List.is_empty patterns_acc then
    (* If we ended with patterns that were never added to a case, give up. *)
    raise UnableToConvertSwitch
  else
    let stmts_of_block = function
      | (_, Flow_ast.Statement.Block { Flow_ast.Statement.Block.body; _ }) -> body
      | _ -> raise UnableToConvertSwitch
    in
    match output_kind with
    | ReturnOutput ->
      (* We can turn this into a return of a match expression. *)
      let match_cases =
        Base.List.rev_map match_cases_rev ~f:(fun case ->
            let (loc, { Flow_ast.Match.Case.pattern; body; guard; comments; invalid_syntax }) =
              case
            in
            let body =
              match stmts_of_block body with
              | [
               (_, Flow_ast.Statement.Return { Flow_ast.Statement.Return.argument = Some expr; _ });
              ] ->
                expr
              | _ -> raise UnableToConvertSwitch
            in
            (loc, { Flow_ast.Match.Case.pattern; body; guard; comments; invalid_syntax })
        )
      in
      let match_expr =
        ( Loc.none,
          Flow_ast.Expression.Match
            {
              Flow_ast.Match.arg = discriminant;
              cases = match_cases;
              match_keyword_loc = Loc.none;
              comments = None;
            }
        )
      in
      ( loc,
        Flow_ast.Statement.Return
          { Flow_ast.Statement.Return.argument = Some match_expr; comments; return_out = Loc.none }
      )
    | AssignOutput { name } ->
      (* We can turn this into an assign of a match expression. *)
      let match_cases =
        Base.List.rev_map match_cases_rev ~f:(fun case ->
            let (loc, { Flow_ast.Match.Case.pattern; body; guard; comments; invalid_syntax }) =
              case
            in
            let body =
              match stmts_of_block body with
              | [
               ( _,
                 Flow_ast.Statement.Expression
                   {
                     Flow_ast.Statement.Expression.expression =
                       ( _,
                         Flow_ast.Expression.Assignment
                           { Flow_ast.Expression.Assignment.operator = None; right = expr; _ }
                       );
                     _;
                   }
               );
              ] ->
                expr
              | _ -> raise UnableToConvertSwitch
            in
            (loc, { Flow_ast.Match.Case.pattern; body; guard; comments; invalid_syntax })
        )
      in
      let match_expr =
        ( Loc.none,
          Flow_ast.Expression.Match
            {
              Flow_ast.Match.arg = discriminant;
              cases = match_cases;
              match_keyword_loc = Loc.none;
              comments = None;
            }
        )
      in
      ( loc,
        Flow_ast.Statement.Expression
          {
            Flow_ast.Statement.Expression.expression =
              ( Loc.none,
                Flow_ast.Expression.Assignment
                  {
                    Flow_ast.Expression.Assignment.operator = None;
                    left =
                      ( Loc.none,
                        Flow_ast.Pattern.Identifier
                          {
                            Flow_ast.Pattern.Identifier.name =
                              (Loc.none, { Flow_ast.Identifier.name; comments = None });
                            annot = Flow_ast.Type.Missing Loc.none;
                            optional = false;
                          }
                      );
                    right = match_expr;
                    comments = None;
                  }
              );
            directive = None;
            comments;
          }
      )
    | UnknownOutput
    | StatementOutput ->
      ( loc,
        Flow_ast.Statement.Match
          {
            Flow_ast.Match.arg = discriminant;
            cases = Base.List.rev match_cases_rev;
            match_keyword_loc = Loc.none;
            comments;
          }
      )

class mapper target_loc =
  object (this)
    inherit Flow_ast_contains_mapper.mapper target_loc as super

    val mutable found : bool = false

    method is_found = found

    method! statement stmt =
      if found then
        stmt
      else
        match stmt with
        | (loc, Flow_ast.Statement.Switch switch) when this#target_contained_by loc ->
          let stmt' = convert_switch loc switch in
          if stmt' == stmt then
            stmt
          else (
            found <- true;
            stmt'
          )
        | _ -> super#statement stmt
  end

let refactor ast loc =
  let mapper = new mapper loc in
  try
    let ast' = mapper#program ast in
    if (not mapper#is_found) || ast' == ast then
      (* No change *)
      None
    else
      Some ast'
  with
  | UnableToConvertSwitch -> None
