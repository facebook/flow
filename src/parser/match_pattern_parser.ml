(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_env
open Parser_common
open Flow_ast.MatchPattern
module Ast = Flow_ast

module Match_pattern (Parse : PARSER) : Parser_common.MATCH_PATTERN = struct
  let rec match_pattern env =
    let start_loc = Peek.loc env in
    ignore @@ Eat.maybe env T_BIT_OR;
    let pattern = subpattern env in
    let pattern =
      match Peek.token env with
      | T_BIT_OR ->
        let rec or_patterns env acc =
          match Peek.token env with
          | T_BIT_OR ->
            Eat.token env;
            let acc = subpattern env :: acc in
            or_patterns env acc
          | _ -> List.rev acc
        in
        let (or_loc, or_pattern) =
          with_loc
            ~start_loc
            (fun env ->
              let patterns = or_patterns env [pattern] in
              let trailing = Eat.trailing_comments env in
              let comments = Flow_ast_utils.mk_comments_opt ~trailing () in
              { OrPattern.patterns; comments })
            env
        in
        (or_loc, OrPattern or_pattern)
      | _ -> pattern
    in
    match Peek.token env with
    | T_IDENTIFIER { raw = "as"; _ } ->
      let (as_loc, as_pattern) =
        with_loc
          ~start_loc
          (fun env ->
            Eat.token env;
            let target =
              match Peek.token env with
              | T_CONST ->
                let (loc, binding) = binding_pattern env ~kind:Ast.Variable.Const in
                AsPattern.Binding (loc, binding)
              | T_LET ->
                let (loc, binding) = binding_pattern env ~kind:Ast.Variable.Let in
                AsPattern.Binding (loc, binding)
              | T_VAR ->
                let (loc, binding) = binding_pattern env ~kind:Ast.Variable.Var in
                AsPattern.Binding (loc, binding)
              | _ -> AsPattern.Identifier (Parse.identifier env)
            in
            let trailing = Eat.trailing_comments env in
            let comments = Flow_ast_utils.mk_comments_opt ~trailing () in
            { AsPattern.pattern; target; comments })
          env
      in
      (as_loc, AsPattern as_pattern)
    | _ -> pattern

  and subpattern env =
    match Peek.token env with
    | T_IDENTIFIER { raw = "_"; _ } ->
      let leading = Peek.comments env in
      let loc = Peek.loc env in
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, WildcardPattern { WildcardPattern.comments; invalid_syntax_default_keyword = false })
    | T_DEFAULT ->
      let leading = Peek.comments env in
      let loc = Peek.loc env in
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, WildcardPattern { WildcardPattern.comments; invalid_syntax_default_keyword = true })
    | T_LPAREN ->
      let leading = Peek.comments env in
      Expect.token env T_LPAREN;
      let pattern = match_pattern env in
      Expect.token env T_RPAREN;
      let trailing = Eat.trailing_comments env in
      add_comments ~leading ~trailing pattern
    | T_NUMBER { kind; raw } ->
      let leading = Peek.comments env in
      let loc = Peek.loc env in
      let value = Parse.number env kind raw in
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, NumberPattern { Ast.NumberLiteral.value; raw; comments })
    | T_BIGINT { kind; raw } ->
      let leading = Peek.comments env in
      let loc = Peek.loc env in
      let value = Parse.bigint env kind raw in
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, BigIntPattern { Ast.BigIntLiteral.value; raw; comments })
    | T_STRING (loc, value, raw, octal) ->
      let leading = Peek.comments env in
      if octal then strict_error env Parse_error.StrictOctalLiteral;
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, StringPattern { Ast.StringLiteral.value; raw; comments })
    | (T_TRUE | T_FALSE) as token ->
      let leading = Peek.comments env in
      let loc = Peek.loc env in
      Eat.token env;
      let value = token = T_TRUE in
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, BooleanPattern { Ast.BooleanLiteral.value; comments })
    | T_NULL ->
      let leading = Peek.comments env in
      let loc = Peek.loc env in
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, NullPattern comments)
    | T_PLUS -> unary_pattern env ~operator:UnaryPattern.Plus
    | T_MINUS -> unary_pattern env ~operator:UnaryPattern.Minus
    | T_CONST ->
      let (loc, binding) = binding_pattern env ~kind:Ast.Variable.Const in
      (loc, BindingPattern binding)
    | T_LET ->
      let (loc, binding) = binding_pattern env ~kind:Ast.Variable.Let in
      (loc, BindingPattern binding)
    | T_VAR ->
      let (loc, binding) = binding_pattern env ~kind:Ast.Variable.Var in
      (loc, BindingPattern binding)
    | T_LCURLY -> object_pattern env
    | T_LBRACKET -> array_pattern env
    | _ when Peek.is_identifier env ->
      let start_loc = Peek.loc env in
      let id = Parse.identifier env in
      let rec member acc =
        match Peek.token env with
        | T_PERIOD ->
          let mem =
            with_loc
              ~start_loc
              (fun env ->
                Eat.token env;
                let property = MemberPattern.PropertyIdentifier (identifier_name env) in
                let trailing = Eat.trailing_comments env in
                let comments = Flow_ast_utils.mk_comments_opt ~trailing () in
                { MemberPattern.base = acc; property; comments })
              env
          in
          member (MemberPattern.BaseMember mem)
        | T_LBRACKET ->
          let mem =
            with_loc
              ~start_loc
              (fun env ->
                Expect.token env T_LBRACKET;
                let leading = Peek.comments env in
                let property =
                  match Peek.token env with
                  | T_STRING (loc, value, raw, octal) ->
                    if octal then strict_error env Parse_error.StrictOctalLiteral;
                    Expect.token env (T_STRING (loc, value, raw, octal));
                    let trailing = Eat.trailing_comments env in
                    let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
                    MemberPattern.PropertyString (loc, { Ast.StringLiteral.value; raw; comments })
                  | T_NUMBER { kind; raw } ->
                    let loc = Peek.loc env in
                    let value = Parse.number env kind raw in
                    let trailing = Eat.trailing_comments env in
                    let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
                    MemberPattern.PropertyNumber (loc, { Ast.NumberLiteral.value; raw; comments })
                  | T_BIGINT { kind; raw } ->
                    let loc = Peek.loc env in
                    let value = Parse.bigint env kind raw in
                    let trailing = Eat.trailing_comments env in
                    let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
                    MemberPattern.PropertyBigInt (loc, { Ast.BigIntLiteral.value; raw; comments })
                  | _ ->
                    error_unexpected ~expected:"a numeric or string literal" env;
                    let loc = Peek.loc env in
                    MemberPattern.PropertyString
                      (loc, { Ast.StringLiteral.value = ""; raw = "\"\""; comments = None })
                in
                Expect.token env T_RBRACKET;
                let trailing = Eat.trailing_comments env in
                let comments = Flow_ast_utils.mk_comments_opt ~trailing () in
                { MemberPattern.base = acc; property; comments })
              env
          in
          member (MemberPattern.BaseMember mem)
        | _ ->
          (match acc with
          | MemberPattern.BaseIdentifier ((loc, _) as id) -> (loc, IdentifierPattern id)
          | MemberPattern.BaseMember (loc, member) -> (loc, MemberPattern (loc, member)))
      in
      member (MemberPattern.BaseIdentifier id)
    | t ->
      let leading = Peek.comments env in
      let loc = Peek.loc env in
      error_unexpected env;
      (* Let's get rid of the bad token *)
      (match t with
      | T_ERROR _ -> Eat.token env
      | _ -> ());
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing:[] () in
      (loc, WildcardPattern { WildcardPattern.comments; invalid_syntax_default_keyword = false })

  and unary_pattern env ~operator =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Eat.token env;
        let argument =
          match Peek.token env with
          | T_NUMBER { kind; raw } ->
            let leading = Peek.comments env in
            let loc = Peek.loc env in
            let value = Parse.number env kind raw in
            let trailing = Eat.trailing_comments env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
            (loc, UnaryPattern.NumberLiteral { Ast.NumberLiteral.value; raw; comments })
          | T_BIGINT { kind; raw } ->
            let leading = Peek.comments env in
            let loc = Peek.loc env in
            let value = Parse.bigint env kind raw in
            let trailing = Eat.trailing_comments env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
            (loc, UnaryPattern.BigIntLiteral { Ast.BigIntLiteral.value; raw; comments })
          | _ ->
            let loc = Peek.loc env in
            error_unexpected ~expected:"a number literal" env;
            ( loc,
              UnaryPattern.NumberLiteral
                { Ast.NumberLiteral.value = 0.; raw = "0"; comments = None }
            )
        in
        let trailing = Eat.trailing_comments env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        UnaryPattern { UnaryPattern.operator; argument; comments })
      env

  and binding_pattern env ~kind =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Eat.token env;
        let id = Parse.identifier ~restricted_error:Parse_error.StrictVarName env in
        let trailing = Eat.trailing_comments env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        { BindingPattern.kind; id; comments })
      env

  and object_pattern env =
    let property_key env =
      let open ObjectPattern.Property in
      let leading = Peek.comments env in
      match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Parse_error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let trailing = Eat.trailing_comments env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        StringLiteral (loc, { Ast.StringLiteral.value; raw; comments })
      | T_NUMBER { kind; raw } ->
        let loc = Peek.loc env in
        let value = Parse.number env kind raw in
        let trailing = Eat.trailing_comments env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        NumberLiteral (loc, { Ast.NumberLiteral.value; raw; comments })
      | T_BIGINT { kind; raw } ->
        let loc = Peek.loc env in
        let value = Parse.bigint env kind raw in
        let trailing = Eat.trailing_comments env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        BigIntLiteral (loc, { Ast.BigIntLiteral.value; raw; comments })
      | _ ->
        let id = identifier_name env in
        Identifier id
    in
    let property =
      with_loc (fun env ->
          let leading = Peek.comments env in
          let shorthand_prop (loc, binding) =
            let { BindingPattern.id = (_, id); _ } = binding in
            let key = ObjectPattern.Property.Identifier (loc, id) in
            let pattern = (loc, BindingPattern binding) in
            let trailing = Eat.trailing_comments env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
            ObjectPattern.Property.Valid
              { ObjectPattern.Property.key; pattern; shorthand = true; comments }
          in
          match Peek.token env with
          | T_CONST -> shorthand_prop (binding_pattern env ~kind:Ast.Variable.Const)
          | T_LET -> shorthand_prop (binding_pattern env ~kind:Ast.Variable.Let)
          | T_VAR -> shorthand_prop (binding_pattern env ~kind:Ast.Variable.Var)
          | _
            when Peek.is_identifier env
                 &&
                 match Peek.ith_token ~i:1 env with
                 | T_COMMA
                 | T_RCURLY ->
                   true
                 | _ -> false ->
            ObjectPattern.Property.InvalidShorthand (identifier_name env)
          | _ ->
            let key = property_key env in
            Expect.token env T_COLON;
            let pattern = match_pattern env in
            let trailing = Eat.trailing_comments env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
            ObjectPattern.Property.Valid
              { ObjectPattern.Property.key; pattern; shorthand = false; comments }
      )
    in
    let rec properties env acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        (List.rev acc, None)
      | T_ELLIPSIS ->
        let rest = rest_pattern env in
        if Peek.token env = T_COMMA then
          error_at env (Peek.loc env, Parse_error.MatchNonLastRest `Object);
        (List.rev acc, Some rest)
      | _ ->
        let prop = property env in
        if not (Peek.token env = T_RCURLY) then Expect.token env T_COMMA;
        properties env (prop :: acc)
    in
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_LCURLY;
        let (properties, rest) = properties env [] in
        let internal = Peek.comments env in
        Expect.token env T_RCURLY;
        let trailing = Eat.trailing_comments env in
        ObjectPattern
          {
            ObjectPattern.properties;
            rest;
            comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
          })
      env

  and array_pattern env =
    let rec elements env ~start_loc acc =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET ->
        (List.rev acc, None)
      | T_ELLIPSIS ->
        let rest = rest_pattern env in
        if Peek.token env = T_COMMA then
          error_at env (Peek.loc env, Parse_error.MatchNonLastRest `Array);
        (List.rev acc, Some rest)
      | _ ->
        let pattern = match_pattern env in
        let index = Loc.btwn start_loc (Peek.loc env) in
        if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
        let element = { ArrayPattern.Element.index; pattern } in
        elements env ~start_loc (element :: acc)
    in
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let (elements, rest) = elements env ~start_loc [] in
        let internal = Peek.comments env in
        Expect.token env T_RBRACKET;
        let trailing = Eat.trailing_comments env in
        let comments =
          Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ()
        in
        ArrayPattern { ArrayPattern.elements; rest; comments })
      env

  and rest_pattern env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_ELLIPSIS;
        let argument =
          match Peek.token env with
          | T_CONST -> Some (binding_pattern env ~kind:Ast.Variable.Const)
          | T_LET -> Some (binding_pattern env ~kind:Ast.Variable.Let)
          | T_VAR -> Some (binding_pattern env ~kind:Ast.Variable.Var)
          | _ -> None
        in
        let trailing = Eat.trailing_comments env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        { RestPattern.argument; comments })
      env

  and add_comments ?(leading = []) ?(trailing = []) (loc, pattern) =
    let merge_comments inner =
      Flow_ast_utils.merge_comments
        ~inner
        ~outer:(Flow_ast_utils.mk_comments_opt ~leading ~trailing ())
    in
    let merge_comments_with_internal inner =
      Flow_ast_utils.merge_comments_with_internal
        ~inner
        ~outer:(Flow_ast_utils.mk_comments_opt ~leading ~trailing ())
    in
    ( loc,
      match pattern with
      | WildcardPattern ({ WildcardPattern.comments; _ } as p) ->
        WildcardPattern { p with WildcardPattern.comments = merge_comments comments }
      | NumberPattern ({ Ast.NumberLiteral.comments; _ } as p) ->
        NumberPattern { p with Ast.NumberLiteral.comments = merge_comments comments }
      | BigIntPattern ({ Ast.BigIntLiteral.comments; _ } as p) ->
        BigIntPattern { p with Ast.BigIntLiteral.comments = merge_comments comments }
      | StringPattern ({ Ast.StringLiteral.comments; _ } as p) ->
        StringPattern { p with Ast.StringLiteral.comments = merge_comments comments }
      | BooleanPattern ({ Ast.BooleanLiteral.comments; _ } as p) ->
        BooleanPattern { p with Ast.BooleanLiteral.comments = merge_comments comments }
      | NullPattern comments -> NullPattern (merge_comments comments)
      | UnaryPattern ({ UnaryPattern.comments; _ } as p) ->
        UnaryPattern { p with UnaryPattern.comments = merge_comments comments }
      | BindingPattern ({ BindingPattern.comments; _ } as p) ->
        BindingPattern { p with BindingPattern.comments = merge_comments comments }
      | IdentifierPattern (id_loc, ({ Ast.Identifier.comments; _ } as p)) ->
        IdentifierPattern (id_loc, { p with Ast.Identifier.comments = merge_comments comments })
      | MemberPattern (loc, ({ MemberPattern.comments; _ } as p)) ->
        MemberPattern (loc, { p with MemberPattern.comments = merge_comments comments })
      | ObjectPattern ({ ObjectPattern.comments; _ } as p) ->
        ObjectPattern { p with ObjectPattern.comments = merge_comments_with_internal comments }
      | ArrayPattern ({ ArrayPattern.comments; _ } as p) ->
        ArrayPattern { p with ArrayPattern.comments = merge_comments_with_internal comments }
      | OrPattern ({ OrPattern.comments; _ } as p) ->
        OrPattern { p with OrPattern.comments = merge_comments comments }
      | AsPattern ({ AsPattern.comments; _ } as p) ->
        AsPattern { p with AsPattern.comments = merge_comments comments }
    )
end
