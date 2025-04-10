(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_env
open Flow_ast
open Parser_common
open Comment_attachment

module Statement
    (Parse : PARSER)
    (Type : Parser_common.TYPE)
    (Declaration : Parser_common.DECLARATION)
    (Object : Parser_common.OBJECT)
    (Pattern_cover : Parser_common.COVER)
    (Expression : Parser_common.EXPRESSION) : Parser_common.STATEMENT = struct
  module Enum = Enum_parser.Enum (Parse)

  type for_lhs =
    | For_expression of pattern_cover
    | For_declaration of (Loc.t * (Loc.t, Loc.t) Ast.Statement.VariableDeclaration.t)

  type semicolon_type =
    | Explicit of Loc.t Comment.t list
    | Implicit of Comment_attachment.trailing_and_remover_result

  (* FunctionDeclaration is not a valid Statement, but Annex B sometimes allows it.
     However, AsyncFunctionDeclaration and GeneratorFunctionDeclaration are never
     allowed as statements. We still parse them as statements (and raise an error) to
     recover gracefully. *)
  let function_as_statement env =
    let func = Declaration._function env in
    ( if in_strict_mode env then
      function_as_statement_error_at env (fst func)
    else
      let open Ast.Statement in
      match func with
      | (loc, FunctionDeclaration { Ast.Function.async = true; _ }) ->
        error_at env (loc, Parse_error.AsyncFunctionAsStatement)
      | (loc, FunctionDeclaration { Ast.Function.generator = true; _ }) ->
        error_at env (loc, Parse_error.GeneratorFunctionAsStatement)
      | _ -> ()
    );
    func

  let string_literal env (loc, value, raw, octal) =
    if octal then strict_error env Parse_error.StrictOctalLiteral;
    let leading = Peek.comments env in
    Expect.token env (T_STRING (loc, value, raw, octal));
    let trailing = Eat.trailing_comments env in
    ( loc,
      { StringLiteral.value; raw; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
    )

  (* Semicolon insertion is handled here :(. There seem to be 2 cases where
   * semicolons are inserted. First, if we reach the EOF. Second, if the next
   * token is } or is separated by a LineTerminator.
   *)
  let semicolon ?(expected = "the token `;`") ?(required = true) env =
    match Peek.token env with
    | T_EOF
    | T_RCURLY ->
      Implicit { trailing = Eat.trailing_comments env; remove_trailing = (fun x _ -> x) }
    | T_SEMICOLON ->
      Eat.token env;
      (match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        Explicit (Eat.trailing_comments env)
      | _ when Peek.is_line_terminator env -> Explicit (Eat.comments_until_next_line env)
      | _ -> Explicit [])
    | _ when Peek.is_line_terminator env ->
      Implicit (Comment_attachment.trailing_and_remover_after_last_line env)
    | _ ->
      if required then error_unexpected ~expected env;
      Explicit []

  (* Consumes and returns the trailing comments after the end of a statement. Also returns
     a remover that can remove all comments that are not trailing the previous token.

     If a statement is the end of a block or file, all comments are trailing.
     Otherwise, if a statement is followed by a new line, only comments on the current
     line are trailing. If a statement is not followed by a new line, it does not have
     trailing comments as they are instead leading comments for the next statement. *)
  let statement_end_trailing_comments env =
    match Peek.token env with
    | T_EOF
    | T_RCURLY ->
      { trailing = Eat.trailing_comments env; remove_trailing = (fun x _ -> x) }
    | _ when Peek.is_line_terminator env ->
      Comment_attachment.trailing_and_remover_after_last_line env
    | _ -> Comment_attachment.trailing_and_remover_after_last_loc env

  let variable_declaration_end ~kind env declarations =
    match semicolon env with
    | Explicit comments -> (comments, declarations)
    | Implicit { remove_trailing; _ } ->
      (* Remove trailing comments from the last declarator *)
      let declarations =
        match List.rev declarations with
        | [] -> []
        | decl :: decls ->
          let decl' =
            remove_trailing decl (fun remover decl -> remover#variable_declarator ~kind decl)
          in
          List.rev (decl' :: decls)
      in
      ([], declarations)

  let rec empty env =
    let loc = Peek.loc env in
    let leading = Peek.comments env in
    Expect.token env T_SEMICOLON;
    let { trailing; _ } = statement_end_trailing_comments env in
    ( loc,
      Statement.Empty
        { Statement.Empty.comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
    )

  and break env =
    let leading = Peek.comments env in
    let (loc, (label, trailing)) =
      with_loc
        (fun env ->
          Expect.token env T_BREAK;
          let label =
            if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env then
              None
            else
              let ((label_loc, { Identifier.name; comments = _ }) as label) =
                Parse.identifier env
              in
              if not (SSet.mem name (labels env)) then
                error_at env (label_loc, Parse_error.UnknownLabel name);
              Some label
          in
          let (trailing, label) =
            match (semicolon env, label) with
            | (Explicit trailing, _)
            | (Implicit { trailing; _ }, None) ->
              (trailing, label)
            | (Implicit { remove_trailing; _ }, Some label) ->
              ([], Some (remove_trailing label (fun remover label -> remover#identifier label)))
          in
          (label, trailing))
        env
    in
    if label = None && not (in_loop env || in_switch env) then
      error_at env (loc, Parse_error.IllegalBreak);
    let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
    (loc, Statement.Break { Statement.Break.label; comments })

  and continue env =
    let leading = Peek.comments env in
    let (loc, (label, trailing)) =
      with_loc
        (fun env ->
          Expect.token env T_CONTINUE;
          let label =
            if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env then
              None
            else
              let ((label_loc, { Identifier.name; comments = _ }) as label) =
                Parse.identifier env
              in
              if not (SSet.mem name (labels env)) then
                error_at env (label_loc, Parse_error.UnknownLabel name);
              Some label
          in
          let (trailing, label) =
            match (semicolon env, label) with
            | (Explicit trailing, _)
            | (Implicit { trailing; _ }, None) ->
              (trailing, label)
            | (Implicit { remove_trailing; _ }, Some label) ->
              ([], Some (remove_trailing label (fun remover label -> remover#identifier label)))
          in
          (label, trailing))
        env
    in
    if not (in_loop env) then error_at env (loc, Parse_error.IllegalContinue);
    ( loc,
      Statement.Continue
        {
          Statement.Continue.label;
          comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
        }
    )

  and debugger =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DEBUGGER;
        let pre_semicolon_trailing =
          if Peek.token env = T_SEMICOLON then
            Eat.trailing_comments env
          else
            []
        in
        let trailing =
          match semicolon env with
          | Explicit trailing
          | Implicit { trailing; _ } ->
            pre_semicolon_trailing @ trailing
        in
        Statement.Debugger
          { Statement.Debugger.comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
    )

  and do_while =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DO;
        let body = Parse.statement (env |> with_in_loop true) in
        (* Annex B allows labelled FunctionDeclarations (see
           sec-labelled-function-declarations), but not in IterationStatement
           (see sec-semantics-static-semantics-early-errors). *)
        if (not (in_strict_mode env)) && is_labelled_function body then
          function_as_statement_error_at env (fst body);
        let pre_keyword_trailing = Eat.trailing_comments env in
        Expect.token env T_WHILE;
        let pre_cond_trailing = Eat.trailing_comments env in
        Expect.token env T_LPAREN;
        let test = Parse.expression env in
        Expect.token env T_RPAREN;
        let past_cond_trailing =
          if Peek.token env = T_SEMICOLON then
            Eat.trailing_comments env
          else
            []
        in
        (* The rules of automatic semicolon insertion in ES5 don't mention this,
         * but the semicolon after a do-while loop is optional. This is properly
         * specified in ES6 *)
        let past_cond_trailing =
          match semicolon ~required:false env with
          | Explicit trailing -> past_cond_trailing @ trailing
          | Implicit { trailing; _ } -> trailing
        in
        let trailing = pre_keyword_trailing @ pre_cond_trailing @ past_cond_trailing in
        Statement.DoWhile
          {
            Statement.DoWhile.body;
            test;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
    )

  and for_ =
    let assert_can_be_forin_or_forof env err = function
      | (loc, { Statement.VariableDeclaration.declarations; _ }) ->
        (* Only a single declarator is allowed, without an init. So
         * something like
         *
         * for (var x in y) {}
         *
         * is allowed, but we disallow
         *
         * for (var x, y in z) {}
         * for (var x = 42 in y) {}
         *)
        (match declarations with
        | [(_, { Statement.VariableDeclaration.Declarator.init = None; _ })] -> ()
        | _ -> error_at env (loc, err))
    in
    (* Annex B allows labelled FunctionDeclarations (see
       sec-labelled-function-declarations), but not in IterationStatement
       (see sec-semantics-static-semantics-early-errors). *)
    let assert_not_labelled_function env body =
      if (not (in_strict_mode env)) && is_labelled_function body then
        function_as_statement_error_at env (fst body)
      else
        ()
    in
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_FOR;
        let async = allow_await env && Eat.maybe env T_AWAIT in
        let leading = leading @ Peek.comments env in
        Expect.token env T_LPAREN;
        let comments = Flow_ast_utils.mk_comments_opt ~leading () in
        let init_starts_with_async =
          match Peek.token env with
          | T_ASYNC -> true
          | _ -> false
        in
        let (init, errs) =
          let env = env |> with_no_in true in
          match Peek.token env with
          | T_SEMICOLON -> (None, [])
          | T_LET when Peek.ith_token env ~i:1 <> T_IN ->
            let (loc, (declarations, leading, errs)) = with_loc Declaration.let_ env in
            ( Some
                (For_declaration
                   ( loc,
                     {
                       Statement.VariableDeclaration.kind = Variable.Let;
                       declarations;
                       comments = Flow_ast_utils.mk_comments_opt ~leading ();
                     }
                   )
                ),
              errs
            )
          | T_CONST ->
            let (loc, (declarations, leading, errs)) = with_loc Declaration.const env in
            ( Some
                (For_declaration
                   ( loc,
                     {
                       Statement.VariableDeclaration.kind = Variable.Const;
                       declarations;
                       comments = Flow_ast_utils.mk_comments_opt ~leading ();
                     }
                   )
                ),
              errs
            )
          | T_VAR ->
            let (loc, (declarations, leading, errs)) = with_loc Declaration.var env in
            ( Some
                (For_declaration
                   ( loc,
                     {
                       Statement.VariableDeclaration.kind = Variable.Var;
                       declarations;
                       comments = Flow_ast_utils.mk_comments_opt ~leading ();
                     }
                   )
                ),
              errs
            )
          | _ ->
            let expr = Parse.expression_or_pattern env in
            (Some (For_expression expr), [])
        in
        match Peek.token env with
        | T_OF ->
          (* This is a for of loop *)
          let left =
            match init with
            | Some (For_declaration decl) ->
              assert_can_be_forin_or_forof env Parse_error.InvalidLHSInForOf decl;
              Statement.ForOf.LeftDeclaration decl
            | Some (For_expression expr) ->
              (* #sec-for-in-and-for-of-statements-static-semantics-early-errors *)
              let patt = Pattern_cover.as_pattern ~err:Parse_error.InvalidLHSInForOf env expr in
              (match ((not async) && init_starts_with_async, patt) with
              | ( true,
                  ( _,
                    Pattern.Identifier
                      {
                        Pattern.Identifier.name =
                          (id_loc, { Identifier.name = "async"; comments = _ });
                        annot = _;
                        optional = _;
                      }
                  )
                ) ->
                (* #prod-nLtPS4oB - `for (async of ...)` is forbidden because it is
                   ambiguous whether it's a for-of with an `async` identifier, or a
                   regular for loop with an async arrow function with a param named
                   `of`. We can backtrack, so we know it's a for-of, but the spec
                   still disallows it. *)
                error_at env (id_loc, Parse_error.InvalidLHSInForOf)
              | _ -> ());
              Statement.ForOf.LeftPattern patt
            | None -> assert false
          in
          Expect.token env T_OF;
          let right = Parse.assignment env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.ForOf { Statement.ForOf.left; right; body; await = async; comments }
        | T_IN ->
          (* This is a for in loop *)
          let left =
            match init with
            | Some (For_declaration decl) ->
              assert_can_be_forin_or_forof env Parse_error.InvalidLHSInForIn decl;
              Statement.ForIn.LeftDeclaration decl
            | Some (For_expression expr) ->
              (* #sec-for-in-and-for-of-statements-static-semantics-early-errors *)
              let patt = Pattern_cover.as_pattern ~err:Parse_error.InvalidLHSInForIn env expr in
              Statement.ForIn.LeftPattern patt
            | None -> assert false
          in
          if async then
            (* If `async` is true, this should have been a for-await-of loop, but we
               recover by trying to parse like a for-in loop. *)
            Expect.token env T_OF
          else
            Expect.token env T_IN;
          let right = Parse.expression env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.ForIn { Statement.ForIn.left; right; body; each = false; comments }
        | _ ->
          (* This is a for loop *)
          errs |> List.iter (error_at env);
          if async then
            (* If `async` is true, this should have been a for-await-of loop, but we
               recover by trying to parse like a normal loop. *)
            Expect.token env T_OF
          else
            Expect.token env T_SEMICOLON;
          let init =
            match init with
            | Some (For_declaration decl) -> Some (Statement.For.InitDeclaration decl)
            | Some (For_expression expr) ->
              Some (Statement.For.InitExpression (Pattern_cover.as_expression env expr))
            | None -> None
          in
          let test =
            match Peek.token env with
            | T_SEMICOLON -> None
            | _ -> Some (Parse.expression env)
          in
          Expect.token env T_SEMICOLON;
          let update =
            match Peek.token env with
            | T_RPAREN -> None
            | _ -> Some (Parse.expression env)
          in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.For { Statement.For.init; test; update; body; comments }
    )

  and if_ =
    (*
     * Either the consequent or alternate of an if statement
     *)
    let if_branch env =
      (* Normally this would just be a Statement, but Annex B allows
         FunctionDeclarations in non-strict mode. See
         sec-functiondeclarations-in-ifstatement-statement-clauses *)
      let stmt =
        if Peek.is_function env then
          function_as_statement env
        else
          Parse.statement env
      in
      (* Annex B allows labelled FunctionDeclarations in non-strict mode
         (see sec-labelled-function-declarations), but not in IfStatement
         (see sec-if-statement-static-semantics-early-errors). *)
      if (not (in_strict_mode env)) && is_labelled_function stmt then
        function_as_statement_error_at env (fst stmt);

      stmt
    in
    let alternate env =
      let leading = Peek.comments env in
      Expect.token env T_ELSE;
      let body = if_branch env in
      { Statement.If.Alternate.body; comments = Flow_ast_utils.mk_comments_opt ~leading () }
    in
    with_loc (fun env ->
        let pre_if_leading = Peek.comments env in
        Expect.token env T_IF;
        let pre_cond_leading = Peek.comments env in
        let leading = pre_if_leading @ pre_cond_leading in
        Expect.token env T_LPAREN;
        let test = Parse.expression env in
        Expect.token env T_RPAREN;
        let consequent = if_branch env in
        let alternate =
          if Peek.token env = T_ELSE then
            Some (with_loc alternate env)
          else
            None
        in
        Statement.If
          {
            Statement.If.test;
            consequent;
            alternate;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          }
    )

  and return =
    with_loc (fun env ->
        if not (in_function env) then error env Parse_error.IllegalReturn;
        let leading = Peek.comments env in
        let start_loc = Peek.loc env in
        Expect.token env T_RETURN;
        let trailing =
          if Peek.token env = T_SEMICOLON then
            Eat.trailing_comments env
          else
            []
        in
        let argument =
          if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env then
            None
          else
            Some (Parse.expression env)
        in
        let return_out = Loc.btwn start_loc (Peek.loc env) in
        let (trailing, argument) =
          match (semicolon env, argument) with
          | (Explicit comments, _)
          | (Implicit { trailing = comments; _ }, None) ->
            (trailing @ comments, argument)
          | (Implicit { remove_trailing; _ }, Some arg) ->
            (trailing, Some (remove_trailing arg (fun remover arg -> remover#expression arg)))
        in
        Statement.Return
          {
            Statement.Return.argument;
            return_out;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
    )

  and switch =
    let case ~seen_default env =
      let leading = Peek.comments env in
      let (test, trailing) =
        match Peek.token env with
        | T_DEFAULT ->
          if seen_default then error env Parse_error.MultipleDefaultsInSwitch;
          Expect.token env T_DEFAULT;
          (None, Eat.trailing_comments env)
        | _ ->
          Expect.token env T_CASE;
          (Some (Parse.expression env), [])
      in
      let seen_default = seen_default || test = None in
      Expect.token env T_COLON;
      let { trailing = line_end_trailing; _ } = statement_end_trailing_comments env in
      let trailing = trailing @ line_end_trailing in
      let term_fn = function
        | T_RCURLY
        | T_DEFAULT
        | T_CASE ->
          true
        | _ -> false
      in
      let consequent = Parse.statement_list ~term_fn (env |> with_in_switch true) in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      let case = { Statement.Switch.Case.test; consequent; comments } in
      (case, seen_default)
    in
    let rec case_list env (seen_default, acc) =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        List.rev acc
      | _ ->
        let (case_, seen_default) = with_loc_extra (case ~seen_default) env in
        let acc = case_ :: acc in
        case_list env (seen_default, acc)
    in
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_SWITCH;
        Expect.token env T_LPAREN;
        let discriminant = Parse.expression env in
        Expect.token env T_RPAREN;
        Expect.token env T_LCURLY;
        let cases = case_list env (false, []) in
        Expect.token env T_RCURLY;
        let { trailing; _ } = statement_end_trailing_comments env in
        Statement.Switch
          {
            Statement.Switch.discriminant;
            cases;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            exhaustive_out = fst discriminant;
          }
    )

  and match_statement env =
    let open Match in
    let case env =
      let leading = Peek.comments env in
      let pattern = Parse.match_pattern env in
      let guard =
        if Eat.maybe env T_IF then (
          Expect.token env T_LPAREN;
          let test = Parse.expression env in
          Expect.token env T_RPAREN;
          Some test
        ) else
          None
      in
      (* Continue parsing colon until hermes-parser is also updated. *)
      if not @@ Eat.maybe env T_COLON then Expect.token env T_ARROW;
      let body = Parse.statement ~allow_sequence:false env in
      (match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        ()
      | _ -> ignore @@ Eat.maybe env T_COMMA);
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      { Case.pattern; body; guard; comments }
    in
    let rec case_list env acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        List.rev acc
      | _ -> case_list env (with_loc case env :: acc)
    in
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        let match_keyword_loc = Peek.loc env in
        Expect.token env T_MATCH;
        if Peek.is_line_terminator env then raise Try.Rollback;
        let args = Expression.arguments env in
        if Peek.is_line_terminator env || not (Eat.maybe env T_LCURLY) then raise Try.Rollback;
        let arg = Parser_common.reparse_arguments_as_match_argument env args in
        let cases = case_list env [] in
        Expect.token env T_RCURLY;
        let trailing = Eat.trailing_comments env in
        Statement.Match
          {
            arg;
            cases;
            match_keyword_loc;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          })
      env

  and throw =
    with_loc (fun env ->
        let leading = Peek.comments env in
        let start_loc = Peek.loc env in
        Expect.token env T_THROW;
        if Peek.is_line_terminator env then error_at env (start_loc, Parse_error.NewlineAfterThrow);
        let argument = Parse.expression env in
        let (trailing, argument) =
          match semicolon env with
          | Explicit trailing -> (trailing, argument)
          | Implicit { remove_trailing; _ } ->
            ([], remove_trailing argument (fun remover arg -> remover#expression arg))
        in
        let open Statement in
        Throw { Throw.argument; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
    )

  and try_ =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_TRY;
        let block =
          let block = Parse.block_body env in
          if Peek.token env = T_CATCH then
            block_remove_trailing env block
          else
            block
        in
        let handler =
          match Peek.token env with
          | T_CATCH ->
            let catch =
              with_loc
                (fun env ->
                  let leading = Peek.comments env in
                  Expect.token env T_CATCH;
                  let trailing = Eat.trailing_comments env in
                  let param =
                    if Peek.token env = T_LPAREN then (
                      Expect.token env T_LPAREN;
                      let p = Some (Parse.pattern env Parse_error.StrictCatchVariable) in
                      Expect.token env T_RPAREN;
                      p
                    ) else
                      None
                  in
                  let body = Parse.block_body env in
                  (* Fix trailing comment attachment if catch block is end of statement *)
                  let body =
                    if Peek.token env <> T_FINALLY then
                      let { remove_trailing; _ } = statement_end_trailing_comments env in
                      remove_trailing body (fun remover (loc, body) -> (loc, remover#block loc body))
                    else
                      body
                  in
                  {
                    Ast.Statement.Try.CatchClause.param;
                    body;
                    comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
                  })
                env
            in
            Some catch
          | _ -> None
        in
        let finalizer =
          match Peek.token env with
          | T_FINALLY ->
            Expect.token env T_FINALLY;
            let (loc, body) = Parse.block_body env in
            let { remove_trailing; _ } = statement_end_trailing_comments env in
            let body = remove_trailing body (fun remover body -> remover#block loc body) in
            Some (loc, body)
          | _ -> None
        in
        (* No catch or finally? That's an error! *)
        if handler = None && finalizer = None then
          error_at env (fst block, Parse_error.NoCatchOrFinally);

        Statement.Try
          {
            Statement.Try.block;
            handler;
            finalizer;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          }
    )

  and var =
    with_loc (fun env ->
        let kind = Variable.Var in
        let (declarations, leading, errs) = Declaration.var env in
        let (trailing, declarations) = variable_declaration_end ~kind env declarations in
        errs |> List.iter (error_at env);
        Statement.VariableDeclaration
          {
            Statement.VariableDeclaration.kind;
            declarations;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
    )

  and const =
    with_loc (fun env ->
        let kind = Variable.Const in
        let (declarations, leading, errs) = Declaration.const env in
        let (trailing, declarations) = variable_declaration_end ~kind env declarations in
        errs |> List.iter (error_at env);
        Statement.VariableDeclaration
          {
            Statement.VariableDeclaration.kind;
            declarations;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
    )

  and let_ =
    with_loc (fun env ->
        let kind = Variable.Let in
        let (declarations, leading, errs) = Declaration.let_ env in
        let (trailing, declarations) = variable_declaration_end ~kind env declarations in
        errs |> List.iter (error_at env);
        Statement.VariableDeclaration
          {
            Statement.VariableDeclaration.kind;
            declarations;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
    )

  and while_ =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_WHILE;
        let leading = leading @ Peek.comments env in
        Expect.token env T_LPAREN;
        let test = Parse.expression env in
        Expect.token env T_RPAREN;
        let body = Parse.statement (env |> with_in_loop true) in
        (* Annex B allows labelled FunctionDeclarations in non-strict mode
           (see sec-labelled-function-declarations), but not in IterationStatement
           (see sec-semantics-static-semantics-early-errors). *)
        if (not (in_strict_mode env)) && is_labelled_function body then
          function_as_statement_error_at env (fst body);
        Statement.While
          { Statement.While.test; body; comments = Flow_ast_utils.mk_comments_opt ~leading () }
    )

  and with_ env =
    let (loc, stmt) =
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token env T_WITH;
          let leading = leading @ Peek.comments env in
          Expect.token env T_LPAREN;
          let _object = Parse.expression env in
          Expect.token env T_RPAREN;
          let body = Parse.statement env in
          (* Annex B allows labelled FunctionDeclarations in non-strict mode
             (see sec-labelled-function-declarations), but not in WithStatement
             (see sec-with-statement-static-semantics-early-errors). *)
          if (not (in_strict_mode env)) && is_labelled_function body then
            function_as_statement_error_at env (fst body);
          Statement.With
            { Statement.With._object; body; comments = Flow_ast_utils.mk_comments_opt ~leading () })
        env
    in
    strict_error_at env (loc, Parse_error.StrictModeWith);
    (loc, stmt)

  and block env =
    let (loc, block) = Parse.block_body env in
    let { remove_trailing; _ } = statement_end_trailing_comments env in
    let block = remove_trailing block (fun remover block -> remover#block loc block) in
    (loc, Statement.Block block)

  and maybe_labeled =
    with_loc (fun env ->
        let leading = Peek.comments env in
        match (Parse.expression env, Peek.token env) with
        | ((loc, Ast.Expression.Identifier label), T_COLON) ->
          let (_, { Identifier.name; comments = _ }) = label in
          Expect.token env T_COLON;
          if SSet.mem name (labels env) then
            error_at env (loc, Parse_error.Redeclaration ("Label", name));
          let env = add_label env name in
          let body =
            (* labelled FunctionDeclarations are allowed in non-strict mode
               (see #sec-labelled-function-declarations) *)
            if Peek.is_function env then
              function_as_statement env
            else
              Parse.statement env
          in
          Statement.Labeled
            { Statement.Labeled.label; body; comments = Flow_ast_utils.mk_comments_opt ~leading () }
        | (expression, _) ->
          let (trailing, expression) =
            match semicolon ~expected:"the end of an expression statement (`;`)" env with
            | Explicit comments -> (comments, expression)
            | Implicit { remove_trailing; _ } ->
              ([], remove_trailing expression (fun remover expr -> remover#expression expr))
          in
          let open Statement in
          Expression
            {
              Expression.expression;
              directive = None;
              comments = Flow_ast_utils.mk_comments_opt ~trailing ();
            }
    )

  and expression ?(allow_sequence = true) =
    with_loc (fun env ->
        let expression =
          if allow_sequence then
            Parse.expression env
          else
            Parse.assignment env
        in
        let (trailing, expression) =
          match semicolon ~expected:"the end of an expression statement (`;`)" env with
          | Explicit comments -> (comments, expression)
          | Implicit { remove_trailing; _ } ->
            ([], remove_trailing expression (fun remover expr -> remover#expression expr))
        in
        let directive =
          if allow_directive env then
            match expression with
            | (_, Ast.Expression.StringLiteral { Ast.StringLiteral.raw; _ }) ->
              (* the parser may recover from errors and generate unclosed strings, where
                 the opening quote should be reliable but the closing one might not exist.
                 be defensive. *)
              if String.length raw > 1 && raw.[0] = raw.[String.length raw - 1] then
                Some (String.sub raw 1 (String.length raw - 2))
              else
                None
            | _ -> None
          else
            None
        in
        Statement.Expression
          {
            Statement.Expression.expression;
            directive;
            comments = Flow_ast_utils.mk_comments_opt ~trailing ();
          }
    )

  and type_alias_helper ~leading env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAlias;
    let leading = leading @ Peek.comments env in
    Expect.token env T_TYPE;
    Eat.push_lex_mode env Lex_mode.TYPE;
    let id =
      let id = Type.type_identifier env in
      if Peek.token env = T_LESS_THAN then
        id_remove_trailing env id
      else
        id
    in
    let tparams = Type.type_params env in
    Expect.token env T_ASSIGN;
    let right = Type._type env in
    Eat.pop_lex_mode env;
    let (trailing, right) =
      match semicolon env with
      | Explicit comments -> (comments, right)
      | Implicit { remove_trailing; _ } ->
        ([], remove_trailing right (fun remover right -> remover#type_ right))
    in

    {
      Statement.TypeAlias.id;
      tparams;
      right;
      comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
    }

  and declare_type_alias env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let type_alias = type_alias_helper ~leading env in
        Statement.DeclareTypeAlias type_alias)
      env

  (** Type aliases squeeze into an unambiguous unused portion of the grammar: `type` is not a
      reserved word, so `type T` is otherwise two identifiers in a row and that's never valid JS.
      However, if there's a line separator between the two, ASI makes it valid JS, so line
      separators are disallowed. *)
  and type_alias env =
    if Peek.ith_is_identifier ~i:1 env && not (Peek.ith_is_implicit_semicolon ~i:1 env) then
      let (loc, type_alias) = with_loc (type_alias_helper ~leading:[]) env in
      (loc, Statement.TypeAlias type_alias)
    else
      Parse.statement env

  and opaque_type_helper ?(declare = false) ~leading env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedOpaqueTypeAlias;
    let leading_opaque = leading @ Peek.comments env in
    Expect.token env T_OPAQUE;
    let leading_type = Peek.comments env in
    Expect.token env T_TYPE;
    let leading = leading_opaque @ leading_type in
    Eat.push_lex_mode env Lex_mode.TYPE;
    let id =
      let id = Type.type_identifier env in
      if Peek.token env = T_LESS_THAN then
        id_remove_trailing env id
      else
        id
    in
    let tparams = Type.type_params env in
    let supertype =
      match Peek.token env with
      | T_COLON ->
        Expect.token env T_COLON;
        Some (Type._type env)
      | _ -> None
    in
    let impltype =
      if declare then
        match Peek.token env with
        | T_ASSIGN ->
          error env Parse_error.DeclareOpaqueTypeInitializer;
          Eat.token env;
          if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env then
            None
          else
            Some (Type._type env)
        | _ -> None
      else (
        Expect.token env T_ASSIGN;
        Some (Type._type env)
      )
    in
    Eat.pop_lex_mode env;
    let (trailing, id, tparams, supertype, impltype) =
      match (semicolon env, tparams, supertype, impltype) with
      (* opaque type Foo = Bar; *)
      | (Explicit comments, _, _, _) -> (comments, id, tparams, supertype, impltype)
      (* opaque type Foo = Bar *)
      | (Implicit { remove_trailing; _ }, _, _, Some impl) ->
        ( [],
          id,
          tparams,
          supertype,
          Some (remove_trailing impl (fun remover impl -> remover#type_ impl))
        )
      (* opaque type Foo: Super *)
      | (Implicit { remove_trailing; _ }, _, Some super, None) ->
        ( [],
          id,
          tparams,
          Some (remove_trailing super (fun remover super -> remover#type_ super)),
          None
        )
      (* opaque type Foo<T> *)
      | (Implicit { remove_trailing; _ }, Some tparams, None, None) ->
        ( [],
          id,
          Some
            (remove_trailing tparams (fun remover tparams ->
                 remover#type_params ~kind:Flow_ast_mapper.OpaqueTypeTP tparams
             )
            ),
          None,
          None
        )
      (* declare opaque type Foo *)
      | (Implicit { remove_trailing; _ }, None, None, None) ->
        ([], remove_trailing id (fun remover id -> remover#identifier id), None, None, None)
    in

    {
      Statement.OpaqueType.id;
      tparams;
      impltype;
      supertype;
      comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
    }

  and declare_opaque_type env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let opaque_t = opaque_type_helper ~declare:true ~leading env in
        Statement.DeclareOpaqueType opaque_t)
      env

  and opaque_type env =
    match Peek.ith_token ~i:1 env with
    | T_TYPE ->
      let (loc, opaque_t) = with_loc (opaque_type_helper ~declare:false ~leading:[]) env in
      (loc, Statement.OpaqueType opaque_t)
    | _ -> Parse.statement env

  and interface_helper ~leading env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeInterface;
    let leading = leading @ Peek.comments env in
    Expect.token env T_INTERFACE;
    let id =
      let id = Type.type_identifier env in
      if Peek.token env = T_EXTENDS then
        id
      else
        id_remove_trailing env id
    in
    let tparams =
      let tparams = Type.type_params env in
      if Peek.token env = T_EXTENDS then
        tparams
      else
        type_params_remove_trailing env ~kind:Flow_ast_mapper.InterfaceTP tparams
    in
    let (extends, body) = Type.interface_helper env in
    let { remove_trailing; _ } = statement_end_trailing_comments env in
    let body =
      remove_trailing body (fun remover (loc, body) -> (loc, remover#object_type loc body))
    in

    {
      Statement.Interface.id;
      tparams;
      body;
      extends;
      comments = Flow_ast_utils.mk_comments_opt ~leading ();
    }

  and declare_interface env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let iface = interface_helper ~leading env in
        Statement.DeclareInterface iface)
      env

  and interface env =
    (* disambiguate between a value named `interface`, like `var interface = 1; interface++`,
       and an interface declaration like `interface Foo {}`.` *)
    if Peek.ith_is_identifier_name ~i:1 env then
      let (loc, iface) = with_loc (interface_helper ~leading:[]) env in
      (loc, Statement.InterfaceDeclaration iface)
    else
      expression env

  and declare_class =
    let rec mixins env acc =
      let super = Type.generic env in
      let acc = super :: acc in
      match Peek.token env with
      | T_COMMA ->
        Expect.token env T_COMMA;
        mixins env acc
      | _ -> List.rev acc
      (* This is identical to `interface`, except that mixins are allowed *)
    in
    fun ~leading env ->
      let env = env |> with_strict true in
      let leading = leading @ Peek.comments env in
      Expect.token env T_CLASS;
      let id =
        let id = Parse.identifier env in
        match Peek.token env with
        | T_LESS_THAN
        | T_LCURLY ->
          id_remove_trailing env id
        | _ -> id
      in
      let tparams =
        let tparams = Type.type_params env in
        match Peek.token env with
        | T_LCURLY -> type_params_remove_trailing env ~kind:Flow_ast_mapper.DeclareClassTP tparams
        | _ -> tparams
      in
      let extends =
        if Eat.maybe env T_EXTENDS then
          let extends = Type.generic env in
          match Peek.token env with
          | T_LCURLY -> Some (generic_type_remove_trailing env extends)
          | _ -> Some extends
        else
          None
      in
      let mixins =
        match Peek.token env with
        | T_IDENTIFIER { raw = "mixins"; _ } ->
          Eat.token env;
          let mixins = mixins env [] in
          (match Peek.token env with
          | T_LCURLY -> generic_type_list_remove_trailing env mixins
          | _ -> mixins)
        | _ -> []
      in
      let implements =
        match Peek.token env with
        | T_IMPLEMENTS ->
          let implements = Object.class_implements env ~attach_leading:false in
          (match Peek.token env with
          | T_LCURLY -> Some (class_implements_remove_trailing env implements)
          | _ -> Some implements)
        | _ -> None
      in
      let body = Type._object ~is_class:true env in
      let { remove_trailing; _ } = statement_end_trailing_comments env in
      let body =
        remove_trailing body (fun remover (loc, body) -> (loc, remover#object_type loc body))
      in
      let comments = Flow_ast_utils.mk_comments_opt ~leading () in
      Statement.DeclareClass.{ id; tparams; body; extends; mixins; implements; comments }

  and declare_class_statement env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let fn = declare_class ~leading env in
        Statement.DeclareClass fn)
      env

  and declare_component ~leading env =
    let leading = leading @ Peek.comments env in
    Expect.identifier env "component";
    let id =
      id_remove_trailing
        env
        (* Components should have at least the same strictness as functions *)
        (Parse.identifier ~restricted_error:Parse_error.StrictFunctionName env)
    in
    let tparams =
      type_params_remove_trailing env ~kind:Flow_ast_mapper.DeclareComponentTP (Type.type_params env)
    in
    let params = Type.component_param_list env in
    let (params, renders) =
      if Peek.is_renders_ident env then
        let renders = Type.renders_annotation_opt env in
        let renders = component_renders_annotation_remove_trailing env renders in
        (params, renders)
      else
        let missing_annotation = Type.renders_annotation_opt env in
        (params, missing_annotation)
    in

    let (trailing, renders) =
      match semicolon env with
      | Explicit comments -> (comments, renders)
      | Implicit { remove_trailing; _ } ->
        ( [],
          remove_trailing renders (fun remover annot -> remover#component_renders_annotation annot)
        )
    in
    {
      Statement.DeclareComponent.id;
      params;
      renders;
      tparams;
      comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
    }

  and declare_component_statement env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let component = declare_component ~leading env in
        Statement.DeclareComponent component)
      env

  and declare_enum env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let enum = Enum.declaration ~leading env in
        Statement.DeclareEnum enum)
      env

  and declare_function ~async ?(leading = []) env =
    let leading = leading @ Peek.comments env in
    let effect_ =
      match Peek.token env with
      | T_FUNCTION ->
        Eat.token env;
        Function.Arbitrary
      | T_IDENTIFIER { raw = "hook"; _ } when not async ->
        Eat.token env;
        Function.Hook
      | t ->
        Expect.error env t;
        Function.Arbitrary
    in
    let id = id_remove_trailing env (Parse.identifier env) in
    let annot =
      with_loc
        (fun env ->
          let tparams =
            type_params_remove_trailing
              env
              ~kind:Flow_ast_mapper.DeclareFunctionTP
              (Type.type_params env)
          in
          let params = Type.function_param_list env in
          Expect.token env T_COLON;
          Eat.push_lex_mode env Lex_mode.TYPE;
          let return =
            if is_start_of_type_guard env && effect_ <> Function.Hook then
              Ast.Type.Function.TypeGuard (Type.type_guard env)
            else
              let return = Type._type env in
              let has_predicate = Peek.token env = T_CHECKS in
              if has_predicate && effect_ <> Function.Hook then
                Ast.Type.Function.TypeAnnotation (type_remove_trailing env return)
              else
                Ast.Type.Function.TypeAnnotation return
          in
          Eat.pop_lex_mode env;
          Ast.Type.(Function { Function.params; return; tparams; comments = None; effect_ }))
        env
    in
    let predicate = Type.predicate_opt env in
    let (trailing, annot, predicate) =
      match (semicolon env, predicate) with
      | (Explicit comments, _) -> (comments, annot, predicate)
      | (Implicit { remove_trailing; _ }, None) ->
        ([], remove_trailing annot (fun remover annot -> remover#type_ annot), None)
      | (Implicit { remove_trailing; _ }, Some pred) ->
        ([], annot, Some (remove_trailing pred (fun remover pred -> remover#predicate pred)))
    in
    let annot = (fst annot, annot) in

    {
      Statement.DeclareFunction.id;
      annot;
      predicate;
      comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
    }

  and declare_function_statement env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let async =
          match Peek.token env with
          | T_ASYNC ->
            error env Parse_error.DeclareAsync;
            Expect.token env T_ASYNC;
            true
          | _ -> false
        in
        let fn = declare_function ~async ~leading env in
        Statement.DeclareFunction fn)
      env

  and declare_var ~kind env leading =
    let leading = leading @ Peek.comments env in
    (match kind with
    | Ast.Variable.Var -> Expect.token env T_VAR
    | Ast.Variable.Let -> Expect.token env T_LET
    | Ast.Variable.Const -> Expect.token env T_CONST);
    let name = Parse.identifier ~restricted_error:Parse_error.StrictVarName env in
    let annot = Type.annotation env in
    let (trailing, name, annot) =
      match semicolon env with
      (* declare var x; *)
      | Explicit trailing -> (trailing, name, annot)
      (* declare var x *)
      | Implicit { remove_trailing; _ } ->
        ([], name, remove_trailing annot (fun remover annot -> remover#type_annotation annot))
    in

    {
      Statement.DeclareVariable.id = name;
      annot;
      kind;
      comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
    }

  and declare_var_statement ~kind env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let var = declare_var ~kind env leading in
        Statement.DeclareVariable var)
      env

  and declare_module_or_namespace_body env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_LCURLY;
        let body =
          Parse.module_body
            ~term_fn:(function
              | T_RCURLY -> true
              | _ -> false)
            env
        in
        let internal =
          if body = [] then
            Peek.comments env
          else
            []
        in
        Expect.token env T_RCURLY;
        let { trailing; _ } = statement_end_trailing_comments env in
        let comments =
          Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ()
        in
        { Statement.Block.body; comments })
      env

  and declare_module =
    let declare_module_ ~leading env =
      let id =
        match Peek.token env with
        | T_STRING str ->
          Statement.DeclareModule.Literal
            (string_literal_remove_trailing env (string_literal env str))
        | _ -> Statement.DeclareModule.Identifier (id_remove_trailing env (Parse.identifier env))
      in
      let body = declare_module_or_namespace_body env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading () in
      Statement.(DeclareModule DeclareModule.{ id; body; comments })
    in
    fun env ->
      let start_loc = Peek.loc env in
      let leading = Peek.comments env in
      Expect.token env T_DECLARE;
      let leading = leading @ Peek.comments env in
      Expect.identifier env "module";
      if Peek.token env = T_PERIOD then
        with_loc ~start_loc (declare_module_exports ~leading) env
      else
        with_loc ~start_loc (declare_module_ ~leading) env

  and declare_namespace =
    let declare_namespace_ ~leading ~global env =
      let id = id_remove_trailing env (Parse.identifier env) in
      let id =
        if global then
          Statement.DeclareNamespace.Global id
        else
          Statement.DeclareNamespace.Local id
      in
      let body = declare_module_or_namespace_body env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading () in
      Statement.(DeclareNamespace DeclareNamespace.{ id; body; comments })
    in
    fun env ~global ->
      let start_loc = Peek.loc env in
      let leading = Peek.comments env in
      Expect.token env T_DECLARE;
      let leading = leading @ Peek.comments env in
      if not global then Expect.identifier env "namespace";
      with_loc ~start_loc (declare_namespace_ ~global ~leading) env

  and declare_module_exports ~leading env =
    let leading_period = Peek.comments env in
    Expect.token env T_PERIOD;
    let leading_exports = Peek.comments env in
    Expect.identifier env "exports";
    let leading_annot = Peek.comments env in
    let leading = List.concat [leading; leading_period; leading_exports; leading_annot] in
    let annot = Type.annotation env in
    let (annot, trailing) =
      match semicolon env with
      | Explicit trailing -> (annot, trailing)
      | Implicit { remove_trailing; _ } ->
        (remove_trailing annot (fun remover annot -> remover#type_annotation annot), [])
    in
    let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
    Statement.DeclareModuleExports { Statement.DeclareModuleExports.annot; comments }

  and declare ?(in_module_or_namespace = false) env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeDeclaration;

    (* eventually, just emit a wrapper AST node *)
    match Peek.ith_token ~i:1 env with
    | T_CLASS -> declare_class_statement env
    | T_ENUM when (parse_options env).enums -> declare_enum env
    | T_INTERFACE -> declare_interface env
    | T_TYPE ->
      (match Peek.token env with
      | T_IMPORT when in_module_or_namespace -> import_declaration env
      | _ -> declare_type_alias env)
    | T_OPAQUE -> declare_opaque_type env
    | T_TYPEOF when Peek.token env = T_IMPORT -> import_declaration env
    | T_FUNCTION
    | T_ASYNC ->
      declare_function_statement env
    | T_IDENTIFIER { raw = "hook"; _ } when (parse_options env).components ->
      declare_function_statement env
    | T_VAR -> declare_var_statement ~kind:Ast.Variable.Var env
    | T_LET -> declare_var_statement ~kind:Ast.Variable.Let env
    | T_CONST -> declare_var_statement ~kind:Ast.Variable.Const env
    | T_EXPORT when in_module_or_namespace -> declare_export_declaration env
    | T_IDENTIFIER { raw = "module"; _ } -> declare_module env
    | T_IDENTIFIER { raw = "global"; _ } -> declare_namespace ~global:true env
    | T_IDENTIFIER { raw = "namespace"; _ } -> declare_namespace ~global:false env
    | T_IDENTIFIER { raw = "component"; _ } when (parse_options env).components ->
      declare_component_statement env
    | _ when in_module_or_namespace ->
      (match Peek.token env with
      | T_IMPORT -> import_declaration env
      | _ ->
        (* Oh boy, found some bad stuff in a declare module. Let's just
         * pretend it's a declare var (arbitrary choice) *)
        declare_var_statement ~kind:Ast.Variable.Var env)
    | _ -> Parse.statement env

  and export_source env =
    Expect.identifier env "from";
    match Peek.token env with
    | T_STRING str -> string_literal env str
    | _ ->
      (* Just make up a string for the error case *)
      let ret = (Peek.loc env, { StringLiteral.value = ""; raw = ""; comments = None }) in
      error_unexpected ~expected:"a string" env;
      ret

  and export_source_and_semicolon env =
    let (source_loc, source) = export_source env in
    match semicolon env with
    | Explicit trailing -> ((source_loc, source), trailing)
    | Implicit { remove_trailing; _ } ->
      ( ( source_loc,
          remove_trailing source (fun remover source -> remover#string_literal source_loc source)
        ),
        []
      )

  and export_specifiers ?(preceding_comma = true) env specifiers =
    match Peek.token env with
    | T_EOF
    | T_RCURLY ->
      List.rev specifiers
    | _ ->
      if not preceding_comma then error env Parse_error.ExportSpecifierMissingComma;
      let specifier =
        with_loc
          (fun env ->
            let local = identifier_name env in
            let exported =
              match Peek.token env with
              | T_IDENTIFIER { raw = "as"; _ } ->
                Eat.token env;
                Some (identifier_name env)
              | _ -> None
            in
            {
              Statement.ExportNamedDeclaration.ExportSpecifier.local;
              exported;
              from_remote = false;
              imported_name_def_loc = None;
            })
          env
      in
      let preceding_comma = Eat.maybe env T_COMMA in
      export_specifiers ~preceding_comma env (specifier :: specifiers)

  and assert_export_specifier_identifiers env specifiers =
    List.iter
      (function
        | ( _,
            {
              Statement.ExportNamedDeclaration.ExportSpecifier.local = id;
              exported = _;
              from_remote = _;
              imported_name_def_loc = _;
            }
          ) ->
          assert_identifier_name_is_identifier ~restricted_error:Parse_error.StrictVarName env id)
      specifiers

  and export_declaration ~decorators env =
    let env = env |> with_strict true |> with_in_export true in
    let leading = Peek.comments env in
    let start_loc = Peek.loc env in
    Expect.token env T_EXPORT;
    match Peek.token env with
    | T_DEFAULT ->
      (* export default ... *)
      with_loc
        ~start_loc
        (fun env ->
          let open Statement.ExportDefaultDeclaration in
          let leading = leading @ Peek.comments env in
          let (default, ()) = with_loc (fun env -> Expect.token env T_DEFAULT) env in
          let env = with_in_export_default true env in
          let (declaration, trailing) =
            if Peek.is_function env || Peek.is_hook env then
              (* export default [async] function [foo] (...) { ... } *)
              let fn = Declaration._function env in
              (Declaration fn, [])
            else if Peek.is_class env then
              (* export default class foo { ... } *)
              let _class = Object.class_declaration env decorators in
              (Declaration _class, [])
            else if Peek.token env = T_ENUM then
              (* export default enum foo { ... } *)
              (Declaration (Declaration.enum_declaration env), [])
            else if Peek.is_component env then
              (* export default component foo { ... } *)
              (Declaration (Declaration.component env), [])
            else
              (* export default [assignment expression]; *)
              let expr = Parse.assignment env in
              let (expr, trailing) =
                match semicolon env with
                | Explicit trailing -> (expr, trailing)
                | Implicit { remove_trailing; _ } ->
                  (remove_trailing expr (fun remover expr -> remover#expression expr), [])
              in
              (Expression expr, trailing)
          in
          Statement.ExportDefaultDeclaration
            {
              default;
              declaration;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
        env
    | T_TYPE when Peek.ith_token ~i:1 env <> T_LCURLY ->
      (* export type ... *)
      with_loc
        ~start_loc
        (fun env ->
          let open Statement.ExportNamedDeclaration in
          if not (should_parse_types env) then error env Parse_error.UnexpectedTypeExport;
          match Peek.ith_token ~i:1 env with
          | T_MULT ->
            Expect.token env T_TYPE;
            let specifier_loc = Peek.loc env in
            Expect.token env T_MULT;
            let (source, trailing) = export_source_and_semicolon env in
            Statement.ExportNamedDeclaration
              {
                declaration = None;
                specifiers = Some (ExportBatchSpecifier (specifier_loc, None));
                source = Some source;
                export_kind = Statement.ExportType;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }
          | T_ENUM ->
            error env Parse_error.EnumInvalidExport;
            Expect.token env T_TYPE;
            Statement.ExportNamedDeclaration
              {
                declaration = None;
                specifiers = None;
                source = None;
                export_kind = Statement.ExportType;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              }
          | _ ->
            let (loc, type_alias) = with_loc (type_alias_helper ~leading:[]) env in
            let type_alias = (loc, Statement.TypeAlias type_alias) in
            Statement.ExportNamedDeclaration
              {
                declaration = Some type_alias;
                specifiers = None;
                source = None;
                export_kind = Statement.ExportType;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              })
        env
    | T_OPAQUE ->
      (* export opaque type ... *)
      with_loc
        ~start_loc
        (fun env ->
          let open Statement.ExportNamedDeclaration in
          let (loc, opaque_t) = with_loc (opaque_type_helper ~leading:[]) env in
          let opaque_t = (loc, Statement.OpaqueType opaque_t) in
          Statement.ExportNamedDeclaration
            {
              declaration = Some opaque_t;
              specifiers = None;
              source = None;
              export_kind = Statement.ExportType;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | T_INTERFACE ->
      (* export interface I { ... } *)
      with_loc
        ~start_loc
        (fun env ->
          let open Statement.ExportNamedDeclaration in
          if not (should_parse_types env) then error env Parse_error.UnexpectedTypeExport;
          let interface =
            let (loc, iface) = with_loc (interface_helper ~leading:[]) env in
            (loc, Statement.InterfaceDeclaration iface)
          in
          Statement.ExportNamedDeclaration
            {
              declaration = Some interface;
              specifiers = None;
              source = None;
              export_kind = Statement.ExportType;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | _ when Peek.is_class env ->
      with_loc
        ~start_loc
        (fun env ->
          let stmt = Object.class_declaration env decorators in
          Statement.ExportNamedDeclaration
            {
              Statement.ExportNamedDeclaration.declaration = Some stmt;
              specifiers = None;
              source = None;
              export_kind = Statement.ExportValue;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | _ when Peek.is_function env || Peek.is_hook env ->
      with_loc
        ~start_loc
        (fun env ->
          error_on_decorators env decorators;
          let stmt = Declaration._function env in
          Statement.ExportNamedDeclaration
            {
              Statement.ExportNamedDeclaration.declaration = Some stmt;
              specifiers = None;
              source = None;
              export_kind = Statement.ExportValue;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | T_LET
    | T_CONST
    | T_VAR ->
      with_loc
        ~start_loc
        (fun env ->
          let stmt = Parse.statement_list_item env ~decorators in
          Statement.ExportNamedDeclaration
            {
              Statement.ExportNamedDeclaration.declaration = Some stmt;
              specifiers = None;
              source = None;
              export_kind = Statement.ExportValue;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | T_ENUM when (parse_options env).enums ->
      with_loc
        ~start_loc
        (fun env ->
          let stmt = Parse.statement_list_item env ~decorators in
          Statement.ExportNamedDeclaration
            {
              Statement.ExportNamedDeclaration.declaration = Some stmt;
              specifiers = None;
              source = None;
              export_kind = Statement.ExportValue;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    (* export component *)
    | _ when Peek.is_component env ->
      with_loc
        ~start_loc
        (fun env ->
          let stmt = Declaration.component env in
          Statement.ExportNamedDeclaration
            {
              Statement.ExportNamedDeclaration.declaration = Some stmt;
              specifiers = None;
              source = None;
              export_kind = Statement.ExportValue;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | T_MULT ->
      with_loc
        ~start_loc
        (fun env ->
          let open Statement.ExportNamedDeclaration in
          let loc = Peek.loc env in
          Expect.token env T_MULT;
          let local_name =
            match Peek.token env with
            | T_IDENTIFIER { raw = "as"; _ } ->
              Eat.token env;
              Some (identifier_name env)
            | _ -> None
          in
          let specifiers = Some (ExportBatchSpecifier (loc, local_name)) in
          let (source, trailing) = export_source_and_semicolon env in
          Statement.ExportNamedDeclaration
            {
              declaration = None;
              specifiers;
              source = Some source;
              export_kind = Statement.ExportValue;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
        env
    | _ ->
      let open Statement.ExportNamedDeclaration in
      let export_kind =
        if Eat.maybe env T_TYPE then
          Statement.ExportType
        else
          Statement.ExportValue
      in
      if Eat.maybe env T_LCURLY then
        with_loc
          ~start_loc
          (fun env ->
            let specifiers = export_specifiers env [] in
            Expect.token env T_RCURLY;
            let (source, trailing, specifiers) =
              match Peek.token env with
              | T_IDENTIFIER { raw = "from"; _ } ->
                let (source, trailing) = export_source_and_semicolon env in
                let specifiers =
                  List.map
                    (fun (loc, s) ->
                      ( loc,
                        {
                          s with
                          Statement.ExportNamedDeclaration.ExportSpecifier.from_remote = true;
                        }
                      ))
                    specifiers
                in
                (Some source, trailing, specifiers)
              | _ ->
                assert_export_specifier_identifiers env specifiers;
                let trailing =
                  match semicolon env with
                  | Explicit trailing -> trailing
                  | Implicit { trailing; _ } -> trailing
                in
                (None, trailing, specifiers)
            in
            Statement.ExportNamedDeclaration
              {
                declaration = None;
                specifiers = Some (ExportSpecifiers specifiers);
                source;
                export_kind;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              })
          env
      else (
        (* error. recover by ignoring the `export` *)
        error_unexpected ~expected:"a declaration, statement or export specifiers" env;
        Parse.statement_list_item env ~decorators
      )

  and declare_export_declaration env =
    with_loc
      (fun env ->
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeDeclaration;
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let env = env |> with_strict true |> with_in_export true in
        let leading = leading @ Peek.comments env in
        Expect.token env T_EXPORT;
        Statement.DeclareExportDeclaration.(
          match Peek.token env with
          | T_DEFAULT ->
            (* declare export default ... *)
            let leading = leading @ Peek.comments env in
            let (default, ()) = with_loc (fun env -> Expect.token env T_DEFAULT) env in
            let env = with_in_export_default true env in
            let (declaration, trailing) =
              match Peek.token env with
              | T_FUNCTION ->
                (* declare export default function foo (...): ...  *)
                let fn = with_loc (declare_function ~async:false) env in
                (Some (Function fn), [])
              | T_CLASS ->
                (* declare export default class foo { ... } *)
                let class_ = with_loc (declare_class ~leading:[]) env in
                (Some (Class class_), [])
              | T_IDENTIFIER { raw = "component"; _ } when (parse_options env).components ->
                (* declare export default component Foo() { ... } *)
                let component = with_loc (declare_component ~leading:[]) env in
                (Some (Component component), [])
              | T_IDENTIFIER { raw = "hook"; _ } when (parse_options env).components ->
                (* declare export default hook foo (...): ...  *)
                let fn = with_loc (declare_function ~async:false) env in
                (Some (Function fn), [])
              | _ ->
                (* declare export default [type]; *)
                let type_ = Type._type env in
                let (type_, trailing) =
                  match semicolon env with
                  | Explicit trailing -> (type_, trailing)
                  | Implicit { remove_trailing; _ } ->
                    (remove_trailing type_ (fun remover type_ -> remover#type_ type_), [])
                in
                (Some (DefaultType type_), trailing)
            in
            let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
            Statement.DeclareExportDeclaration
              { default = Some default; declaration; specifiers = None; source = None; comments }
          | T_LET
          | T_CONST
          | T_VAR
          | T_CLASS
          | T_FUNCTION ->
            let declaration =
              match Peek.token env with
              | T_FUNCTION ->
                (* declare export function foo (...): ...  *)
                let fn = with_loc (declare_function ~async:false) env in
                Some (Function fn)
              | T_CLASS ->
                (* declare export class foo { ... } *)
                let class_ = with_loc (declare_class ~leading:[]) env in
                Some (Class class_)
              | T_VAR ->
                (* declare export var foo: ... *)
                let var = with_loc (fun env -> declare_var ~kind:Ast.Variable.Var env []) env in
                Some (Variable var)
              | T_LET ->
                (* declare export let foo: ... *)
                let var = with_loc (fun env -> declare_var ~kind:Ast.Variable.Let env []) env in
                Some (Variable var)
              | T_CONST ->
                (* declare export const foo: ... *)
                let var = with_loc (fun env -> declare_var ~kind:Ast.Variable.Const env []) env in
                Some (Variable var)
              | _ -> assert false
            in
            let comments = Flow_ast_utils.mk_comments_opt ~leading () in
            Statement.DeclareExportDeclaration
              { default = None; declaration; specifiers = None; source = None; comments }
          | T_IDENTIFIER { raw = "hook"; _ } when (parse_options env).components ->
            let declaration =
              (* declare export hook foo (...): ...  *)
              let fn = with_loc (declare_function ~async:false) env in
              Some (Function fn)
            in
            let comments = Flow_ast_utils.mk_comments_opt ~leading () in
            Statement.DeclareExportDeclaration
              { default = None; declaration; specifiers = None; source = None; comments }
          | T_IDENTIFIER { raw = "component"; _ } when (parse_options env).components ->
            let declaration =
              (* declare export component Foo() { ... } *)
              let component = with_loc (declare_component ~leading:[]) env in
              Some (Component component)
            in
            let comments = Flow_ast_utils.mk_comments_opt ~leading () in
            Statement.DeclareExportDeclaration
              { default = None; declaration; specifiers = None; source = None; comments }
          | T_MULT ->
            (* declare export * from 'foo' *)
            let loc = Peek.loc env in
            Expect.token env T_MULT;
            let local_name =
              match Peek.token env with
              | T_IDENTIFIER { raw = "as"; _ } ->
                Eat.token env;
                Some (Parse.identifier env)
              | _ -> None
            in
            let specifiers =
              Statement.ExportNamedDeclaration.(Some (ExportBatchSpecifier (loc, local_name)))
            in
            let (source, trailing) = export_source_and_semicolon env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
            Statement.DeclareExportDeclaration
              { default = None; declaration = None; specifiers; source = Some source; comments }
          | T_TYPE ->
            (* declare export type = ... *)
            let alias = with_loc (type_alias_helper ~leading:[]) env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading () in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = Some (NamedType alias);
                specifiers = None;
                source = None;
                comments;
              }
          | T_OPAQUE ->
            (* declare export opaque type = ... *)
            let opaque = with_loc (opaque_type_helper ~declare:true ~leading:[]) env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading () in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = Some (NamedOpaqueType opaque);
                specifiers = None;
                source = None;
                comments;
              }
          | T_INTERFACE ->
            (* declare export interface ... *)
            let iface = with_loc (interface_helper ~leading:[]) env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading () in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = Some (Interface iface);
                specifiers = None;
                source = None;
                comments;
              }
          | T_ENUM when (parse_options env).enums ->
            (* declare export enum ... *)
            let enum = with_loc Enum.declaration env in
            let comments = Flow_ast_utils.mk_comments_opt ~leading () in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = Some (Enum enum);
                specifiers = None;
                source = None;
                comments;
              }
          | _ ->
            Expect.token env T_LCURLY;
            let specifiers = export_specifiers env [] in
            Expect.token env T_RCURLY;
            let (source, trailing) =
              match Peek.token env with
              | T_IDENTIFIER { raw = "from"; _ } ->
                let (source, trailing) = export_source_and_semicolon env in
                (Some source, trailing)
              | _ ->
                assert_export_specifier_identifiers env specifiers;
                let trailing =
                  match semicolon env with
                  | Explicit trailing -> trailing
                  | Implicit { trailing; _ } -> trailing
                in
                (None, trailing)
            in
            let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = None;
                specifiers = Some (Statement.ExportNamedDeclaration.ExportSpecifiers specifiers);
                source;
                comments;
              }
        ))
      env

  and import_declaration =
    Statement.ImportDeclaration.(
      let missing_source env =
        (* Just make up a string for the error case *)
        let loc = Peek.loc_skip_lookahead env in
        (loc, { StringLiteral.value = ""; raw = ""; comments = None })
      in
      let source env =
        match Peek.token env with
        | T_IDENTIFIER { raw = "from"; _ } ->
          Eat.token env;
          (match Peek.token env with
          | T_STRING str -> string_literal env str
          | _ ->
            error_unexpected ~expected:"a string" env;
            missing_source env)
        | _ ->
          error_unexpected ~expected:"the keyword `from`" env;
          missing_source env
      in
      let is_type_import = function
        | T_TYPE
        | T_TYPEOF ->
          true
        | _ -> false
        (* `x` or `x as y` in a specifier *)
      in
      let with_maybe_as ~for_type ?error_if_type env =
        let identifier env =
          if for_type then
            Type.type_identifier env
          else
            Parse.identifier env
        in
        match Peek.ith_token ~i:1 env with
        | T_IDENTIFIER { raw = "as"; _ } ->
          let remote = identifier_name env in
          Eat.token env;

          (* as *)
          let local = Some (identifier env) in
          (remote, local)
        | T_EOF
        | T_COMMA
        | T_RCURLY ->
          (identifier env, None)
        | _ -> begin
          match (error_if_type, Peek.token env) with
          | (Some error_if_type, T_TYPE)
          | (Some error_if_type, T_TYPEOF) ->
            error env error_if_type;
            Eat.token env;

            (* consume `type` or `typeof` *)
            (Type.type_identifier env, None)
          | _ -> (identifier env, None)
        end
        (*
           ImportSpecifier[Type]:
             [~Type] ImportedBinding
             [~Type] IdentifierName ImportedTypeBinding
             [~Type] IdentifierName IdentifierName ImportedBinding
             [~Type] IdentifierName IdentifierName IdentifierName ImportedTypeBinding
             [+Type] ImportedTypeBinding
             [+Type] IdentifierName IdentifierName ImportedTypeBinding

           Static Semantics:

           `IdentifierName ImportedTypeBinding`:
           - It is a Syntax Error if IdentifierName's StringValue is not "type" or "typeof"

           `IdentifierName IdentifierName ImportedBinding`:
           - It is a Syntax Error if the second IdentifierName's StringValue is not "as"

           `IdentifierName IdentifierName IdentifierName  ImportedTypeBinding`:
           - It is a Syntax Error if the first IdentifierName's StringValue is not "type"
             or "typeof", and the third IdentifierName's StringValue is not "as"
        *)
      in

      let specifier env =
        let kind =
          match Peek.token env with
          | T_TYPE -> Some ImportType
          | T_TYPEOF -> Some ImportTypeof
          | _ -> None
        in
        if is_type_import (Peek.token env) then
          (* consume `type`, but we don't know yet whether this is `type foo` or
             `type as foo`. *)
          let type_keyword_or_remote = identifier_name env in
          match Peek.token env with
          (* `type` (a value) *)
          | T_EOF
          | T_RCURLY
          | T_COMMA ->
            let remote = type_keyword_or_remote in
            (* `type` becomes a value *)
            assert_identifier_name_is_identifier env remote;
            { remote; local = None; remote_name_def_loc = None; kind = None }
          (* `type as foo` (value named `type`) or `type as,` (type named `as`) *)
          | T_IDENTIFIER { raw = "as"; _ } -> begin
            match Peek.ith_token ~i:1 env with
            | T_EOF
            | T_RCURLY
            | T_COMMA ->
              (* `type as` *)
              { remote = Type.type_identifier env; remote_name_def_loc = None; local = None; kind }
            | T_IDENTIFIER { raw = "as"; _ } ->
              (* `type as as foo` *)
              let remote = identifier_name env in
              (* first `as` *)
              Eat.token env;

              (* second `as` *)
              let local = Some (Type.type_identifier env) in
              (* `foo` *)
              { remote; remote_name_def_loc = None; local; kind }
            | _ ->
              (* `type as foo` *)
              let remote = type_keyword_or_remote in
              (* `type` becomes a value *)
              assert_identifier_name_is_identifier env remote;
              Eat.token env;

              (* `as` *)
              let local = Some (Parse.identifier env) in
              { remote; remote_name_def_loc = None; local; kind = None }
          end
          (* `type x`, or `type x as y` *)
          | _ ->
            let (remote, local) = with_maybe_as ~for_type:true env in
            { remote; remote_name_def_loc = None; local; kind }
        else
          (* standard `x` or `x as y` *)
          let (remote, local) = with_maybe_as ~for_type:false env in
          { remote; remote_name_def_loc = None; local; kind = None }
        (* specifier in an `import type { ... }` *)
      in
      let type_specifier env =
        let (remote, local) =
          with_maybe_as
            env
            ~for_type:true
            ~error_if_type:Parse_error.ImportTypeShorthandOnlyInPureImport
        in
        { remote; remote_name_def_loc = None; local; kind = None }
        (* specifier in an `import typeof { ... }` *)
      in
      let typeof_specifier env =
        let (remote, local) =
          with_maybe_as
            env
            ~for_type:true
            ~error_if_type:Parse_error.ImportTypeShorthandOnlyInPureImport
        in
        { remote; remote_name_def_loc = None; local; kind = None }
      in
      let rec specifier_list ?(preceding_comma = true) env statement_kind acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY ->
          List.rev acc
        | _ ->
          if not preceding_comma then error env Parse_error.ImportSpecifierMissingComma;
          let specifier =
            match statement_kind with
            | ImportType -> type_specifier env
            | ImportTypeof -> typeof_specifier env
            | ImportValue -> specifier env
          in
          let preceding_comma = Eat.maybe env T_COMMA in
          specifier_list ~preceding_comma env statement_kind (specifier :: acc)
      in
      let named_or_namespace_specifier env import_kind =
        match Peek.token env with
        | T_MULT ->
          let id =
            with_loc_opt
              (fun env ->
                (* consume T_MULT *)
                Eat.token env;
                match Peek.token env with
                | T_IDENTIFIER { raw = "as"; _ } ->
                  (* consume "as" *)
                  Eat.token env;
                  (match import_kind with
                  | ImportType
                  | ImportTypeof ->
                    Some (Type.type_identifier env)
                  | ImportValue -> Some (Parse.identifier env))
                | _ ->
                  error_unexpected ~expected:"the keyword `as`" env;
                  None)
              env
          in
          (match id with
          | Some id -> Some (ImportNamespaceSpecifier id)
          | None -> None)
        | _ ->
          Expect.token env T_LCURLY;
          let specifiers = specifier_list env import_kind [] in
          Expect.token env T_RCURLY;
          Some (ImportNamedSpecifiers specifiers)
      in
      let semicolon_and_trailing env source =
        match semicolon env with
        | Explicit trailing -> (trailing, source)
        | Implicit { remove_trailing; _ } ->
          ( [],
            remove_trailing source (fun remover (loc, source) ->
                (loc, remover#string_literal loc source)
            )
          )
      in
      let with_specifiers import_kind env leading =
        let specifiers = named_or_namespace_specifier env import_kind in
        let source = source env in
        let (trailing, source) = semicolon_and_trailing env source in
        Statement.ImportDeclaration
          {
            import_kind;
            source;
            specifiers;
            default = None;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
      in
      let with_default import_kind env leading =
        let default_specifier =
          match import_kind with
          | ImportType
          | ImportTypeof ->
            {
              Statement.ImportDeclaration.identifier = Type.type_identifier env;
              remote_default_name_def_loc = None;
            }
          | ImportValue ->
            {
              Statement.ImportDeclaration.identifier = Parse.identifier env;
              remote_default_name_def_loc = None;
            }
        in
        let additional_specifiers =
          match Peek.token env with
          | T_COMMA ->
            (* `import Foo, ...` *)
            Expect.token env T_COMMA;
            named_or_namespace_specifier env import_kind
          | _ -> None
        in
        let source = source env in
        let (trailing, source) = semicolon_and_trailing env source in
        Statement.ImportDeclaration
          {
            import_kind;
            source;
            specifiers = additional_specifiers;
            default = Some default_specifier;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
      in
      with_loc (fun env ->
          let env = env |> with_strict true in
          let leading = Peek.comments env in
          Expect.token env T_IMPORT;

          match Peek.token env with
          (* `import * as ns from "ModuleName";` *)
          | T_MULT -> with_specifiers ImportValue env leading
          (* `import { ... } from "ModuleName";` *)
          | T_LCURLY -> with_specifiers ImportValue env leading
          (* `import "ModuleName";` *)
          | T_STRING str ->
            let source = string_literal env str in
            let (trailing, source) = semicolon_and_trailing env source in
            Statement.ImportDeclaration
              {
                import_kind = ImportValue;
                source;
                specifiers = None;
                default = None;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }
          (* `import type [...] from "ModuleName";`
             note that if [...] is missing, we're importing a value named `type`! *)
          | T_TYPE when should_parse_types env -> begin
            match Peek.ith_token ~i:1 env with
            (* `import type, { other, names } from "ModuleName";` *)
            | T_COMMA
            (* `import type from "ModuleName";` *)
            | T_IDENTIFIER { raw = "from"; _ } ->
              (* Importing the exported value named "type". This is not a type-import.*)
              with_default ImportValue env leading
            (* `import type *` is invalid, since the namespace can't be a type *)
            | T_MULT ->
              (* consume `type` *)
              Eat.token env;

              (* unexpected `*` *)
              error_unexpected env;

              with_specifiers ImportType env leading
            | T_LCURLY ->
              (* consume `type` *)
              Eat.token env;

              with_specifiers ImportType env leading
            | _ ->
              (* consume `type` *)
              Eat.token env;

              with_default ImportType env leading
          end
          (* `import typeof ... from "ModuleName";` *)
          | T_TYPEOF when should_parse_types env ->
            Expect.token env T_TYPEOF;
            begin
              match Peek.token env with
              | T_MULT
              | T_LCURLY ->
                with_specifiers ImportTypeof env leading
              | _ -> with_default ImportTypeof env leading
            end
          (* import Foo from "ModuleName"; *)
          | _ -> with_default ImportValue env leading
      )
    )
end
