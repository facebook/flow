(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Token
open Parser_env
open Flow_ast
module SSet = Set.Make (String)
open Parser_common
open Comment_attachment

module type STATEMENT = sig
  val for_ : env -> (Loc.t, Loc.t) Statement.t

  val if_ : env -> (Loc.t, Loc.t) Statement.t

  val let_ : env -> (Loc.t, Loc.t) Statement.t

  val try_ : env -> (Loc.t, Loc.t) Statement.t

  val while_ : env -> (Loc.t, Loc.t) Statement.t

  val with_ : env -> (Loc.t, Loc.t) Statement.t

  val block : env -> (Loc.t, Loc.t) Statement.t

  val break : env -> (Loc.t, Loc.t) Statement.t

  val continue : env -> (Loc.t, Loc.t) Statement.t

  val debugger : env -> (Loc.t, Loc.t) Statement.t

  val declare : ?in_module:bool -> env -> (Loc.t, Loc.t) Statement.t

  val declare_export_declaration : ?allow_export_type:bool -> env -> (Loc.t, Loc.t) Statement.t

  val declare_opaque_type : env -> (Loc.t, Loc.t) Statement.t

  val do_while : env -> (Loc.t, Loc.t) Statement.t

  val empty : env -> (Loc.t, Loc.t) Statement.t

  val export_declaration :
    decorators:(Loc.t, Loc.t) Class.Decorator.t list -> env -> (Loc.t, Loc.t) Statement.t

  val expression : env -> (Loc.t, Loc.t) Statement.t

  val import_declaration : env -> (Loc.t, Loc.t) Statement.t

  val interface : env -> (Loc.t, Loc.t) Statement.t

  val maybe_labeled : env -> (Loc.t, Loc.t) Statement.t

  val opaque_type : env -> (Loc.t, Loc.t) Statement.t

  val return : env -> (Loc.t, Loc.t) Statement.t

  val switch : env -> (Loc.t, Loc.t) Statement.t

  val throw : env -> (Loc.t, Loc.t) Statement.t

  val type_alias : env -> (Loc.t, Loc.t) Statement.t

  val var : env -> (Loc.t, Loc.t) Statement.t

  val const : env -> (Loc.t, Loc.t) Statement.t
end

module Statement
    (Parse : PARSER)
    (Type : Type_parser.TYPE)
    (Declaration : Declaration_parser.DECLARATION)
    (Object : Object_parser.OBJECT)
    (Pattern_cover : Pattern_cover.COVER) : STATEMENT = struct
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
      | _ -> () );
    func

  (* https://tc39.es/ecma262/#sec-exports-static-semantics-early-errors *)
  let assert_identifier_name_is_identifier
      ?restricted_error env (loc, { Ast.Identifier.name; comments = _ }) =
    match name with
    | "let" ->
      (* "let" is disallowed as an identifier in a few situations. 11.6.2.1
         lists them out. It is always disallowed in strict mode *)
      if in_strict_mode env then
        strict_error_at env (loc, Parse_error.StrictReservedWord)
      else if no_let env then
        error_at env (loc, Parse_error.Unexpected (Token.quote_token_value name))
    | "await" ->
      (* `allow_await` means that `await` is allowed to be a keyword,
          which makes it illegal to use as an identifier.
          https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors *)
      if allow_await env then error_at env (loc, Parse_error.UnexpectedReserved)
    | "yield" ->
      (* `allow_yield` means that `yield` is allowed to be a keyword,
          which makes it illegal to use as an identifier.
          https://tc39.github.io/ecma262/#sec-identifiers-static-semantics-early-errors *)
      if allow_yield env then
        error_at env (loc, Parse_error.UnexpectedReserved)
      else
        strict_error_at env (loc, Parse_error.StrictReservedWord)
    | _ when is_strict_reserved name -> strict_error_at env (loc, Parse_error.StrictReservedWord)
    | _ when is_reserved name ->
      error_at env (loc, Parse_error.Unexpected (Token.quote_token_value name))
    | _ ->
      begin
        match restricted_error with
        | Some err when is_restricted name -> strict_error_at env (loc, err)
        | _ -> ()
      end

  let string_literal env (loc, value, raw, octal) =
    if octal then strict_error env Parse_error.StrictOctalLiteral;
    let leading = Peek.comments env in
    Expect.token env (T_STRING (loc, value, raw, octal));
    let trailing = Peek.comments env in
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
      Implicit { trailing = Peek.comments env; remove_trailing = (fun x _ -> x) }
    | T_SEMICOLON ->
      Eat.token env;
      (match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        Explicit (Peek.comments env)
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
      { trailing = Peek.comments env; remove_trailing = (fun x _ -> x) }
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
        { Statement.Empty.comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () } )

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
              let ((_, { Identifier.name; comments = _ }) as label) = Parse.identifier env in
              if not (SSet.mem name (labels env)) then error env (Parse_error.UnknownLabel name);
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
              let ((_, { Identifier.name; comments = _ }) as label) = Parse.identifier env in
              if not (SSet.mem name (labels env)) then error env (Parse_error.UnknownLabel name);
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
        } )

  and debugger =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DEBUGGER;
        let pre_semicolon_trailing =
          if Peek.token env = T_SEMICOLON then
            Peek.comments env
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
          { Statement.Debugger.comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () })

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
        let pre_keyword_trailing = Peek.comments env in
        Expect.token env T_WHILE;
        let pre_cond_trailing = Peek.comments env in
        Expect.token env T_LPAREN;
        let test = Parse.expression env in
        Expect.token env T_RPAREN;
        let past_cond_trailing = Peek.comments env in
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
          })

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
        Expect.token env T_FOR;
        let async = allow_await env && Expect.maybe env T_AWAIT in
        Expect.token env T_LPAREN;

        let (init, errs) =
          let env = env |> with_no_in true in
          match Peek.token env with
          | T_SEMICOLON -> (None, [])
          | T_LET ->
            let (loc, (declarations, leading, errs)) = with_loc Declaration.let_ env in
            ( Some
                (For_declaration
                   ( loc,
                     {
                       Statement.VariableDeclaration.kind = Statement.VariableDeclaration.Let;
                       declarations;
                       comments = Flow_ast_utils.mk_comments_opt ~leading ();
                     } )),
              errs )
          | T_CONST ->
            let (loc, (declarations, leading, errs)) = with_loc Declaration.const env in
            ( Some
                (For_declaration
                   ( loc,
                     {
                       Statement.VariableDeclaration.kind = Statement.VariableDeclaration.Const;
                       declarations;
                       comments = Flow_ast_utils.mk_comments_opt ~leading ();
                     } )),
              errs )
          | T_VAR ->
            let (loc, (declarations, leading, errs)) = with_loc Declaration.var env in
            ( Some
                (For_declaration
                   ( loc,
                     {
                       Statement.VariableDeclaration.kind = Statement.VariableDeclaration.Var;
                       declarations;
                       comments = Flow_ast_utils.mk_comments_opt ~leading ();
                     } )),
              errs )
          | _ ->
            let expr = Parse.expression_or_pattern (env |> with_no_let true) in
            (Some (For_expression expr), [])
        in
        match Peek.token env with
        (* If `async` is true, this must be a for-await-of loop. *)
        | t when t = T_OF || async ->
          let left =
            let open Statement in
            match init with
            | Some (For_declaration decl) ->
              assert_can_be_forin_or_forof env Parse_error.InvalidLHSInForOf decl;
              ForOf.LeftDeclaration decl
            | Some (For_expression expr) ->
              (* #sec-for-in-and-for-of-statements-static-semantics-early-errors *)
              let patt = Pattern_cover.as_pattern ~err:Parse_error.InvalidLHSInForOf env expr in
              ForOf.LeftPattern patt
            | None -> assert false
          in
          (* This is a for of loop *)
          Expect.token env T_OF;
          let right = Parse.assignment env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.ForOf { Statement.ForOf.left; right; body; await = async }
        | T_IN ->
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
          (* This is a for in loop *)
          Expect.token env T_IN;
          let right = Parse.expression env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.ForIn { Statement.ForIn.left; right; body; each = false }
        | _ ->
          (* This is a for loop *)
          errs |> List.iter (error_at env);
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
          Statement.For { Statement.For.init; test; update; body })

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
    with_loc (fun env ->
        let pre_if_leading = Peek.comments env in
        Expect.token env T_IF;
        let pre_cond_leading = Peek.comments env in
        let leading = pre_if_leading @ pre_cond_leading in
        Expect.token env T_LPAREN;
        let test = Parse.expression env in
        Expect.token env T_RPAREN;
        let trailing = Peek.comments env in
        let consequent = if_branch env in
        let alternate =
          if Peek.token env = T_ELSE then (
            Expect.token env T_ELSE;
            Some (if_branch env)
          ) else
            None
        in
        Statement.If
          {
            Statement.If.test;
            consequent;
            alternate;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          })

  and return =
    with_loc (fun env ->
        if not (in_function env) then error env Parse_error.IllegalReturn;
        let leading = Peek.comments env in
        Expect.token env T_RETURN;
        let trailing =
          if Peek.token env = T_SEMICOLON then
            Peek.comments env
          else
            []
        in
        let argument =
          if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env then
            None
          else
            Some (Parse.expression env)
        in
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
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          })

  and switch =
    let rec case_list env (seen_default, acc) =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        List.rev acc
      | _ ->
        let start_loc = Peek.loc env in
        let leading = Peek.comments env in
        let (test, trailing) =
          match Peek.token env with
          | T_DEFAULT ->
            if seen_default then error env Parse_error.MultipleDefaultsInSwitch;
            Expect.token env T_DEFAULT;
            (None, Peek.comments env)
          | _ ->
            Expect.token env T_CASE;
            (Some (Parse.expression env), [])
        in
        let seen_default = seen_default || test = None in
        let end_loc = Peek.loc env in
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
        let end_loc =
          match List.rev consequent with
          | last_stmt :: _ -> fst last_stmt
          | _ -> end_loc
        in
        let acc =
          ( Loc.btwn start_loc end_loc,
            Statement.Switch.Case.
              { test; consequent; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
          )
          :: acc
        in
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
          })

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
        Throw { Throw.argument; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () })

  and try_ =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_TRY;
        let block = Parse.block_body env in
        let handler =
          match Peek.token env with
          | T_CATCH ->
            let catch =
              with_loc
                (fun env ->
                  let leading = Peek.comments env in
                  Expect.token env T_CATCH;
                  let trailing = Peek.comments env in
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
                      remove_trailing body (fun remover (loc, body) ->
                          (loc, remover#block loc body))
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
          })

  and var =
    with_loc (fun env ->
        let kind = Statement.VariableDeclaration.Var in
        let (declarations, leading, errs) = Declaration.var env in
        let (trailing, declarations) = variable_declaration_end ~kind env declarations in
        errs |> List.iter (error_at env);
        Statement.VariableDeclaration
          {
            Statement.VariableDeclaration.kind;
            declarations;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          })

  and const =
    with_loc (fun env ->
        let kind = Statement.VariableDeclaration.Const in
        let (declarations, leading, errs) = Declaration.const env in
        let (trailing, declarations) = variable_declaration_end ~kind env declarations in
        errs |> List.iter (error_at env);
        Statement.VariableDeclaration
          {
            Statement.VariableDeclaration.kind;
            declarations;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          })

  and let_ =
    with_loc (fun env ->
        let kind = Statement.VariableDeclaration.Let in
        let (declarations, leading, errs) = Declaration.let_ env in
        let (trailing, declarations) = variable_declaration_end ~kind env declarations in
        errs |> List.iter (error_at env);
        Statement.VariableDeclaration
          {
            Statement.VariableDeclaration.kind;
            declarations;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          })

  and while_ =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_WHILE;
        Expect.token env T_LPAREN;
        let test = Parse.expression env in
        Expect.token env T_RPAREN;
        let body = Parse.statement (env |> with_in_loop true) in
        (* Annex B allows labelled FunctionDeclarations in non-strict mode
       (see sec-labelled-function-declarations), but not in IterationStatement
       (see sec-semantics-static-semantics-early-errors). *)
        if (not (in_strict_mode env)) && is_labelled_function body then
          function_as_statement_error_at env (fst body);
        let trailing = Peek.comments env in
        Statement.While
          {
            Statement.While.test;
            body;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          })

  and with_ env =
    let (loc, stmt) =
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token env T_WITH;
          Expect.token env T_LPAREN;
          let _object = Parse.expression env in
          Expect.token env T_RPAREN;
          let body = Parse.statement env in
          (* Annex B allows labelled FunctionDeclarations in non-strict mode
         (see sec-labelled-function-declarations), but not in WithStatement
         (see sec-with-statement-static-semantics-early-errors). *)
          if (not (in_strict_mode env)) && is_labelled_function body then
            function_as_statement_error_at env (fst body);
          let trailing = Peek.comments env in
          Statement.With
            {
              Statement.With._object;
              body;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
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
            })

  and expression =
    with_loc (fun env ->
        let expression = Parse.expression env in
        let (trailing, expression) =
          match semicolon ~expected:"the end of an expression statement (`;`)" env with
          | Explicit comments -> (comments, expression)
          | Implicit { remove_trailing; _ } ->
            ([], remove_trailing expression (fun remover expr -> remover#expression expr))
        in
        let directive =
          if allow_directive env then
            match expression with
            | (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.String _; raw; _ }) ->
              Some (String.sub raw 1 (String.length raw - 2))
            | _ -> None
          else
            None
        in
        Statement.Expression
          {
            Statement.Expression.expression;
            directive;
            comments = Flow_ast_utils.mk_comments_opt ~trailing ();
          })

  and type_alias_helper env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAlias;
    let leading = Peek.comments env in
    Expect.token env T_TYPE;
    Eat.push_lex_mode env Lex_mode.TYPE;
    let id = Type.type_identifier env in
    let tparams = Type.type_params env ~attach_leading:false ~attach_trailing:true in
    Expect.token env T_ASSIGN;
    let right = Type._type env in
    Eat.pop_lex_mode env;
    let (trailing, right) =
      match semicolon env with
      | Explicit comments -> (comments, right)
      | Implicit { remove_trailing; _ } ->
        ([], remove_trailing right (fun remover right -> remover#type_ right))
    in
    Statement.TypeAlias.
      { id; tparams; right; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }

  and declare_type_alias env =
    with_loc
      (fun env ->
        Expect.token env T_DECLARE;
        let type_alias = type_alias_helper env in
        Statement.DeclareTypeAlias type_alias)
      env

  and type_alias env =
    if Peek.ith_is_identifier ~i:1 env then
      let (loc, type_alias) = with_loc type_alias_helper env in
      (loc, Statement.TypeAlias type_alias)
    else
      Parse.statement env

  and opaque_type_helper ?(declare = false) env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedOpaqueTypeAlias;
    let leading_opaque = Peek.comments env in
    Expect.token env T_OPAQUE;
    let leading_type = Peek.comments env in
    Expect.token env T_TYPE;
    let leading = leading_opaque @ leading_type in
    Eat.push_lex_mode env Lex_mode.TYPE;
    let id = Type.type_identifier env in
    let tparams = Type.type_params env ~attach_leading:false ~attach_trailing:true in
    let supertype =
      match Peek.token env with
      | T_COLON ->
        Expect.token env T_COLON;
        Some (Type._type env)
      | _ -> None
    in
    let impltype =
      if not declare then (
        Expect.token env T_ASSIGN;
        Some (Type._type env)
      ) else
        None
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
          Some (remove_trailing impl (fun remover impl -> remover#type_ impl)) )
      (* opaque type Foo: Super *)
      | (Implicit { remove_trailing; _ }, _, Some super, None) ->
        ( [],
          id,
          tparams,
          Some (remove_trailing super (fun remover super -> remover#type_ super)),
          None )
      (* opaque type Foo<T> *)
      | (Implicit { remove_trailing; _ }, Some tparams, None, None) ->
        ( [],
          id,
          Some (remove_trailing tparams (fun remover tparams -> remover#type_params tparams)),
          None,
          None )
      (* declare opaque type Foo *)
      | (Implicit { remove_trailing; _ }, None, None, None) ->
        ([], remove_trailing id (fun remover id -> remover#identifier id), None, None, None)
    in
    Statement.OpaqueType.
      {
        id;
        tparams;
        impltype;
        supertype;
        comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
      }

  and declare_opaque_type env =
    with_loc
      (fun env ->
        Expect.token env T_DECLARE;
        let opaque_t = opaque_type_helper ~declare:true env in
        Statement.DeclareOpaqueType opaque_t)
      env

  and opaque_type env =
    match Peek.ith_token ~i:1 env with
    | T_TYPE ->
      let (loc, opaque_t) = with_loc (opaque_type_helper ~declare:false) env in
      (loc, Statement.OpaqueType opaque_t)
    | _ -> Parse.statement env

  and interface_helper env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeInterface;
    let leading = Peek.comments env in
    Expect.token env T_INTERFACE;
    let id = Type.type_identifier env in
    let tparams = Type.type_params env ~attach_leading:false ~attach_trailing:true in
    let (extends, body) = Type.interface_helper env ~id:(Some id) in
    let { remove_trailing; _ } = statement_end_trailing_comments env in
    let body =
      remove_trailing body (fun remover (loc, body) -> (loc, remover#object_type loc body))
    in
    Statement.Interface.
      { id; tparams; body; extends; comments = Flow_ast_utils.mk_comments_opt ~leading () }

  and declare_interface env =
    with_loc
      (fun env ->
        Expect.token env T_DECLARE;
        let iface = interface_helper env in
        Statement.DeclareInterface iface)
      env

  and interface env =
    (* disambiguate between a value named `interface`, like `var interface = 1; interface++`,
       and an interface declaration like `interface Foo {}`.` *)
    if Peek.ith_is_identifier_name ~i:1 env then
      let (loc, iface) = with_loc interface_helper env in
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
    fun env ->
      let env = env |> with_strict true in
      Expect.token env T_CLASS;
      let id = Parse.identifier env in
      let tparams = Type.type_params env ~attach_leading:false ~attach_trailing:true in
      let extends =
        if Expect.maybe env T_EXTENDS then
          Some (Type.generic env)
        else
          None
      in
      let mixins =
        match Peek.token env with
        | T_IDENTIFIER { raw = "mixins"; _ } ->
          Eat.token env;
          mixins env []
        | _ -> []
      in
      let implements =
        match Peek.token env with
        | T_IMPLEMENTS -> Some (Object.class_implements env ~attach_leading:true)
        | _ -> None
      in
      let body = Type._object ~is_class:true env in
      let { remove_trailing; _ } = statement_end_trailing_comments env in
      let body =
        remove_trailing body (fun remover (loc, body) -> (loc, remover#object_type loc body))
      in
      Statement.DeclareClass.{ id; tparams; body; extends; mixins; implements }

  and declare_class_statement env =
    with_loc
      (fun env ->
        Expect.token env T_DECLARE;
        let fn = declare_class env in
        Statement.DeclareClass fn)
      env

  and declare_function ?(leading = []) env =
    let leading = leading @ Peek.comments env in
    Expect.token env T_FUNCTION;
    let id = Parse.identifier env in
    let start_sig_loc = Peek.loc env in
    let tparams = Type.type_params env ~attach_leading:false ~attach_trailing:false in
    let params = Type.function_param_list env in
    Expect.token env T_COLON;
    let return = Type._type env in
    let end_loc = fst return in
    let loc = Loc.btwn start_sig_loc end_loc in
    let annot = (loc, Ast.Type.(Function { Function.params; return; tparams; comments = None })) in
    let predicate = Type.predicate_opt env in
    let (trailing, annot, predicate) =
      match (semicolon env, predicate) with
      | (Explicit comments, _) -> (comments, annot, predicate)
      | (Implicit { remove_trailing; _ }, None) ->
        ([], remove_trailing annot (fun remover annot -> remover#type_ annot), None)
      | (Implicit { remove_trailing; _ }, Some pred) ->
        ([], annot, Some (remove_trailing pred (fun remover pred -> remover#predicate pred)))
    in
    let annot = (loc, annot) in
    Statement.DeclareFunction.
      { id; annot; predicate; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }

  and declare_function_statement env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        begin
          match Peek.token env with
          | T_ASYNC ->
            error env Parse_error.DeclareAsync;
            Expect.token env T_ASYNC
          | _ -> ()
        end;
        let fn = declare_function ~leading env in
        Statement.DeclareFunction fn)
      env

  and declare_var env leading =
    let leading = leading @ Peek.comments env in
    Expect.token env T_VAR;
    let (_loc, { Pattern.Identifier.name; annot; _ }) =
      Parse.identifier_with_type env ~no_optional:true Parse_error.StrictVarName
    in
    let (trailing, name, annot) =
      match (semicolon env, annot) with
      (* declare var x; *)
      | (Explicit trailing, _) -> (trailing, name, annot)
      (* declare var x *)
      | (Implicit { remove_trailing; _ }, Ast.Type.Missing _) ->
        ([], remove_trailing name (fun remover name -> remover#identifier name), annot)
      (* declare var x: mixed *)
      | (Implicit { remove_trailing; _ }, Ast.Type.Available _) ->
        ([], name, remove_trailing annot (fun remover annot -> remover#type_annotation_hint annot))
    in
    Statement.DeclareVariable.
      { id = name; annot; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }

  and declare_var_statement env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_DECLARE;
        let var = declare_var env leading in
        Statement.DeclareVariable var)
      env

  and declare_module =
    let rec module_items env ~module_kind acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        (module_kind, List.rev acc)
      | _ ->
        let stmt = declare ~in_module:true env in
        (* TODO: This is a semantic analysis and shouldn't be in the parser *)
        let module_kind =
          let open Statement in
          let (loc, stmt) = stmt in
          match (module_kind, stmt) with
          (*
           * The first time we see either a `declare export` or a
           * `declare module.exports`, we lock in the kind of the module.
           *
           * `declare export type` and `declare export interface` are the two
           * exceptions to this rule because they are valid in both CommonJS
           * and ES modules (and thus do not indicate an intent for either).
           *)
          | (None, DeclareModuleExports _) -> Some (DeclareModule.CommonJS loc)
          | (None, DeclareExportDeclaration { DeclareExportDeclaration.declaration; _ }) ->
            (match declaration with
            | Some (DeclareExportDeclaration.NamedType _)
            | Some (DeclareExportDeclaration.Interface _) ->
              module_kind
            | _ -> Some (DeclareModule.ES loc))
          (*
           * There should never be more than one `declare module.exports`
           * statement *)
          | (Some (DeclareModule.CommonJS _), DeclareModuleExports _) ->
            error env Parse_error.DuplicateDeclareModuleExports;
            module_kind
          (*
           * It's never ok to mix and match `declare export` and
           * `declare module.exports` in the same module because it leaves the
           * kind of the module (CommonJS vs ES) ambiguous.
           *
           * The 1 exception to this rule is that `export type/interface` are
           * both ok in CommonJS modules.
           *)
          | (Some (DeclareModule.ES _), DeclareModuleExports _) ->
            error env Parse_error.AmbiguousDeclareModuleKind;
            module_kind
          | ( Some (DeclareModule.CommonJS _),
              DeclareExportDeclaration { DeclareExportDeclaration.declaration; _ } ) ->
            (match declaration with
            | Some (DeclareExportDeclaration.NamedType _)
            | Some (DeclareExportDeclaration.Interface _) ->
              ()
            | _ -> error env Parse_error.AmbiguousDeclareModuleKind);
            module_kind
          | _ -> module_kind
        in
        module_items env ~module_kind (stmt :: acc)
    in
    let declare_module_ env start_loc =
      let id =
        match Peek.token env with
        | T_STRING str -> Statement.DeclareModule.Literal (string_literal env str)
        | _ -> Statement.DeclareModule.Identifier (Parse.identifier env)
      in
      let (body_loc, ((module_kind, body), comments)) =
        with_loc
          (fun env ->
            let leading = Peek.comments env in
            Expect.token env T_LCURLY;
            let res = module_items env ~module_kind:None [] in
            Expect.token env T_RCURLY;
            let { trailing; _ } = statement_end_trailing_comments env in
            (res, Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
          env
      in
      let body = (body_loc, { Statement.Block.body; comments }) in
      let loc = Loc.btwn start_loc body_loc in
      let kind =
        match module_kind with
        | Some k -> k
        | None -> Statement.DeclareModule.CommonJS loc
      in
      (loc, Statement.(DeclareModule DeclareModule.{ id; body; kind }))
    in
    fun ?(in_module = false) env ->
      let start_loc = Peek.loc env in
      Expect.token env T_DECLARE;
      Expect.identifier env "module";
      if in_module || Peek.token env = T_PERIOD then
        let (loc, exports) = with_loc declare_module_exports env in
        (Loc.btwn start_loc loc, exports)
      else
        declare_module_ env start_loc

  and declare_module_exports env =
    Expect.token env T_PERIOD;
    Expect.identifier env "exports";
    let type_annot = Type.annotation env in
    let _ = semicolon env in
    Statement.DeclareModuleExports type_annot

  and declare ?(in_module = false) env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeDeclaration;

    (* eventually, just emit a wrapper AST node *)
    match Peek.ith_token ~i:1 env with
    | T_CLASS -> declare_class_statement env
    | T_INTERFACE -> declare_interface env
    | T_TYPE ->
      (match Peek.token env with
      | T_IMPORT when in_module -> import_declaration env
      | _ -> declare_type_alias env)
    | T_OPAQUE -> declare_opaque_type env
    | T_TYPEOF when Peek.token env = T_IMPORT -> import_declaration env
    | T_FUNCTION
    | T_ASYNC ->
      declare_function_statement env
    | T_VAR -> declare_var_statement env
    | T_EXPORT when in_module -> declare_export_declaration ~allow_export_type:in_module env
    | T_IDENTIFIER { raw = "module"; _ } -> declare_module ~in_module env
    | _ when in_module ->
      (match Peek.token env with
      | T_IMPORT ->
        error env Parse_error.InvalidNonTypeImportInDeclareModule;
        Parse.statement env
      | _ ->
        (* Oh boy, found some bad stuff in a declare module. Let's just
              * pretend it's a declare var (arbitrary choice) *)
        declare_var_statement env)
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

  and extract_pattern_binding_names =
    let rec fold acc =
      let open Pattern in
      function
      | (_, Object { Object.properties; _ }) ->
        List.fold_left
          (fun acc prop ->
            match prop with
            | Object.Property (_, { Object.Property.pattern; _ })
            | Object.RestElement (_, { RestElement.argument = pattern; comments = _ }) ->
              fold acc pattern)
          acc
          properties
      | (_, Array { Array.elements; _ }) ->
        List.fold_left
          (fun acc elem ->
            match elem with
            | Some (Array.Element (_, { Array.Element.argument = pattern; default = _ }))
            | Some (Array.RestElement (_, { RestElement.argument = pattern; comments = _ })) ->
              fold acc pattern
            | None -> acc)
          acc
          elements
      | (_, Identifier { Pattern.Identifier.name; _ }) -> name :: acc
      | (_, Expression _) -> failwith "Parser error: No such thing as an expression pattern!"
    in
    List.fold_left fold

  and extract_ident_name (_, { Identifier.name; comments = _ }) = name

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
                let exported = identifier_name env in
                record_export env exported;
                Some exported
              | _ ->
                record_export env local;
                None
            in
            { Statement.ExportNamedDeclaration.ExportSpecifier.local; exported })
          env
      in
      let preceding_comma = Expect.maybe env T_COMMA in
      export_specifiers ~preceding_comma env (specifier :: specifiers)

  and assert_export_specifier_identifiers env specifiers =
    Statement.ExportNamedDeclaration.ExportSpecifier.(
      List.iter
        (function
          | (_, { local = id; exported = None }) ->
            assert_identifier_name_is_identifier ~restricted_error:Parse_error.StrictVarName env id
          | _ -> ())
        specifiers)

  and export_declaration ~decorators =
    with_loc (fun env ->
        let env = env |> with_strict true |> with_in_export true in
        let start_loc = Peek.loc env in
        let leading = Peek.comments env in
        Expect.token env T_EXPORT;
        match Peek.token env with
        | T_DEFAULT ->
          (* export default ... *)
          Statement.ExportDefaultDeclaration.(
            let (default, ()) = with_loc (fun env -> Expect.token env T_DEFAULT) env in
            record_export
              env
              (Flow_ast_utils.ident_of_source (Loc.btwn start_loc (Peek.loc env), "default"));
            let (declaration, trailing) =
              if Peek.is_function env then
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
              else
                (* export default [assignment expression]; *)
                let expr = Parse.assignment env in
                let trailing =
                  match semicolon env with
                  | Explicit comments -> comments
                  | Implicit _ -> []
                in
                (Expression expr, trailing)
            in
            Statement.ExportDefaultDeclaration
              {
                default;
                declaration;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              })
        | T_TYPE when Peek.ith_token ~i:1 env <> T_LCURLY ->
          (* export type ... *)
          Statement.ExportNamedDeclaration.(
            if not (should_parse_types env) then error env Parse_error.UnexpectedTypeExport;
            (match Peek.ith_token ~i:1 env with
            | T_MULT ->
              Expect.token env T_TYPE;
              let specifier_loc = Peek.loc env in
              Expect.token env T_MULT;
              let source = export_source env in
              let trailing =
                match semicolon env with
                | Explicit comments -> comments
                | Implicit _ -> []
              in
              Statement.ExportNamedDeclaration
                {
                  declaration = None;
                  specifiers = Some (ExportBatchSpecifier (specifier_loc, None));
                  source = Some source;
                  exportKind = Statement.ExportType;
                  comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
                }
            | _ ->
              let (loc, type_alias) = with_loc type_alias_helper env in
              record_export
                env
                (Flow_ast_utils.ident_of_source
                   (loc, extract_ident_name type_alias.Statement.TypeAlias.id));
              let type_alias = (loc, Statement.TypeAlias type_alias) in
              Statement.ExportNamedDeclaration
                {
                  declaration = Some type_alias;
                  specifiers = None;
                  source = None;
                  exportKind = Statement.ExportType;
                  comments = Flow_ast_utils.mk_comments_opt ~leading ();
                }))
        | T_OPAQUE ->
          (* export opaque type ... *)
          Statement.ExportNamedDeclaration.(
            let (loc, opaque_t) = with_loc opaque_type_helper env in
            record_export
              env
              (Flow_ast_utils.ident_of_source
                 (loc, extract_ident_name opaque_t.Statement.OpaqueType.id));
            let opaque_t = (loc, Statement.OpaqueType opaque_t) in
            Statement.ExportNamedDeclaration
              {
                declaration = Some opaque_t;
                specifiers = None;
                source = None;
                exportKind = Statement.ExportType;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              })
        | T_INTERFACE ->
          (* export interface I { ... } *)
          Statement.ExportNamedDeclaration.(
            if not (should_parse_types env) then error env Parse_error.UnexpectedTypeExport;
            let interface = interface env in
            (match interface with
            | (loc, Statement.InterfaceDeclaration { Statement.Interface.id; _ }) ->
              record_export env (Flow_ast_utils.ident_of_source (loc, extract_ident_name id))
            | _ ->
              failwith
                ( "Internal Flow Error! Parsed `export interface` into something "
                ^ "other than an interface declaration!" ));
            Statement.ExportNamedDeclaration
              {
                declaration = Some interface;
                specifiers = None;
                source = None;
                exportKind = Statement.ExportType;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              })
        | T_LET
        | T_CONST
        | T_VAR
        (* not using Peek.is_class here because it would guard all of the
      * cases *)
        | T_AT
        | T_CLASS
        (* not using Peek.is_function here because it would guard all of the
      * cases *)
        | T_ASYNC
        | T_FUNCTION
        | T_ENUM ->
          Statement.ExportNamedDeclaration.(
            let stmt = Parse.statement_list_item env ~decorators in
            let names =
              let open Statement in
              match stmt with
              | (_, VariableDeclaration { VariableDeclaration.declarations; _ }) ->
                List.fold_left
                  (fun names (_, declaration) ->
                    let id = declaration.VariableDeclaration.Declarator.id in
                    extract_pattern_binding_names names [id])
                  []
                  declarations
              | (loc, ClassDeclaration { Class.id = Some id; _ })
              | (loc, FunctionDeclaration { Function.id = Some id; _ })
              | (loc, EnumDeclaration { EnumDeclaration.id; _ }) ->
                [Flow_ast_utils.ident_of_source (loc, extract_ident_name id)]
              | (loc, ClassDeclaration { Class.id = None; _ }) ->
                error_at env (loc, Parse_error.ExportNamelessClass);
                []
              | (loc, FunctionDeclaration { Function.id = None; _ }) ->
                error_at env (loc, Parse_error.ExportNamelessFunction);
                []
              | _ -> failwith "Internal Flow Error! Unexpected export statement declaration!"
            in
            List.iter (record_export env) names;
            Statement.ExportNamedDeclaration
              {
                declaration = Some stmt;
                specifiers = None;
                source = None;
                exportKind = Statement.ExportValue;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              })
        | T_MULT ->
          Statement.ExportNamedDeclaration.(
            let loc = Peek.loc env in
            Expect.token env T_MULT;
            let local_name =
              let parse_export_star_as = (parse_options env).esproposal_export_star_as in
              match Peek.token env with
              | T_IDENTIFIER { raw = "as"; _ } ->
                Eat.token env;
                if parse_export_star_as then
                  Some (Parse.identifier env)
                else (
                  error env Parse_error.UnexpectedTypeDeclaration;
                  None
                )
              | _ -> None
            in
            let specifiers = Some (ExportBatchSpecifier (loc, local_name)) in
            let source = export_source env in
            let source = Some source in
            let trailing =
              match semicolon env with
              | Explicit comments -> comments
              | Implicit _ -> []
            in
            Statement.ExportNamedDeclaration
              {
                declaration = None;
                specifiers;
                source;
                exportKind = Statement.ExportValue;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              })
        | _ ->
          Statement.ExportNamedDeclaration.(
            let exportKind =
              match Peek.token env with
              | T_TYPE ->
                Eat.token env;
                Statement.ExportType
              | _ -> Statement.ExportValue
            in
            Expect.token env T_LCURLY;
            let specifiers = export_specifiers env [] in
            Expect.token env T_RCURLY;
            let source =
              match Peek.token env with
              | T_IDENTIFIER { raw = "from"; _ } -> Some (export_source env)
              | _ ->
                assert_export_specifier_identifiers env specifiers;
                None
            in
            let trailing =
              match semicolon env with
              | Explicit comments -> comments
              | Implicit _ -> []
            in
            Statement.ExportNamedDeclaration
              {
                declaration = None;
                specifiers = Some (ExportSpecifiers specifiers);
                source;
                exportKind;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }))

  and declare_export_declaration ?(allow_export_type = false) =
    with_loc (fun env ->
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeDeclaration;
        Expect.token env T_DECLARE;

        let env = env |> with_strict true |> with_in_export true in
        Expect.token env T_EXPORT;
        Statement.DeclareExportDeclaration.(
          match Peek.token env with
          | T_DEFAULT ->
            (* declare export default ... *)
            let (default, ()) = with_loc (fun env -> Expect.token env T_DEFAULT) env in
            let declaration =
              match Peek.token env with
              | T_FUNCTION ->
                (* declare export default function foo (...): ...  *)
                let fn = with_loc declare_function env in
                Some (Function fn)
              | T_CLASS ->
                (* declare export default class foo { ... } *)
                let class_ = with_loc declare_class env in
                Some (Class class_)
              | _ ->
                (* declare export default [type]; *)
                let type_ = Type._type env in
                let _ = semicolon env in
                Some (DefaultType type_)
            in
            Statement.DeclareExportDeclaration
              { default = Some default; declaration; specifiers = None; source = None }
          | T_LET
          | T_CONST
          | T_VAR
          | T_CLASS
          | T_FUNCTION ->
            let declaration =
              match Peek.token env with
              | T_FUNCTION ->
                (* declare export function foo (...): ...  *)
                let fn = with_loc declare_function env in
                Some (Function fn)
              | T_CLASS ->
                (* declare export class foo { ... } *)
                let class_ = with_loc declare_class env in
                Some (Class class_)
              | (T_LET | T_CONST | T_VAR) as token ->
                (match token with
                | T_LET -> error env Parse_error.DeclareExportLet
                | T_CONST -> error env Parse_error.DeclareExportConst
                | _ -> ());

                (* declare export var foo: ... *)
                let var = with_loc (fun env -> declare_var env []) env in
                Some (Variable var)
              | _ -> assert false
            in
            Statement.DeclareExportDeclaration
              { default = None; declaration; specifiers = None; source = None }
          | T_MULT ->
            (* declare export * from 'foo' *)
            let loc = Peek.loc env in
            Expect.token env T_MULT;
            let parse_export_star_as = (parse_options env).esproposal_export_star_as in
            let local_name =
              match Peek.token env with
              | T_IDENTIFIER { raw = "as"; _ } ->
                Eat.token env;
                if parse_export_star_as then
                  Some (Parse.identifier env)
                else (
                  error env Parse_error.UnexpectedTypeDeclaration;
                  None
                )
              | _ -> None
            in
            let specifiers =
              Statement.ExportNamedDeclaration.(Some (ExportBatchSpecifier (loc, local_name)))
            in
            let source = export_source env in
            let _ = semicolon env in
            Statement.DeclareExportDeclaration
              { default = None; declaration = None; specifiers; source = Some source }
          | T_TYPE when allow_export_type ->
            (* declare export type = ... *)
            let alias = with_loc type_alias_helper env in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = Some (NamedType alias);
                specifiers = None;
                source = None;
              }
          | T_OPAQUE ->
            (* declare export opaque type = ... *)
            let opaque = with_loc (opaque_type_helper ~declare:true) env in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = Some (NamedOpaqueType opaque);
                specifiers = None;
                source = None;
              }
          | T_INTERFACE when allow_export_type ->
            (* declare export interface ... *)
            let iface = with_loc interface_helper env in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = Some (Interface iface);
                specifiers = None;
                source = None;
              }
          | _ ->
            (match Peek.token env with
            | T_TYPE -> error env Parse_error.DeclareExportType
            | T_INTERFACE -> error env Parse_error.DeclareExportInterface
            | _ -> ());
            Expect.token env T_LCURLY;
            let specifiers = export_specifiers env [] in
            Expect.token env T_RCURLY;
            let source =
              match Peek.token env with
              | T_IDENTIFIER { raw = "from"; _ } -> Some (export_source env)
              | _ ->
                assert_export_specifier_identifiers env specifiers;
                None
            in
            let _ = semicolon env in
            Statement.DeclareExportDeclaration
              {
                default = None;
                declaration = None;
                specifiers = Some (Statement.ExportNamedDeclaration.ExportSpecifiers specifiers);
                source;
              }))

  and import_declaration =
    Statement.ImportDeclaration.(
      let source env =
        Expect.identifier env "from";
        match Peek.token env with
        | T_STRING str -> string_literal env str
        | _ ->
          (* Just make up a string for the error case *)
          let ret = (Peek.loc env, { StringLiteral.value = ""; raw = ""; comments = None }) in
          error_unexpected ~expected:"a string" env;
          ret
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
        | _ ->
          begin
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
            { remote; local = None; kind = None }
          (* `type as foo` (value named `type`) or `type as,` (type named `as`) *)
          | T_IDENTIFIER { raw = "as"; _ } ->
            begin
              match Peek.ith_token ~i:1 env with
              | T_EOF
              | T_RCURLY
              | T_COMMA ->
                (* `type as` *)
                { remote = Type.type_identifier env; local = None; kind }
              | T_IDENTIFIER { raw = "as"; _ } ->
                (* `type as as foo` *)
                let remote = identifier_name env in
                (* first `as` *)
                Eat.token env;

                (* second `as` *)
                let local = Some (Type.type_identifier env) in
                (* `foo` *)
                { remote; local; kind }
              | _ ->
                (* `type as foo` *)
                let remote = type_keyword_or_remote in
                (* `type` becomes a value *)
                assert_identifier_name_is_identifier env remote;
                Eat.token env;

                (* `as` *)
                let local = Some (Parse.identifier env) in
                { remote; local; kind = None }
            end
          (* `type x`, or `type x as y` *)
          | _ ->
            let (remote, local) = with_maybe_as ~for_type:true env in
            { remote; local; kind }
        else
          (* standard `x` or `x as y` *)
          let (remote, local) = with_maybe_as ~for_type:false env in
          { remote; local; kind = None }
        (* specifier in an `import type { ... }` *)
      in
      let type_specifier env =
        let (remote, local) =
          with_maybe_as
            env
            ~for_type:true
            ~error_if_type:Parse_error.ImportTypeShorthandOnlyInPureImport
        in
        { remote; local; kind = None }
        (* specifier in an `import typeof { ... }` *)
      in
      let typeof_specifier env =
        let (remote, local) =
          with_maybe_as
            env
            ~for_type:true
            ~error_if_type:Parse_error.ImportTypeShorthandOnlyInPureImport
        in
        { remote; local; kind = None }
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
          let preceding_comma = Expect.maybe env T_COMMA in
          specifier_list ~preceding_comma env statement_kind (specifier :: acc)
      in
      let named_or_namespace_specifier env import_kind =
        let start_loc = Peek.loc env in
        match Peek.token env with
        | T_MULT ->
          Expect.token env T_MULT;
          Expect.identifier env "as";
          let id =
            match import_kind with
            | ImportType
            | ImportTypeof ->
              Type.type_identifier env
            | ImportValue -> Parse.identifier env
          in
          ImportNamespaceSpecifier (Loc.btwn start_loc (fst id), id)
        | _ ->
          Expect.token env T_LCURLY;
          let specifiers = specifier_list env import_kind [] in
          Expect.token env T_RCURLY;
          ImportNamedSpecifiers specifiers
      in
      let semicolon_and_trailing env source =
        match semicolon env with
        | Explicit trailing -> (trailing, source)
        | Implicit { remove_trailing; _ } ->
          ( [],
            remove_trailing source (fun remover (loc, source) ->
                (loc, remover#string_literal_type loc source)) )
      in
      let with_specifiers importKind env leading =
        let specifiers = Some (named_or_namespace_specifier env importKind) in
        let source = source env in
        let (trailing, source) = semicolon_and_trailing env source in
        Statement.ImportDeclaration
          {
            importKind;
            source;
            specifiers;
            default = None;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
      in
      let with_default importKind env leading =
        let default_specifier =
          match importKind with
          | ImportType
          | ImportTypeof ->
            Type.type_identifier env
          | ImportValue -> Parse.identifier env
        in
        let additional_specifiers =
          match Peek.token env with
          | T_COMMA ->
            (* `import Foo, ...` *)
            Expect.token env T_COMMA;
            Some (named_or_namespace_specifier env importKind)
          | _ -> None
        in
        let source = source env in
        let (trailing, source) = semicolon_and_trailing env source in
        Statement.ImportDeclaration
          {
            importKind;
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
                importKind = ImportValue;
                source;
                specifiers = None;
                default = None;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }
          (* `import type [...] from "ModuleName";`
         note that if [...] is missing, we're importing a value named `type`! *)
          | T_TYPE when should_parse_types env ->
            begin
              match Peek.ith_token ~i:1 env with
              (* `import type, { other, names } from "ModuleName";` *)
              | T_COMMA
              (* Importing the exported value named "type." This is not a type-import.
               * `import type from "ModuleName";` *)
              | T_IDENTIFIER { raw = "from"; _ } ->
                with_default ImportValue env leading
              | T_MULT ->
                (* `import type *` is invalid, since the namespace can't be a type *)
                Eat.token env;

                (* consume `type` *)
                error_unexpected env;

                (* unexpected `*` *)
                with_specifiers ImportType env leading
              | T_LCURLY ->
                Eat.token env;

                (* consume `type` *)
                with_specifiers ImportType env leading
              | _ ->
                Eat.token env;

                (* consume `type` *)
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
          | _ -> with_default ImportValue env leading))
end
