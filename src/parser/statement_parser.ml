(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Token
open Parser_env
open Ast
module Error = Parse_error
module SSet = Set.Make(String)

open Parser_common

module type STATEMENT = sig
 val for_: env -> Statement.t
 val if_: env -> Statement.t
 val let_: env -> Statement.t
 val try_: env -> Statement.t
 val while_: env -> Statement.t
 val with_: env -> Statement.t
 val block: env -> Statement.t
 val break: env -> Statement.t
 val continue: env -> Statement.t
 val debugger: env -> Statement.t
 val declare: ?in_module:bool -> env -> Statement.t
 val declare_export_declaration: ?allow_export_type:bool -> env -> Statement.t
 val declare_opaque_type : env -> Statement.t
 val do_while: env -> Statement.t
 val empty: env -> Statement.t
 val export_declaration: decorators:Expression.t list -> env -> Statement.t
 val expression: env -> Statement.t
 val import_declaration: env -> Statement.t
 val interface: env -> Statement.t
 val maybe_labeled: env -> Statement.t
 val opaque_type : env -> Statement.t
 val return: env -> Statement.t
 val switch: env -> Statement.t
 val throw: env -> Statement.t
 val type_alias: env -> Statement.t
 val var_or_const: env -> Statement.t
end

module Statement
  (Parse: PARSER)
  (Type: Type_parser.TYPE)
  (Declaration: Declaration_parser.DECLARATION)
  (Object: Object_parser.OBJECT)
  (Pattern_cover : Pattern_cover.COVER)
: STATEMENT = struct
  type for_lhs =
    | For_expression of pattern_cover
    | For_declaration of (Loc.t * Ast.Statement.VariableDeclaration.t)

  (* FunctionDeclaration is not a valid Statement, but Annex B sometimes allows it.
     However, AsyncFunctionDeclaration and GeneratorFunctionDeclaration are never
     allowed as statements. We still parse them as statements (and raise an error) to
     recover gracefully. *)
  let function_as_statement env =
    let func = Declaration._function env in
    if in_strict_mode env then
      function_as_statement_error_at env (fst func)
    else begin match func with
      | _, Ast.Statement.FunctionDeclaration { Ast.Function.async = true; _ } ->
        error_at env (fst func, Parse_error.AsyncFunctionAsStatement)
      | _, Ast.Statement.FunctionDeclaration { Ast.Function.generator = true; _ } ->
        error_at env (fst func, Parse_error.GeneratorFunctionAsStatement)
      | _ -> ()
    end;
    func

  let rec empty env =
    let loc = Peek.loc env in
    Expect.token env T_SEMICOLON;
    loc, Statement.Empty

  and break env =
    let loc, label = with_loc (fun env ->
      Expect.token env T_BREAK;
      let label =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else begin
          let (_, name) as label =
            Parse.identifier env in
          if not (SSet.mem name (labels env))
          then error env (Error.UnknownLabel name);
          Some label
        end
      in
      Eat.semicolon env;
      label
    ) env in
    if label = None && not (in_loop env || in_switch env)
    then error_at env (loc, Error.IllegalBreak);
    loc, Statement.Break { Statement.Break.label }

  and continue env =
    let loc, label = with_loc (fun env ->
      Expect.token env T_CONTINUE;
      let label =
        if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
        then None
        else begin
          let (_, name) as label =
            Parse.identifier env in
          if not (SSet.mem name (labels env))
          then error env (Error.UnknownLabel name);
          Some label
        end in
      Eat.semicolon env;
      label
    ) env in
    if not (in_loop env) then error_at env (loc, Error.IllegalContinue);
    loc, Statement.Continue { Statement.Continue.label }

  and debugger = with_loc (fun env ->
    Expect.token env T_DEBUGGER;
    Eat.semicolon env;
    Statement.Debugger
  )

  and do_while = with_loc (fun env ->
    Expect.token env T_DO;
    let body = Parse.statement (env |> with_in_loop true) in

    (* Annex B allows labelled FunctionDeclarations (see
       sec-labelled-function-declarations), but not in IterationStatement
       (see sec-semantics-static-semantics-early-errors). *)
    if not (in_strict_mode env) && is_labelled_function body
    then function_as_statement_error_at env (fst body);

    Expect.token env T_WHILE;
    Expect.token env T_LPAREN;
    let test = Parse.expression env in
    Expect.token env T_RPAREN;
    (* The rules of automatic semicolon insertion in ES5 don't mention this,
     * but the semicolon after a do-while loop is optional. This is properly
     * specified in ES6 *)
    if Peek.token env = T_SEMICOLON
    then Eat.semicolon env;
    Statement.DoWhile { Statement.DoWhile.
      body;
      test;
    }
  )

  and for_ =
    let assert_can_be_forin_or_forof env err = function
      | loc, { Statement.VariableDeclaration.declarations; _; } ->
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
          | [ (_, { Statement.VariableDeclaration.Declarator.init = None; _; }) ] -> ()
          | _ -> error_at env (loc, err))
    in

    (* Annex B allows labelled FunctionDeclarations (see
       sec-labelled-function-declarations), but not in IterationStatement
       (see sec-semantics-static-semantics-early-errors). *)
    let assert_not_labelled_function env body =
      if not (in_strict_mode env) && is_labelled_function body
      then function_as_statement_error_at env (fst body)
      else ()
    in

    with_loc (fun env ->
      Expect.token env T_FOR;
      let async = allow_await env && Expect.maybe env T_AWAIT in
      Expect.token env T_LPAREN;

      let init, errs =
        let env = env |> with_no_in true in
        match Peek.token env with
        | T_SEMICOLON -> None, []
        | T_LET ->
            let loc, (decl, errs) = with_loc Declaration.let_ env in
            Some (For_declaration (loc, decl)), errs
        | T_CONST ->
            let loc, (decl, errs) = with_loc Declaration.const env in
            Some (For_declaration (loc, decl)), errs
        | T_VAR ->
            let loc, (decl, errs) = with_loc Declaration.var env in
            Some (For_declaration (loc, decl)), errs
        | _ ->
            let expr = Parse.expression_or_pattern (env |> with_no_let true) in
            Some (For_expression expr), []
      in

      match Peek.token env with
      (* If `async` is true, this must be a for-await-of loop. *)
      | t when t = T_OF || async ->
          let left = Statement.(match init with
          | Some (For_declaration decl) ->
            assert_can_be_forin_or_forof env Error.InvalidLHSInForOf decl;
            ForOf.LeftDeclaration decl
          | Some (For_expression expr) ->
            (* #sec-for-in-and-for-of-statements-static-semantics-early-errors *)
            let patt = Pattern_cover.as_pattern ~err:Error.InvalidLHSInForOf env expr in
            ForOf.LeftPattern patt
          | None -> assert false) in
          (* This is a for of loop *)
          Expect.token env T_OF;
          let right = Parse.assignment env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.ForOf { Statement.ForOf.
            left;
            right;
            body;
            async;
          }
      | T_IN ->
          let left = match init with
          | Some (For_declaration decl) ->
            assert_can_be_forin_or_forof env Error.InvalidLHSInForIn decl;
            Statement.ForIn.LeftDeclaration decl
          | Some (For_expression expr) ->
            (* #sec-for-in-and-for-of-statements-static-semantics-early-errors *)
            let patt = Pattern_cover.as_pattern ~err:Error.InvalidLHSInForIn env expr in
            Statement.ForIn.LeftPattern patt
          | None -> assert false in
          (* This is a for in loop *)
          Expect.token env T_IN;
          let right = Parse.expression env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.ForIn { Statement.ForIn.
            left;
            right;
            body;
            each = false;
          }
      | _ ->
          (* This is a for loop *)
          errs |> List.iter (error_at env);
          Expect.token env T_SEMICOLON;
          let init = match init with
          | Some (For_declaration decl) -> Some (Statement.For.InitDeclaration decl)
          | Some (For_expression expr) ->
            Some (Statement.For.InitExpression (Pattern_cover.as_expression env expr))
          | None -> None in
          let test = match Peek.token env with
          | T_SEMICOLON -> None
          | _ -> Some (Parse.expression env) in
          Expect.token env T_SEMICOLON;
          let update = match Peek.token env with
          | T_RPAREN -> None
          | _ -> Some (Parse.expression env) in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          assert_not_labelled_function env body;
          Statement.For { Statement.For.
            init;
            test;
            update;
            body;
          }
    )

  and if_ =
    (**
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
      if not (in_strict_mode env) && is_labelled_function stmt
      then function_as_statement_error_at env (fst stmt);

      stmt
    in

    with_loc (fun env ->
      Expect.token env T_IF;
      Expect.token env T_LPAREN;
      let test = Parse.expression env in
      Expect.token env T_RPAREN;
      let consequent = if_branch env in
      let alternate = if Peek.token env = T_ELSE
      then begin
        Expect.token env T_ELSE;
        Some (if_branch env)
      end else None in
      Statement.If { Statement.If.
        test;
        consequent;
        alternate;
      }
    )

  and return = with_loc (fun env ->
    if not (in_function env)
    then error env Error.IllegalReturn;
    Expect.token env T_RETURN;
    let argument =
      if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
      then None
      else Some (Parse.expression env) in
    Eat.semicolon env;
    Statement.Return { Statement.Return.
      argument;
    }
  )

  and switch =
    let rec case_list env (seen_default, acc) =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev acc
      | _ ->
        let start_loc = Peek.loc env in
        let test = match Peek.token env with
        | T_DEFAULT ->
            if seen_default
            then error env Error.MultipleDefaultsInSwitch;
            Expect.token env T_DEFAULT; None
        | _ ->
            Expect.token env T_CASE;
            Some (Parse.expression env) in
        let seen_default = seen_default || test = None in
        let end_loc = Peek.loc env in
        Expect.token env T_COLON;
        let term_fn = function
        | T_RCURLY | T_DEFAULT | T_CASE -> true
        | _ -> false in
        let consequent =
          Parse.statement_list ~term_fn (env |> with_in_switch true) in
        let end_loc = match List.rev consequent with
        | last_stmt::_ -> fst last_stmt
        | _ -> end_loc in
        let acc = (Loc.btwn start_loc end_loc, Statement.Switch.Case.({
          test;
          consequent;
        }))::acc in
        case_list env (seen_default, acc)

    in with_loc (fun env ->
      Expect.token env T_SWITCH;
      Expect.token env T_LPAREN;
      let discriminant = Parse.expression env in
      Expect.token env T_RPAREN;
      Expect.token env T_LCURLY;
      let cases = case_list env (false, []) in
      Expect.token env T_RCURLY;
      Statement.Switch { Statement.Switch.
        discriminant;
        cases;
      }
    )

  and throw = with_loc (fun env ->
    let start_loc = Peek.loc env in
    Expect.token env T_THROW;
    if Peek.is_line_terminator env
    then error_at env (start_loc, Error.NewlineAfterThrow);
    let argument = Parse.expression env in
    Eat.semicolon env;
    Statement.(Throw { Throw.argument; })
  )

  and try_ = with_loc (fun env ->
    Expect.token env T_TRY;
    let block = Parse.block_body env in
    let handler = match Peek.token env with
    | T_CATCH ->
        let catch = with_loc (fun env ->
          Expect.token env T_CATCH;
          Expect.token env T_LPAREN;
          let param = Parse.pattern env Error.StrictCatchVariable in
          Expect.token env T_RPAREN;
          let body = Parse.block_body env in
          { Ast.Statement.Try.CatchClause.
            param;
            body;
          }
        ) env in
        Some catch
    | _ -> None in
    let finalizer = match Peek.token env with
    | T_FINALLY ->
        Expect.token env T_FINALLY;
        Some (Parse.block_body env)
    | _ -> None in

    (* No catch or finally? That's an error! *)
    if handler = None && finalizer = None then
      error_at env (fst block, Error.NoCatchOrFinally);

    Statement.Try { Statement.Try.
      block;
      handler;
      finalizer;
    }
  )

  and var_or_const = with_loc (fun env ->
    let (_loc, declaration), errs = Declaration.variable env in
    Eat.semicolon env;
    errs |> List.iter (error_at env);
    declaration
  )

  and let_ = with_loc (fun env ->
    Expect.token env T_LET;
    (* Let declaration *)
    let declarations, errs = Declaration.variable_declaration_list (env |> with_no_let true) in
    let declaration =
      Ast.(Statement.VariableDeclaration Statement.VariableDeclaration.({
        declarations;
        kind = Let;
      })) in
    Eat.semicolon env;
    errs |> List.iter (error_at env);
    declaration
  )

  and while_ = with_loc (fun env ->
    Expect.token env T_WHILE;
    Expect.token env T_LPAREN;
    let test = Parse.expression env in
    Expect.token env T_RPAREN;
    let body = Parse.statement (env |> with_in_loop true) in

    (* Annex B allows labelled FunctionDeclarations in non-strict mode
       (see sec-labelled-function-declarations), but not in IterationStatement
       (see sec-semantics-static-semantics-early-errors). *)
    if not (in_strict_mode env) && is_labelled_function body
    then function_as_statement_error_at env (fst body);

    Statement.While { Statement.While.
      test;
      body;
    }
  )

  and with_ env =
    let loc, stmt = with_loc (fun env ->
      Expect.token env T_WITH;
      Expect.token env T_LPAREN;
      let _object = Parse.expression env in
      Expect.token env T_RPAREN;
      let body = Parse.statement env in

      (* Annex B allows labelled FunctionDeclarations in non-strict mode
         (see sec-labelled-function-declarations), but not in WithStatement
         (see sec-with-statement-static-semantics-early-errors). *)
      if not (in_strict_mode env) && is_labelled_function body
      then function_as_statement_error_at env (fst body);

      Statement.With { Statement.With.
        _object;
        body;
      }
    ) env in
    strict_error_at env (loc, Error.StrictModeWith);
    loc, stmt

  and block env =
    let loc, block = Parse.block_body env in
    loc, Statement.Block block

  and maybe_labeled = with_loc (fun env ->
    match (Parse.expression env, Peek.token env) with
    | ((loc, Ast.Expression.Identifier label), T_COLON) ->
        let _, name = label in
        Expect.token env T_COLON;
        if SSet.mem name (labels env)
        then error_at env (loc, Error.Redeclaration ("Label", name));
        let env = add_label env name in
        let body =
          (* labelled FunctionDeclarations are allowed in non-strict mode
             (see #sec-labelled-function-declarations) *)
          if Peek.is_function env then function_as_statement env
          else Parse.statement env
        in
        Statement.Labeled { Statement.Labeled.label; body; }
    | expression, _ ->
        Eat.semicolon env;
        Statement.(Expression { Expression.expression; directive = None; })
  )

  and expression = with_loc (fun env ->
    let expression = Parse.expression env in
    Eat.semicolon env;
    let directive = if allow_directive env
      then match expression with
      | _, Ast.Expression.Literal { Ast.Literal.
          value = Ast.Literal.String _;
          raw;
          _
        } -> Some (String.sub raw 1 (String.length raw - 2))
      | _ -> None
      else None
    in
    Statement.Expression { Statement.Expression.
      expression;
      directive;
    }
  )

  and type_alias_helper env =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeAlias;
    Expect.token env T_TYPE;
    Eat.push_lex_mode env Lex_mode.TYPE;
    let id = Type.type_identifier env in
    let typeParameters = Type.type_parameter_declaration_with_defaults env in
    Expect.token env T_ASSIGN;
    let right = Type._type env in
    Eat.semicolon env;
    Eat.pop_lex_mode env;
    Statement.TypeAlias.({
      id;
      typeParameters;
      right;
    })

  and declare_type_alias env = with_loc (fun env ->
    Expect.token env T_DECLARE;
    let type_alias = type_alias_helper env in
    Statement.DeclareTypeAlias type_alias
  ) env

  and type_alias env =
    if Peek.is_identifier ~i:1 env
    then
      let loc, type_alias = with_loc type_alias_helper env in
      loc, Statement.TypeAlias type_alias
    else
      Parse.statement env

  and opaque_type_helper ?(declare=false) env =
    if not (should_parse_types env)
    then error env Error.UnexpectedOpaqueTypeAlias;
    Expect.token env T_OPAQUE;
    Expect.token env T_TYPE;
    Eat.push_lex_mode env Lex_mode.TYPE;
    let id = Parse.identifier env in
    let typeParameters = Type.type_parameter_declaration_with_defaults env in
    let supertype = match Peek.token env with
    | T_COLON ->
        Expect.token env T_COLON;
        Some (Type._type env)
    | _ -> None in
    let impltype =
      if not declare then
        (Expect.token env T_ASSIGN;
        Some (Type._type env))
      else None in
    Eat.semicolon env;
    Eat.pop_lex_mode env;
    Statement.OpaqueType.({
      id;
      typeParameters;
      impltype;
      supertype;
    })

  and declare_opaque_type env = with_loc (fun env ->
    Expect.token env T_DECLARE;
    let opaque_t = opaque_type_helper ~declare:true env in
    Statement.DeclareOpaqueType opaque_t
  ) env

  and opaque_type env =
    match Peek.token ~i:1 env with
      T_TYPE ->
        let loc, opaque_t = with_loc (opaque_type_helper ~declare:false) env in
        loc, Statement.OpaqueType opaque_t
    | _ -> Parse.statement env

  and interface_helper =
    let rec supers env acc =
      let super = Type.generic env in
      let acc = super::acc in
      match Peek.token env with
      | T_COMMA ->
          Expect.token env T_COMMA;
          supers env acc
      | _ -> List.rev acc
    in
    fun env ->
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeInterface;
      Expect.token env T_INTERFACE;
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_declaration_with_defaults env in
      let extends = if Peek.token env = T_EXTENDS
      then begin
        Expect.token env T_EXTENDS;
        supers env []
      end else [] in
      let body = Type._object ~allow_static:true env in
      Statement.Interface.({
        id;
        typeParameters;
        body;
        extends;
        mixins = [];
      })

  and declare_interface env = with_loc (fun env ->
    Expect.token env T_DECLARE;
    let iface = interface_helper env in
    Statement.DeclareInterface iface
  ) env

  and interface env =
    if Peek.is_identifier ~i:1 env
    then
      let loc, iface = with_loc interface_helper env in
      loc, Statement.InterfaceDeclaration iface
    else expression env

  and declare_class =
    let rec supers env acc =
      let super = Type.generic env in
      let acc = super::acc in
      match Peek.token env with
      | T_COMMA ->
        Expect.token env T_COMMA;
        supers env acc
      | _ -> List.rev acc

    (* This is identical to `interface`, except that mixins are allowed *)
    in fun env ->
      let env = env |> with_strict true in
      Expect.token env T_CLASS;
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_declaration_with_defaults env in
      let extends = if Peek.token env = T_EXTENDS
        then begin
          Expect.token env T_EXTENDS;
          supers env []
        end else [] in
      let mixins = match Peek.token env with
      | T_IDENTIFIER { raw = "mixins"; _ } -> Eat.token env; supers env []
      | _ -> []
      in
      let body = Type._object ~allow_static:true env in
      Statement.Interface.({
        id;
        typeParameters;
        body;
        extends;
        mixins;
      })

  and declare_class_statement env = with_loc (fun env ->
    Expect.token env T_DECLARE;
    let fn = declare_class env in
    Statement.DeclareClass fn
  ) env

  and declare_function env =
    Expect.token env T_FUNCTION;
    let id = Parse.identifier env in
    let start_sig_loc = Peek.loc env in
    let typeParameters = Type.type_parameter_declaration env in
    let params = Type.function_param_list env in
    Expect.token env T_COLON;
    let returnType = Type._type env in
    let end_loc = fst returnType in
    let loc = Loc.btwn start_sig_loc end_loc in
    let typeAnnotation = loc, Ast.Type.(Function {Function.
      params;
      returnType;
      typeParameters;
    }) in
    let typeAnnotation = fst typeAnnotation, typeAnnotation in
    let id = Loc.btwn (fst id) end_loc, snd id in
    let predicate = Type.predicate_opt env in
    Eat.semicolon env;
    Statement.DeclareFunction.({
      id;
      typeAnnotation;
      predicate;
    })

  and declare_function_statement env = with_loc (fun env ->
    Expect.token env T_DECLARE;
    begin match Peek.token env with
    | T_ASYNC ->
      error env Error.DeclareAsync;
      Expect.token env T_ASYNC
    | _ -> ()
    end;
    let fn = declare_function env in
    Statement.DeclareFunction fn
  ) env

  and declare_var env =
    Expect.token env T_VAR;
    let _loc, { Pattern.Identifier.name; typeAnnotation; _; } =
      Parse.identifier_with_type env ~no_optional:true Error.StrictVarName in
    Eat.semicolon env;
    Statement.DeclareVariable.({ id=name; typeAnnotation; })

  and declare_var_statement env = with_loc (fun env ->
    Expect.token env T_DECLARE;
    let var = declare_var env in
    Statement.DeclareVariable var
  ) env

  and declare_module =
    let rec module_items env ~module_kind acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> (module_kind, List.rev acc)
      | _ ->
        let stmt = declare ~in_module:true env in
        let module_kind = Statement.(
          let (loc, stmt) = stmt in
          match (module_kind, stmt) with
          (**
           * The first time we see either a `declare export` or a
           * `declare module.exports`, we lock in the kind of the module.
           *
           * `declare export type` and `declare export interface` are the two
           * exceptions to this rule because they are valid in both CommonJS
           * and ES modules (and thus do not indicate an intent for either).
           *)
          | None, DeclareModuleExports _ -> Some (DeclareModule.CommonJS loc)
          | None, DeclareExportDeclaration {
              DeclareExportDeclaration.declaration;
              _;
            } ->
            (match declaration with
              | Some (DeclareExportDeclaration.NamedType _)
              | Some (DeclareExportDeclaration.Interface _)
                -> module_kind
              | _ -> Some (DeclareModule.ES loc)
            )

          (**
           * There should never be more than one `declare module.exports`
           * statement *)
          | Some (DeclareModule.CommonJS _), DeclareModuleExports _ ->
            error env Parse_error.DuplicateDeclareModuleExports;
            module_kind

          (**
           * It's never ok to mix and match `declare export` and
           * `declare module.exports` in the same module because it leaves the
           * kind of the module (CommonJS vs ES) ambiguous.
           *
           * The 1 exception to this rule is that `export type/interface` are
           * both ok in CommonJS modules.
           *)
          | Some (DeclareModule.ES _), DeclareModuleExports _ ->
            error env Parse_error.AmbiguousDeclareModuleKind;
            module_kind
          | Some (DeclareModule.CommonJS _), DeclareExportDeclaration {
              DeclareExportDeclaration.declaration;
              _;
            } ->
              (match declaration with
                | Some (DeclareExportDeclaration.NamedType _)
                | Some (DeclareExportDeclaration.Interface _)
                  -> ()
                | _ -> error env Parse_error.AmbiguousDeclareModuleKind
              );
              module_kind

          | _ -> module_kind
        ) in
        module_items env ~module_kind (stmt::acc)
    in

    let declare_module_ env start_loc =
      let id = match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          Statement.DeclareModule.Literal (loc, { Literal.value; raw; })
      | _ ->
          Statement.DeclareModule.Identifier (Parse.identifier env) in
      let body_loc, (module_kind, body) = with_loc (fun env ->
        Expect.token env T_LCURLY;
        let res = module_items env ~module_kind:None [] in
        Expect.token env T_RCURLY;
        res
      ) env in
      let body = body_loc, { Statement.Block.body; } in
      let loc = Loc.btwn start_loc body_loc in
      let kind =
        match module_kind with
        | Some k -> k
        | None -> Statement.DeclareModule.CommonJS loc
      in
      loc,
      Statement.(DeclareModule DeclareModule.({ id; body; kind; }))
    in
    fun ?(in_module=false) env ->
      let start_loc = Peek.loc env in
      Expect.token env T_DECLARE;
      Expect.identifier env "module";
      if in_module || Peek.token env = T_PERIOD
      then
        let loc, exports = with_loc declare_module_exports env in
        Loc.btwn start_loc loc, exports
      else
        declare_module_ env start_loc

  and declare_module_exports env =
    Expect.token env T_PERIOD;
    Expect.identifier env "exports";
    let type_annot = Type.annotation env in
    Eat.semicolon env;
    Statement.DeclareModuleExports type_annot

  and declare ?(in_module=false) env =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeDeclaration;
    (* eventually, just emit a wrapper AST node *)
    (match Peek.token ~i:1 env with
      | T_CLASS ->
          declare_class_statement env
      | T_INTERFACE ->
          declare_interface env
      | T_TYPE -> (
          match Peek.token env with
          | T_IMPORT when in_module ->
            import_declaration env
          | _ ->
            declare_type_alias env
        )
      | T_OPAQUE ->
          declare_opaque_type env
      | T_TYPEOF when (Peek.token env) = T_IMPORT ->
          import_declaration env
      | T_FUNCTION
      | T_ASYNC ->
          declare_function_statement env
      | T_VAR ->
          declare_var_statement env
      | T_EXPORT when in_module ->
          declare_export_declaration ~allow_export_type:in_module env
      | T_IDENTIFIER { raw = "module"; _ } ->
          declare_module ~in_module env
      | _ when in_module -> (
          match Peek.token env with
          | T_IMPORT ->
            error env Error.InvalidNonTypeImportInDeclareModule;
            Parse.statement env
          | _ ->
            (* Oh boy, found some bad stuff in a declare module. Let's just
              * pretend it's a declare var (arbitrary choice) *)
            declare_var_statement env
        )
      | _ ->
          Parse.statement env
    )

  and export_source env =
    Expect.identifier env "from";
    match Peek.token env with
    | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let value = Literal.String value in
        loc, { Literal.value; raw; }
    | _ ->
        (* Just make up a string for the error case *)
        let value = Literal.String "" in
        let ret = Peek.loc env, { Literal.value; raw = ""; } in
        error_unexpected env;
        ret

  and extract_pattern_binding_names =
    let rec fold acc = Pattern.(function
      | (_, Object {Object.properties; _;}) ->
        List.fold_left (fun acc prop ->
          match prop with
          | Object.Property (_, {Object.Property.pattern; _;})
          | Object.RestProperty (_, {Object.RestProperty.argument = pattern;})
            -> fold acc pattern
        ) acc properties
      | (_, Array {Array.elements; _;}) ->
        List.fold_left Array.(fun acc elem ->
          match elem with
          | Some (Element pattern)
          | Some (RestElement (_, {RestElement.argument = pattern;}))
            -> fold acc pattern
          | None -> acc
        ) acc elements
      | (_, Assignment {Assignment.left;_;}) -> fold acc left
      | (_, Identifier {Pattern.Identifier.name; _; }) ->
        name::acc
      | (_, Expression _) ->
        failwith "Parser error: No such thing as an expression pattern!"
    ) in
    List.fold_left fold

  and extract_ident_name (_, name) = name

  and export_specifiers env specifiers =
    match Peek.token env with
    | T_EOF
    | T_RCURLY ->
        List.rev specifiers
    | _ ->
        let specifier = with_loc (fun env ->
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
          { Statement.ExportNamedDeclaration.ExportSpecifier.local; exported; }
        ) env in
        if Peek.token env = T_COMMA
        then Expect.token env T_COMMA;
        export_specifiers env (specifier::specifiers)

  and assert_export_specifier_identifiers env specifiers =
    let open Statement.ExportNamedDeclaration.ExportSpecifier in
    List.iter (function
      | _, { local = id; exported = None; } ->
        Parse.assert_identifier_name_is_identifier
          ~restricted_error:Parse_error.StrictVarName env id
      | _ ->
        ()
    ) specifiers

  and export_declaration ~decorators = with_loc (fun env ->
    let env = env |> with_strict true |> with_in_export true in
    let start_loc = Peek.loc env in
    Expect.token env T_EXPORT;
    match Peek.token env with
    | T_DEFAULT ->
        (* export default ... *)
        let open Statement.ExportDefaultDeclaration in
        Expect.token env T_DEFAULT;
        record_export env (Loc.btwn start_loc (Peek.loc env), "default");
        let declaration = match Peek.token env with
        | T_FUNCTION ->
            (* export default function foo (...) { ... } *)
            let fn = Declaration._function env in
            Declaration fn
        | _ when Peek.is_class env ->
            (* export default class foo { ... } *)
            let _class = Object.class_declaration env decorators in
            Declaration _class
        | _ ->
            (* export default [assignment expression]; *)
            let expr = Parse.assignment env in
            Eat.semicolon env;
            Expression expr
          in
        Statement.ExportDefaultDeclaration {
          declaration;
          exportKind = Statement.ExportValue;
        }
    | T_TYPE when (Peek.token env ~i:1) <> T_LCURLY ->
        (* export type ... *)
        let open Statement.ExportNamedDeclaration in
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeExport;
        (match Peek.token ~i:1 env with
        | T_MULT ->
          Expect.token env T_TYPE;
          let specifier_loc = Peek.loc env in
          Expect.token env T_MULT;
          let source = export_source env in
          Eat.semicolon env;
          Statement.ExportNamedDeclaration {
            declaration = None;
            specifiers = Some (ExportBatchSpecifier (specifier_loc, None));
            source = Some source;
            exportKind = Statement.ExportType;
          }
        | _ ->
          let loc, type_alias = with_loc type_alias_helper env in
          record_export env (loc, extract_ident_name type_alias.Statement.TypeAlias.id);
          let type_alias = (loc, Statement.TypeAlias type_alias) in
          Statement.ExportNamedDeclaration {
            declaration = Some type_alias;
            specifiers = None;
            source = None;
            exportKind = Statement.ExportType;
          }
        )
    | T_OPAQUE ->
        (* export opaque type ... *)
        let open Statement.ExportNamedDeclaration in
        let loc, opaque_t = with_loc opaque_type_helper env in
        record_export env (loc, extract_ident_name opaque_t.Statement.OpaqueType.id);
        let opaque_t = (loc, Statement.OpaqueType opaque_t) in
        Statement.ExportNamedDeclaration {
          declaration = Some opaque_t;
          specifiers = None;
          source = None;
          exportKind = Statement.ExportType;
        }
    | T_INTERFACE ->
        (* export interface I { ... } *)
        let open Statement.ExportNamedDeclaration in
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeExport;
        let interface = interface env in
        (match interface with
          | (loc, Statement.InterfaceDeclaration {Statement.Interface.id; _;}) ->
            record_export env (loc, extract_ident_name id)
          | _ -> failwith (
              "Internal Flow Error! Parsed `export interface` into something " ^
              "other than an interface declaration!"
            )
        );
        Statement.ExportNamedDeclaration {
          declaration = Some interface;
          specifiers = None;
          source = None;
          exportKind = Statement.ExportType;
        }
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
    | T_FUNCTION ->
        let open Statement.ExportNamedDeclaration in
        let stmt = Parse.statement_list_item env ~decorators:decorators in
        let names = Statement.(
          match stmt with
          | (_, VariableDeclaration { VariableDeclaration.declarations; _; }) ->
            List.fold_left (fun names (_, declaration) ->
              let id = declaration.VariableDeclaration.Declarator.id in
              extract_pattern_binding_names names [id]
            ) [] declarations
          | (loc, ClassDeclaration { Class.id = Some id; _; })
          | (loc, FunctionDeclaration { Function.id = Some id; _; })
            -> [(loc, extract_ident_name id)]
          | (loc, ClassDeclaration { Class.id = None; _; }) ->
            error_at env (loc, Error.ExportNamelessClass);
            []
          | (loc, FunctionDeclaration { Function.id = None; _; }) ->
            error_at env (loc, Error.ExportNamelessFunction);
            []
          | _ -> failwith "Internal Flow Error! Unexpected export statement declaration!"
        ) in
        List.iter (record_export env) names;
        Statement.ExportNamedDeclaration {
          declaration = Some stmt;
          specifiers = None;
          source = None;
          exportKind = Statement.ExportValue;
        }
    | T_MULT ->
        let open Statement.ExportNamedDeclaration in
        let loc = Peek.loc env in
        Expect.token env T_MULT;
        let local_name =
          let parse_export_star_as =
            (parse_options env).esproposal_export_star_as
          in
          match Peek.token env with
          | T_IDENTIFIER { raw = "as"; _ } ->
            Eat.token env;
            if parse_export_star_as
            then Some (Parse.identifier env)
            else (error env Error.UnexpectedTypeDeclaration; None)
          | _ ->
            None
        in
        let specifiers =
          Some (ExportBatchSpecifier (loc, local_name))
        in
        let source = export_source env in
        let source = Some source in
        Eat.semicolon env;
        Statement.ExportNamedDeclaration {
          declaration = None;
          specifiers;
          source;
          exportKind = Statement.ExportValue;
        }
    | _ ->
        let open Statement.ExportNamedDeclaration in
        let exportKind = (
          match Peek.token env with
          | T_TYPE -> Eat.token env; Statement.ExportType
          | _ -> Statement.ExportValue
        ) in
        Expect.token env T_LCURLY;
        let specifiers = export_specifiers env [] in
        Expect.token env T_RCURLY;
        let source =
          match Peek.token env with
          | T_IDENTIFIER { raw = "from"; _ } ->
            Some (export_source env)
          | _ ->
            assert_export_specifier_identifiers env specifiers;
            None
        in
        Eat.semicolon env;
        Statement.ExportNamedDeclaration {
          declaration = None;
          specifiers = Some (ExportSpecifiers specifiers);
          source;
          exportKind;
        }
  )

  and declare_export_declaration ?(allow_export_type=false) = with_loc (fun env ->
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeDeclaration;
    Expect.token env T_DECLARE;

    let env = env |> with_strict true |> with_in_export true in
    Expect.token env T_EXPORT;
    Statement.DeclareExportDeclaration.(match Peek.token env with
    | T_DEFAULT ->
        (* declare export default ... *)
        Expect.token env T_DEFAULT;
        let declaration = match Peek.token env with
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
            Eat.semicolon env;
            Some (DefaultType type_)
          in
        Statement.DeclareExportDeclaration {
          default = true;
          declaration;
          specifiers = None;
          source = None;
        }
    | T_LET
    | T_CONST
    | T_VAR
    | T_CLASS
    | T_FUNCTION ->
        let declaration = match Peek.token env with
        | T_FUNCTION ->
            (* declare export function foo (...): ...  *)
            let fn = with_loc declare_function env in
            Some (Function fn)
        | T_CLASS ->
            (* declare export class foo { ... } *)
            let class_ = with_loc declare_class env in
            Some (Class class_)
        | T_LET
        | T_CONST
        | T_VAR as token ->
            (match token with
            | T_LET -> error env Error.DeclareExportLet
            | T_CONST -> error env Error.DeclareExportConst
            | _ -> ());
            (* declare export var foo: ... *)
            let var = with_loc declare_var env in
            Some (Variable var)
        | _ -> assert false in
        Statement.DeclareExportDeclaration {
          default = false;
          declaration;
          specifiers = None;
          source = None;
        }
    | T_MULT ->
        (* declare export * from 'foo' *)
        let loc = Peek.loc env in
        Expect.token env T_MULT;
        let parse_export_star_as =
          (parse_options env).esproposal_export_star_as
        in
        let local_name =
          match Peek.token env with
          | T_IDENTIFIER { raw = "as"; _ } ->
            Eat.token env;
            if parse_export_star_as
            then Some (Parse.identifier env)
            else (error env Error.UnexpectedTypeDeclaration; None)
          | _ ->
            None
        in
        let specifiers = Statement.ExportNamedDeclaration.(
          Some (ExportBatchSpecifier (loc, local_name))
        ) in
        let source = export_source env in
        Eat.semicolon env;
        Statement.DeclareExportDeclaration {
          default = false;
          declaration = None;
          specifiers;
          source = Some source;
        }
    | T_TYPE when allow_export_type ->
        (* declare export type = ... *)
        let alias = with_loc type_alias_helper env in
        Statement.DeclareExportDeclaration {
          default = false;
          declaration = Some (NamedType alias);
          specifiers = None;
          source = None;
        }
    | T_OPAQUE ->
        (* declare export opaque type = ... *)
        let opaque = with_loc (opaque_type_helper ~declare:true) env in
        Statement.DeclareExportDeclaration {
          default = false;
          declaration = Some (NamedOpaqueType opaque);
          specifiers = None;
          source = None;
        }
    | T_INTERFACE when allow_export_type ->
        (* declare export interface ... *)
        let iface = with_loc interface_helper env in
        Statement.DeclareExportDeclaration {
          default = false;
          declaration = Some (Interface iface);
          specifiers = None;
          source = None;
        }
    | _ ->
        (match Peek.token env with
          | T_TYPE -> error env Error.DeclareExportType
          | T_INTERFACE -> error env Error.DeclareExportInterface
          | _ -> ()
        );
        Expect.token env T_LCURLY;
        let specifiers = export_specifiers env [] in
        Expect.token env T_RCURLY;
        let source =
          match Peek.token env with
          | T_IDENTIFIER { raw = "from"; _ } ->
            Some (export_source env)
          | _ ->
            assert_export_specifier_identifiers env specifiers;
            None
        in
        Eat.semicolon env;
        Statement.DeclareExportDeclaration {
          default = false;
          declaration = None;
          specifiers = Some (Statement.ExportNamedDeclaration.ExportSpecifiers specifiers);
          source;
        }
    )
  )

  and import_declaration =
    let open Statement.ImportDeclaration in

    let source env =
      Expect.identifier env "from";
      match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          loc, { Literal.value; raw; }
      | _ ->
          (* Just make up a string for the error case *)
          let value = Literal.String "" in
          let ret = Peek.loc env, { Literal.value; raw = ""; } in
          error_unexpected env;
          ret

    in let rec specifier_list ?(preceding_comma=true) env is_type_import acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev acc
      | _ ->
        if not preceding_comma then error env Error.ImportSpecifierMissingComma;
        let remote = identifier_name env in
        let starts_w_type, kind =
          match snd remote with
          | "type" -> true, Some ImportType
          | "typeof" -> true, Some ImportTypeof
          | _ -> false, None
        in
        let specifier =
          let is_as = match Peek.token env with
            | T_IDENTIFIER { raw = "as"; _ } -> true
            | _ -> false
          in
          if is_as then begin
            let as_ident = Parse.identifier env in
            if starts_w_type && not (Peek.is_identifier env) then begin
              (* `import {type as ,` or `import {type as }` *)
              if is_type_import then
                error_at env (fst remote, Error.ImportTypeShorthandOnlyInPureImport);
              { remote = as_ident; local = None; kind }
            end else begin
              (* `import {type as foo` *)
              let local = Some (Parse.identifier env) in
              { remote; local; kind = None; }
            end
          end else if starts_w_type && Peek.is_identifier env then begin
            (* `import {type foo` *)
            if is_type_import then
              error_at env (fst remote, Error.ImportTypeShorthandOnlyInPureImport);
            let remote = identifier_name env in
            let local =
              match Peek.token env with
              | T_IDENTIFIER { raw = "as"; _ } ->
                Eat.token env;
                Some (Type.type_identifier env)
              | _ ->
                None
            in
            { remote; local; kind }
          end else begin
            Parse.assert_identifier_name_is_identifier env remote;
            { remote; local = None; kind = None; }
          end
        in
        let preceding_comma = Expect.maybe env T_COMMA in
        specifier_list ~preceding_comma env is_type_import (ImportNamedSpecifier specifier::acc)

    in let named_or_namespace_specifier env is_type_import =
      let start_loc = Peek.loc env in
      match Peek.token env with
      | T_MULT ->
          Expect.token env T_MULT;
          Expect.identifier env "as";
          let id = Parse.identifier env in
          [ImportNamespaceSpecifier (Loc.btwn start_loc (fst id), id)]
      | _ ->
          Expect.token env T_LCURLY;
          let specifiers = specifier_list env is_type_import [] in
          Expect.token env T_RCURLY;
          specifiers

    in with_loc (fun env ->
      let env = env |> with_strict true in
      Expect.token env T_IMPORT;
      (* It might turn out that we need to treat this "type" token as an
       * identifier, like import type from "module" *)
      let importKind, type_ident =
        match Peek.token env with
        | T_TYPE ->
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeImport;
          ImportType, Some(Parse.identifier env)
        | T_TYPEOF ->
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeImport;
          Expect.token env T_TYPEOF;
          ImportTypeof, None
        | _ -> ImportValue, None
      in
      let is_type_import = importKind <> ImportValue in
      match Peek.token env, Peek.is_identifier env with
      (* import "ModuleName"; *)
      | T_STRING (str_loc, value, raw, octal), _
          when importKind = ImportValue ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (str_loc, value, raw, octal));
        let value = Literal.String value in
        let source = (str_loc, { Literal.value; raw; }) in
        Eat.semicolon env;
        Statement.ImportDeclaration {
          importKind;
          source;
          specifiers = [];
        }

      (* import [type] SomeDefault ... *)
      | T_COMMA, _ (* `import type, ...` *)
      | _, true -> (* `import type Foo` or `import type from` *)
          let importKind, default_specifier = (
            match type_ident, Peek.token env with
            | Some type_ident, T_COMMA (* `import type,` *)
            | Some type_ident, T_IDENTIFIER { raw = "from"; _ } -> (* `import type from` *)
              ImportValue, ImportDefaultSpecifier type_ident
            | _ -> (* Either `import type Foo` or `import Foo` *)
              importKind, ImportDefaultSpecifier (Parse.identifier env)
          ) in

          let additional_specifiers = (
            match Peek.token env with
            | T_COMMA -> (* `import Foo, ...` *)
                Expect.token env T_COMMA;
                named_or_namespace_specifier env is_type_import
            | _ -> []
          ) in

          let source = source env in
          Eat.semicolon env;
          Statement.ImportDeclaration {
            importKind;
            source;
            specifiers = default_specifier::additional_specifiers;
          }

      (* `import [type] { ... } ...` or `import [typeof] * as ...` *)
      | _ ->
          let specifiers = named_or_namespace_specifier env is_type_import in
          let source = source env in
          Eat.semicolon env;
          Statement.ImportDeclaration {
            importKind;
            source;
            specifiers;
          }
    )
end
