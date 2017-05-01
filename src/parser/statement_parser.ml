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
 val _for: env -> Statement.t
 val _if: env -> Statement.t
 val _let: env -> Statement.t
 val _try: env -> Statement.t
 val _while: env -> Statement.t
 val _with: env -> Statement.t
 val block: env -> Statement.t
 val break: env -> Statement.t
 val continue: env -> Statement.t
 val debugger: env -> Statement.t
 val declare: ?in_module:bool -> env -> Statement.t
 val declare_export_declaration: ?allow_export_type:bool -> env -> Statement.t
 val do_while: env -> Statement.t
 val empty: env -> Statement.t
 val export_declaration: env -> Expression.t list -> Statement.t
 val expression: env -> Statement.t
 val import_declaration: env -> Statement.t
 val interface: env -> Statement.t
 val maybe_labeled: env -> Statement.t
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
: STATEMENT = struct
  let rec empty env =
    let loc = Peek.loc env in
    Expect.token env T_SEMICOLON;
    loc, Statement.Empty

  and break env =
    let start_loc = Peek.loc env in
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
    let end_loc = match Peek.semicolon_loc env with
    | Some loc -> loc
    | None -> (match label with
      | Some id -> fst id
      | None -> start_loc) in
    let loc = Loc.btwn start_loc end_loc in
    if label = None && not (in_loop env || in_switch env)
    then error_at env (loc, Error.IllegalBreak);
    Eat.semicolon env;
    loc, Statement.Break {
      Statement.Break.label = label;
    }

  and continue env =
    let start_loc = Peek.loc env in
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
    let end_loc = match Peek.semicolon_loc env with
    | Some loc -> loc
    | None -> (match label with
      | Some id -> fst id
      | None -> start_loc) in
    let loc = Loc.btwn start_loc end_loc in
    if not (in_loop env)
    then error_at env (loc, Error.IllegalContinue);
    Eat.semicolon env;
    loc, Statement.Continue {
      Statement.Continue.label = label;
    }

  and debugger env =
    let start_loc = Peek.loc env in
    Expect.token env T_DEBUGGER;
    let end_loc = match Peek.semicolon_loc env with
    | None -> start_loc
    | Some loc -> loc in
    Eat.semicolon env;
    Loc.btwn start_loc end_loc, Statement.Debugger;

  and do_while env =
    let start_loc = Peek.loc env in
    Expect.token env T_DO;
    let body = Parse.statement (env |> with_in_loop true) in
    Expect.token env T_WHILE;
    Expect.token env T_LPAREN;
    let test = Parse.expression env in
    let end_loc = Peek.loc env in
    Expect.token env T_RPAREN;
    let end_loc = match Peek.semicolon_loc env with
    | None -> end_loc
    | Some loc -> loc in
    (* The rules of automatic semicolon insertion in ES5 don't mention this,
     * but the semicolon after a do-while loop is optional. This is properly
     * specified in ES6 *)
    if Peek.token env = T_SEMICOLON
    then Eat.semicolon env;
    Loc.btwn start_loc end_loc, Statement.(DoWhile DoWhile.({
      body;
      test;
    }))

  and _for =
    let assert_can_be_forin_or_forof env err = Statement.VariableDeclaration.(function
      | Some (Statement.For.InitDeclaration (loc, {
        declarations;
        _;
      })) ->
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
          | [ (_, { Declarator.init = None; _; }) ] -> ()
          | _ -> error_at env (loc, err))
      | Some (Statement.For.InitExpression (loc, expr)) ->
          (* Only certain expressions can be the lhs of a for in or for of *)
          if not (Parse.is_assignable_lhs (loc, expr))
          then error_at env (loc, err)
      | _ -> error env err
    ) in

    fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_FOR;
      let async = allow_await env && Expect.maybe env T_AWAIT in
      Expect.token env T_LPAREN;

      let init, errs =
        match Peek.token env with
        | T_SEMICOLON -> None, []
        | T_LET ->
            let decl, errs = Declaration._let (env |> with_no_in true) in
            Some (Statement.For.InitDeclaration decl), errs
        | T_CONST ->
            let decl, errs = Declaration.const (env |> with_no_in true) in
            Some (Statement.For.InitDeclaration decl), errs
        | T_VAR ->
            let decl, errs = Declaration.var (env |> with_no_in true) in
            Some (Statement.For.InitDeclaration decl), errs
        | _ ->
            let expr = Parse.expression (env |> with_no_in true |> with_no_let true) in
            Some (Statement.For.InitExpression expr), []
      in

      match Peek.token env with
      (* If `async` is true, this must be a for-await-of loop. *)
      | t when t = T_OF || async ->
          assert_can_be_forin_or_forof env Error.InvalidLHSInForOf init;
          let left = Statement.(match init with
          | Some (For.InitDeclaration decl) -> ForOf.LeftDeclaration decl
          | Some (For.InitExpression expr) -> ForOf.LeftExpression expr
          | None -> assert false) in
          (* This is a for of loop *)
          Expect.token env T_OF;
          let right = Parse.assignment env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          Loc.btwn start_loc (fst body), Statement.(ForOf ForOf.({
            left;
            right;
            body;
            async;
          }))
      | T_IN ->
          assert_can_be_forin_or_forof env Error.InvalidLHSInForIn init;
          let left = Statement.(match init with
          | Some (For.InitDeclaration decl) -> ForIn.LeftDeclaration decl
          | Some (For.InitExpression expr) -> ForIn.LeftExpression expr
          | None -> assert false) in
          (* This is a for in loop *)
          Expect.token env T_IN;
          let right = Parse.expression env in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          Loc.btwn start_loc (fst body), Statement.(ForIn ForIn.({
            left;
            right;
            body;
            each = false;
          }))
      | _ ->
          (* This is a for loop *)
          errs |> List.iter (error_at env);
          Expect.token env T_SEMICOLON;
          let test = match Peek.token env with
          | T_SEMICOLON -> None
          | _ -> Some (Parse.expression env) in
          Expect.token env T_SEMICOLON;
          let update = match Peek.token env with
          | T_RPAREN -> None
          | _ -> Some (Parse.expression env) in
          Expect.token env T_RPAREN;
          let body = Parse.statement (env |> with_in_loop true) in
          Loc.btwn start_loc (fst body), Statement.(For For.({
            init;
            test;
            update;
            body;
          }))

  and _if env =
    let start_loc = Peek.loc env in
    Expect.token env T_IF;
    Expect.token env T_LPAREN;
    let test = Parse.expression env in
    Expect.token env T_RPAREN;
    let consequent = match Peek.token env with
    | _ when Peek.is_function env ->
        strict_error env Error.StrictFunctionStatement;
        Declaration._function env
    | _ -> Parse.statement env in
    let alternate = if Peek.token env = T_ELSE
    then begin
      Expect.token env T_ELSE;
      Some (Parse.statement env)
    end else None in
    let end_loc = match alternate with
    | Some stmt -> fst stmt
    | None -> fst consequent in
    Loc.btwn start_loc end_loc, Statement.(If If.({
      test;
      consequent;
      alternate;
    }))

  and return env =
    if not (in_function env)
    then error env Error.IllegalReturn;
    let start_loc = Peek.loc env in
    Expect.token env T_RETURN;
    let argument =
      if Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
      then None
      else Some (Parse.expression env) in
    let end_loc = match Peek.semicolon_loc env with
    | Some loc -> loc
    | None -> (match argument with
      | Some argument -> fst argument
      | None -> start_loc) in
    Eat.semicolon env;
    Loc.btwn start_loc end_loc, Statement.(Return Return.({
      argument;
    }))

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

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_SWITCH;
      Expect.token env T_LPAREN;
      let discriminant = Parse.expression env in
      Expect.token env T_RPAREN;
      Expect.token env T_LCURLY;
      let cases = case_list env (false, []) in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Loc.btwn start_loc end_loc, Statement.(Switch Switch.({
        discriminant;
        cases;
      }))

  and throw env =
    let start_loc = Peek.loc env in
    Expect.token env T_THROW;
    if Peek.is_line_terminator env
    then error_at env (start_loc, Error.NewlineAfterThrow);
    let argument = Parse.expression env in
    let end_loc = match Peek.semicolon_loc env with
    | Some loc -> loc
    | None -> fst argument in
    Eat.semicolon env;
    Loc.btwn start_loc end_loc, Statement.(Throw Throw.({
      argument;
    }))

  and _try env =
    let start_loc = Peek.loc env in
    Expect.token env T_TRY;
    let block = Parse.block_body env in
    let handler = match Peek.token env with
    | T_CATCH ->
        let start_loc = Peek.loc env in
        Expect.token env T_CATCH;
        Expect.token env T_LPAREN;
        let id = Parse.identifier ~restricted_error:Error.StrictCatchVariable env in
        let param = fst id, Pattern.Identifier {
          Pattern.Identifier.name=id;
                             typeAnnotation=None;
                             optional=false;
        } in
        Expect.token env T_RPAREN;
        let body = Parse.block_body env in
        let loc = Loc.btwn start_loc (fst body) in
        Some (loc, Ast.Statement.Try.CatchClause.({
          param;
          body;
        }))
    | _ -> None in
    let finalizer = match Peek.token env with
    | T_FINALLY ->
        Expect.token env T_FINALLY;
        Some (Parse.block_body env)
    | _ -> None in
    let end_loc = match finalizer with
    | Some finalizer -> fst finalizer
    | None ->
        (match handler with
        | Some handler -> fst handler
        | None ->
            (* No catch or finally? That's an error! *)
            error_at env (fst block, Error.NoCatchOrFinally);
            fst block) in
    Loc.btwn start_loc end_loc, Statement.(Try Try.({
      block;
      handler;
      finalizer;
    }));

  and var_or_const env =
    let (start_loc, declaration), errs = Declaration.variable env in
    let end_loc = match Peek.semicolon_loc env with
    | None -> start_loc
    | Some end_loc -> end_loc in
    Eat.semicolon env;
    errs |> List.iter (error_at env);
    Loc.btwn start_loc end_loc, declaration

  and _let env =
    let start_loc = Peek.loc env in
    Expect.token env T_LET;
    (* Let declaration *)
    let end_loc, declarations, errs =
      Declaration.variable_declaration_list (env |> with_no_let true) in
    let declaration =
      Ast.(Statement.VariableDeclaration Statement.VariableDeclaration.({
        declarations;
        kind = Let;
      })) in
    let end_loc = match Peek.semicolon_loc env with
    | None -> end_loc
    | Some end_loc -> end_loc in
    Eat.semicolon env;
    errs |> List.iter (error_at env);
    Loc.btwn start_loc end_loc, declaration

  and _while env =
    let start_loc = Peek.loc env in
    Expect.token env T_WHILE;
    Expect.token env T_LPAREN;
    let test = Parse.expression env in
    Expect.token env T_RPAREN;
    let body = Parse.statement (env |> with_in_loop true) in
    Loc.btwn start_loc (fst body), Statement.(While While.({
      test;
      body;
    }));

  and _with env =
    let start_loc = Peek.loc env in
    Expect.token env T_WITH;
    Expect.token env T_LPAREN;
    let _object = Parse.expression env in
    Expect.token env T_RPAREN;
    let body = Parse.statement env in
    let loc = Loc.btwn start_loc (fst body) in
    strict_error_at env (loc, Error.StrictModeWith);
    loc, Statement.(With With.({
      _object;
      body;
    }))

  and block env =
    let loc, block = Parse.block_body env in
    loc, Statement.Block block

  and maybe_labeled env =
    let expr = Parse.expression env in
    match (expr, Peek.token env) with
    | ((loc, Ast.Expression.Identifier label), T_COLON) ->
        let _, name = label in
        Expect.token env T_COLON;
        if SSet.mem name (labels env)
        then error_at env (loc, Error.Redeclaration ("Label", name));
        let env = add_label env name in
        let labeled_stmt = Parse.statement env in
        Loc.btwn loc (fst labeled_stmt), Statement.Labeled {
          Statement.Labeled.label = label;
          Statement.Labeled.body = labeled_stmt;
        }
    | expression, _ ->
        let end_loc = match Peek.semicolon_loc env with
        | Some loc -> loc
        | None -> (fst expression) in
        Eat.semicolon env;
        Loc.btwn (fst expression) end_loc, Statement.(Expression Expression.({
          expression;
          directive = None;
        }))

  and expression env =
    let loc, expression = with_loc Parse.expression env in
    let loc = match Peek.semicolon_loc env with
    | Some semicolon_loc -> Loc.btwn loc semicolon_loc
    | None -> loc in
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
    loc, Statement.(Expression Expression.({
      expression;
      directive;
    }))

  and type_alias_helper env =
    let start_loc = Peek.loc env in
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeAlias;
    Expect.token env T_TYPE;
    Eat.push_lex_mode env Lex_mode.TYPE;
    let id = Parse.identifier env in
    let typeParameters = Type.type_parameter_declaration_with_defaults env in
    Expect.token env T_ASSIGN;
    let right = Type._type env in
    let end_loc = match Peek.semicolon_loc env with
    | None -> fst right
    | Some end_loc -> end_loc in
    Eat.semicolon env;
    Eat.pop_lex_mode env;
    Loc.btwn start_loc end_loc, Statement.TypeAlias.({
      id;
      typeParameters;
      right;
    })

  and type_alias env =
    if Peek.is_identifier ~i:1 env
    then
      let loc, type_alias = type_alias_helper env in
      loc, Statement.TypeAlias type_alias
    else
      Parse.statement env

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
      let start_loc = Peek.loc env in
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
      let loc = Loc.btwn start_loc (fst body) in
      loc, Statement.Interface.({
        id;
        typeParameters;
        body;
        extends;
        mixins = [];
      })


  and interface env =
    if Peek.is_identifier ~i:1 env
    then
      let loc, iface = interface_helper env in
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
    in fun env start_loc ->
      let env = env |> with_strict true in
      Expect.token env T_CLASS;
      let id = Parse.identifier env in
      let typeParameters = Type.type_parameter_declaration_with_defaults env in
      let extends = if Peek.token env = T_EXTENDS
        then begin
          Expect.token env T_EXTENDS;
          supers env []
        end else [] in
      let mixins = if Peek.value env = "mixins"
        then begin
          Expect.contextual env "mixins";
          supers env []
        end else [] in
      let body = Type._object ~allow_static:true env in
      let loc = Loc.btwn start_loc (fst body) in
      loc, Statement.Interface.({
        id;
        typeParameters;
        body;
        extends;
        mixins;
      })

  and declare_class_statement env start_loc =
    let loc, fn = declare_class env start_loc in
    loc, Statement.DeclareClass fn

  and declare_function env start_loc =
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
    let end_loc = match Peek.semicolon_loc env with
    | None ->
      begin match predicate with
      | Some (end_loc, _) -> end_loc
      | None -> end_loc
      end
    | Some end_loc -> end_loc in
    Eat.semicolon env;
    let loc = Loc.btwn start_loc end_loc in
    loc, Statement.DeclareFunction.({
      id;
      typeAnnotation;
      predicate;
    })

  and declare_function_statement env start_loc =
    let loc, fn = declare_function env start_loc in
    loc, Statement.DeclareFunction fn

  and declare_var env start_loc =
    Expect.token env T_VAR;
    let loc, { Pattern.Identifier.name; typeAnnotation; _; } =
      Parse.identifier_with_type env ~no_optional:true Error.StrictVarName in
    let end_loc = match Peek.semicolon_loc env with
    | None -> loc
    | Some loc -> loc in
    let loc = Loc.btwn start_loc end_loc in
    Eat.semicolon env;
    loc, Statement.DeclareVariable.({ id=name; typeAnnotation; })

  and declare_var_statement env start_loc =
    let loc, var = declare_var env start_loc in
    loc, Statement.DeclareVariable var

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

    in fun env start_loc ->
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

  and declare_module_exports env start_loc =
    Expect.token env T_PERIOD;
    Expect.contextual env "exports";
    let type_annot = Type.annotation env in
    let end_loc =
      match Peek.semicolon_loc env with
      | Some loc -> loc
      | None -> fst type_annot
    in
    Eat.semicolon env;
    let loc = Loc.btwn start_loc end_loc in
    (loc, Statement.DeclareModuleExports type_annot)

  and declare ?(in_module=false) env =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeDeclaration;
    let start_loc = Peek.loc env in
    (* eventually, just emit a wrapper AST node *)
    (match Peek.token ~i:1 env with
      | T_CLASS ->
          Expect.token env T_DECLARE;
          declare_class_statement env start_loc
      | T_INTERFACE ->
          Expect.token env T_DECLARE;
          interface env
      | T_TYPE -> (
          match Peek.token env with
          | T_IMPORT when in_module ->
            import_declaration env
          | _ ->
            Expect.token env T_DECLARE;
            type_alias env;
        )
      | T_TYPEOF when (Peek.token env) = T_IMPORT ->
        import_declaration env
      | T_FUNCTION ->
          Expect.token env T_DECLARE;
          declare_function_statement env start_loc
      | T_VAR ->
          Expect.token env T_DECLARE;
          declare_var_statement env start_loc
      | T_ASYNC ->
          Expect.token env T_DECLARE;
          error env Error.DeclareAsync;
          Expect.token env T_ASYNC;
          declare_function_statement env start_loc
      | T_EXPORT when in_module ->
          declare_export_declaration ~allow_export_type:in_module env
      | T_IDENTIFIER when Peek.value ~i:1 env = "module" ->
          Expect.token env T_DECLARE;
          Expect.contextual env "module";
          if in_module || Peek.token env = T_PERIOD
          then declare_module_exports env start_loc
          else declare_module env start_loc
      | _ when in_module -> (
          match Peek.token env with
          | T_IMPORT ->
            error env Error.InvalidNonTypeImportInDeclareModule;
            Parse.statement env
          | _ ->
            (* Oh boy, found some bad stuff in a declare module. Let's just
              * pretend it's a declare var (arbitrary choice) *)
            Expect.token env T_DECLARE;
            declare_var_statement env start_loc
        )
      | _ ->
          Parse.statement env
    )

  and export_source env =
    Expect.contextual env "from";
    match Peek.token env with
    | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let value = Literal.String value in
        loc, { Literal.value; raw; }
    | _ ->
        (* Just make up a string for the error case *)
        let raw = Peek.value env in
        let value = Literal.String raw in
        let ret = Peek.loc env, { Literal.value; raw; } in
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

  and export_specifiers_and_errs env specifiers errs =
    match Peek.token env with
    | T_EOF
    | T_RCURLY ->
        List.rev specifiers, List.rev errs
    | _ ->
        let local, err = Parse.identifier_or_reserved_keyword env in
        let exported, err, end_loc = if Peek.value env = "as"
        then begin
          Expect.contextual env "as";
          let name, _ = Parse.identifier_or_reserved_keyword env in
          (record_export env (fst name, extract_ident_name name));
          Some name, None, fst name
        end else begin
          let loc = fst local in
          record_export env (loc, extract_ident_name local);
          None, err, loc
        end in
        let loc = Loc.btwn (fst local) end_loc in
        let specifier = loc, {
          Statement.ExportNamedDeclaration.ExportSpecifier.local;
          exported;
        } in
        if Peek.token env = T_COMMA
        then Expect.token env T_COMMA;
        let errs = match err with
        | Some err -> err::errs
        | None -> errs in
        export_specifiers_and_errs env (specifier::specifiers) errs

  and export_declaration env decorators =
    let env = env |> with_strict true |> with_in_export true in
    let start_loc = Peek.loc env in
    Expect.token env T_EXPORT;
    match Peek.token env with
    | T_DEFAULT ->
        (* export default ... *)
        let open Statement.ExportDefaultDeclaration in
        Expect.token env T_DEFAULT;
        record_export env (Loc.btwn start_loc (Peek.loc env), "default");
        let end_loc, declaration = match Peek.token env with
        | T_FUNCTION ->
            (* export default function foo (...) { ... } *)
            let fn = Declaration._function env in
            fst fn, Declaration fn
        | _ when Peek.is_class env ->
            (* export default class foo { ... } *)
            let _class = Object.class_declaration env decorators in
            fst _class, Declaration _class
        | _ ->
            (* export default [assignment expression]; *)
            let expr = Parse.assignment env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst expr in
            Eat.semicolon env;
            end_loc, Expression expr
          in
        Loc.btwn start_loc end_loc, Statement.ExportDefaultDeclaration {
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
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> fst source in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
            declaration = None;
            specifiers = Some (ExportBatchSpecifier (specifier_loc, None));
            source = Some source;
            exportKind = Statement.ExportType;
          }
        | _ ->
          let type_alias = type_alias env in
          (match type_alias with
            | (loc, Statement.TypeAlias {Statement.TypeAlias.id; _;}) ->
              record_export env (loc, extract_ident_name id)
            | _ -> failwith (
                "Internal Flow Error! Parsed `export type` into something " ^
                "other than a type alias!"
              )
          );
          let end_loc = fst type_alias in
          Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
            declaration = Some type_alias;
            specifiers = None;
            source = None;
            exportKind = Statement.ExportType;
          }
        )
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
        let end_loc = fst interface in
        Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
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
        Loc.btwn start_loc (fst stmt), Statement.ExportNamedDeclaration {
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
          if Peek.value env = "as"
          then (
            Expect.contextual env "as";
            if parse_export_star_as
            then Some (Parse.identifier env)
            else (error env Error.UnexpectedTypeDeclaration; None)
          ) else None
        in
        let specifiers =
          Some (ExportBatchSpecifier (loc, local_name))
        in
        let source = export_source env in
        let end_loc = match Peek.semicolon_loc env with
        | Some loc -> loc
        | None -> fst source in
        let source = Some source in
        Eat.semicolon env;
        Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
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
        let specifiers, errs = export_specifiers_and_errs env [] [] in
        let specifiers = Some (ExportSpecifiers specifiers) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        let source = if Peek.value env = "from"
        then Some (export_source env)
        else begin
          errs |> List.iter (error_at env);
          None
        end in
        let end_loc = match Peek.semicolon_loc env with
        | Some loc -> loc
        | None ->
            (match source with
            | Some source -> fst source
            | None -> end_loc) in
        Eat.semicolon env;
        Loc.btwn start_loc end_loc, Statement.ExportNamedDeclaration {
          declaration = None;
          specifiers;
          source;
          exportKind;
        }

  and declare_export_declaration ?(allow_export_type=false) env =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeDeclaration;
    let start_loc = Peek.loc env in
    Expect.token env T_DECLARE;

    let env = env |> with_strict true |> with_in_export true in
    Expect.token env T_EXPORT;
    Statement.DeclareExportDeclaration.(match Peek.token env with
    | T_DEFAULT ->
        (* declare export default ... *)
        Expect.token env T_DEFAULT;
        let end_loc, declaration = match Peek.token env with
        | T_FUNCTION ->
            (* declare export default function foo (...): ...  *)
            let fn = declare_function env start_loc in
            fst fn, Some (Function fn)
        | T_CLASS ->
            (* declare export default class foo { ... } *)
            let _class = declare_class env start_loc in
            fst _class, Some (Class _class)
        | _ ->
            (* declare export default [type]; *)
            let _type = Type._type env in
            let end_loc = match Peek.semicolon_loc env with
            | Some loc -> loc
            | None -> fst _type in
            Eat.semicolon env;
            end_loc, Some (DefaultType _type)
          in
        Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
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
        let end_loc, declaration = match Peek.token env with
        | T_FUNCTION ->
            (* declare export function foo (...): ...  *)
            let fn = declare_function env start_loc in
            fst fn, Some (Function fn)
        | T_CLASS ->
            (* declare export class foo { ... } *)
            let _class = declare_class env start_loc in
            fst _class, Some (Class _class)
        | T_LET
        | T_CONST
        | T_VAR as token ->
            (match token with
            | T_LET -> error env Error.DeclareExportLet
            | T_CONST -> error env Error.DeclareExportConst
            | _ -> ());
            (* declare export var foo: ... *)
            let var = declare_var env start_loc in
            fst var, Some (Variable var)
        | _ -> assert false in
        Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
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
          if Peek.value env = "as"
          then (
            Expect.contextual env "as";
            if parse_export_star_as
            then Some (Parse.identifier env)
            else (error env Error.UnexpectedTypeDeclaration; None)
          ) else None
        in
        let specifiers = Statement.ExportNamedDeclaration.(
          Some (ExportBatchSpecifier (loc, local_name))
        ) in
        let source = export_source env in
        let end_loc = match Peek.semicolon_loc env with
        | Some loc -> loc
        | None -> fst source in
        let source = Some source in
        Eat.semicolon env;
        Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
          default = false;
          declaration = None;
          specifiers;
          source;
        }
    | T_TYPE when allow_export_type ->
        (* declare export type = ... *)
        let (alias_loc, alias) = type_alias_helper env in
        let loc = Loc.btwn start_loc alias_loc in
        (loc, Statement.DeclareExportDeclaration {
          default = false;
          declaration = Some (NamedType (alias_loc, alias));
          specifiers = None;
          source = None;
        })
    | T_INTERFACE when allow_export_type ->
        (* declare export interface ... *)
        let (iface_loc, iface) = interface_helper env in
        let loc = Loc.btwn start_loc iface_loc in
        (loc, Statement.DeclareExportDeclaration {
          default = false;
          declaration = Some (Interface (iface_loc, iface));
          specifiers = None;
          source = None;
        })
    | _ ->
        (match Peek.token env with
          | T_TYPE -> error env Error.DeclareExportType
          | T_INTERFACE -> error env Error.DeclareExportInterface
          | _ -> ()
        );
        Expect.token env T_LCURLY;
        let specifiers, errs = export_specifiers_and_errs env [] [] in
        let specifiers = Some (Statement.ExportNamedDeclaration.ExportSpecifiers specifiers) in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        let source = if Peek.value env = "from"
        then Some (export_source env)
        else begin
          errs |> List.iter (error_at env);
          None
        end in
        let end_loc = match Peek.semicolon_loc env with
        | Some loc -> loc
        | None ->
            (match source with
            | Some source -> fst source
            | None -> end_loc) in
        Eat.semicolon env;
        Loc.btwn start_loc end_loc, Statement.DeclareExportDeclaration {
          default = false;
          declaration = None;
          specifiers;
          source;
        }
    )

  and import_declaration =
    let open Statement.ImportDeclaration in

    let source env =
      Expect.contextual env "from";
      match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
          if octal then strict_error env Error.StrictOctalLiteral;
          Expect.token env (T_STRING (loc, value, raw, octal));
          let value = Literal.String value in
          loc, { Literal.value; raw; }
      | _ ->
          (* Just make up a string for the error case *)
          let raw = Peek.value env in
          let value = Literal.String raw in
          let ret = Peek.loc env, { Literal.value; raw; } in
          error_unexpected env;
          ret

    in let rec specifier_list ?(preceding_comma=true) env is_type_import acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev acc
      | _ ->
        if not preceding_comma then error_at env (
          Peek.loc env, Error.ImportSpecifierMissingComma
        );
        let remote, err = Parse.identifier_or_reserved_keyword env in
        let (starts_w_type, type_kind) =
          let v = snd remote in
          if v = "type" then true, Some ImportType
          else if v = "typeof" then true, Some ImportTypeof
          else false, None
        in
        let specifier =
          if Peek.value env = "as" then (
            let as_ident = Parse.identifier env in
            if starts_w_type && not (Peek.is_identifier env) then (
              (* `import {type as ,` or `import {type as }` *)
              (if is_type_import then error_at env (
                fst remote,
                Error.ImportTypeShorthandOnlyInPureImport
              ));
              ImportNamedSpecifier {
                local = None;
                remote = as_ident;
                kind = type_kind;
              }
            ) else (
              (* `import {type as foo` *)
              let local = Some (Parse.identifier env) in
              ImportNamedSpecifier {local; remote; kind = None; }
            )
          ) else if starts_w_type && Peek.is_identifier env then (
            (* `import {type foo` *)
            (if is_type_import then error_at env (
              fst remote,
              Error.ImportTypeShorthandOnlyInPureImport
            ));
            let remote, err = Parse.identifier_or_reserved_keyword env in
            (match err with Some err -> error_at env err | None -> ());
            let local =
              if Peek.value env = "as" then (
                Expect.contextual env "as";
                Some (Parse.identifier env)
              ) else None
            in
            ImportNamedSpecifier { local; remote; kind = type_kind; }
          ) else (
            (match err with Some err -> error_at env err | None -> ());
            ImportNamedSpecifier { local = None; remote; kind = None; }
          )
        in
        let preceding_comma = Expect.maybe env T_COMMA in
        specifier_list ~preceding_comma env is_type_import (specifier::acc)

    in let named_or_namespace_specifier env is_type_import =
      let start_loc = Peek.loc env in
      match Peek.token env with
      | T_MULT ->
          Expect.token env T_MULT;
          Expect.contextual env "as";
          let id = Parse.identifier env in
          [ImportNamespaceSpecifier (Loc.btwn start_loc (fst id), id)]
      | _ ->
          Expect.token env T_LCURLY;
          let specifiers = specifier_list env is_type_import [] in
          Expect.token env T_RCURLY;
          specifiers

    in fun env ->
      let env = env |> with_strict true in
      let start_loc = Peek.loc env in
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
        let end_loc = match Peek.semicolon_loc env with
        | Some loc -> loc
        | None -> str_loc in
        Eat.semicolon env;
        Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
          importKind;
          source;
          specifiers = [];
        }

      (* import [type] SomeDefault ... *)
      | T_COMMA, _ (* `import type, ...` *)
      | _, true -> (* `import type Foo` or `import type from` *)
          let importKind, default_specifier = (
            match type_ident, Peek.token env, Peek.value env with
            | Some type_ident, T_COMMA, _ (* `import type,` *)
            | Some type_ident, T_IDENTIFIER, "from" -> (* `import type from` *)
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
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> fst source in
          let source = source in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
            importKind;
            source;
            specifiers = default_specifier::additional_specifiers;
          }

      (* `import [type] { ... } ...` or `import [typeof] * as ...` *)
      | _ ->
          let specifiers = named_or_namespace_specifier env is_type_import in
          let source = source env in
          let end_loc = match Peek.semicolon_loc env with
          | Some loc -> loc
          | None -> fst source in
          let source = source in
          Eat.semicolon env;
          Loc.btwn start_loc end_loc, Statement.ImportDeclaration {
            importKind;
            source;
            specifiers;
          }
end
