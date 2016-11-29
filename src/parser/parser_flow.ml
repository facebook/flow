(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Token = Lexer_flow.Token
open Token
open Parser_env
module Ast = Spider_monkey_ast
open Ast
module Error = Parse_error
module SSet = Set.Make(String)
open Parser_common

(* Sometimes we add the same error for multiple different reasons. This is hard
   to avoid, so instead we just filter the duplicates out. This function takes
   a reversed list of errors and returns the list in forward order with dupes
   removed. This differs from a set because the original order is preserved. *)
let filter_duplicate_errors =
  let module ErrorSet = Set.Make(struct
    type t = Loc.t * Error.t
    let compare (a_loc, a_error) (b_loc, b_error) =
      let loc = Loc.compare a_loc b_loc in
      if loc = 0
      then Pervasives.compare a_error b_error
      else loc
  end) in
  fun errs ->
    let errs = List.rev errs in
    let _, deduped = List.fold_left (fun (set, deduped) err ->
      if ErrorSet.mem err set then (set, deduped)
      else (ErrorSet.add err set, err::deduped)
    ) (ErrorSet.empty, []) errs in
    List.rev deduped

module rec Parse : PARSER = struct
  module Type = Type_parser.Type (Parse)
  module Declaration = Declaration_parser.Declaration (Parse) (Type)
  module Expression = Expression_parser.Expression (Parse) (Type) (Declaration)
  module Object = Object_parser.Object (Parse) (Type) (Declaration) (Expression)

  module Statement: sig
    val _for: env -> Ast.Statement.t
    val _if: env -> Ast.Statement.t
    val _let: env -> Ast.Statement.t
    val _try: env -> Ast.Statement.t
    val _while: env -> Ast.Statement.t
    val _with: env -> Ast.Statement.t
    val block: env -> Ast.Statement.t
    val break: env -> Ast.Statement.t
    val continue: env -> Ast.Statement.t
    val debugger: env -> Ast.Statement.t
    val declare: ?in_module:bool -> env -> Ast.Statement.t
    val declare_export_declaration: ?allow_export_type:bool -> env -> Ast.Statement.t
    val do_while: env -> Ast.Statement.t
    val empty: env -> Ast.Statement.t
    val export_declaration: env -> Ast.Expression.t list -> Ast.Statement.t
    val expression: env -> Ast.Statement.t
    val import_declaration: env -> Ast.Statement.t
    val interface: env -> Ast.Statement.t
    val maybe_labeled: env -> Ast.Statement.t
    val return: env -> Ast.Statement.t
    val switch: env -> Ast.Statement.t
    val throw: env -> Ast.Statement.t
    val type_alias: env -> Ast.Statement.t
    val var_or_const: env -> Ast.Statement.t
  end = struct
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
          }))

    and expression env =
      let loc, expression = with_loc Parse.expression env in
      let loc = match Peek.semicolon_loc env with
      | Some semicolon_loc -> Loc.btwn loc semicolon_loc
      | None -> loc in
      Eat.semicolon env;
      loc, Statement.(Expression Expression.({
        expression;
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
      let predicate = Type.predicate_opt env in
      let loc = Loc.btwn start_sig_loc end_loc in
      let typeAnnotation = loc, Ast.Type.(Function {Function.
        params;
        returnType;
        typeParameters;
      }) in
      let typeAnnotation = fst typeAnnotation, typeAnnotation in
      let id = Loc.btwn (fst id) end_loc, snd id in
      let end_loc = match Peek.semicolon_loc env with
      | None -> end_loc
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
        let body_start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let (module_kind, body) = module_items env ~module_kind:None [] in
        Expect.token env T_RCURLY;
        let body_end_loc = Peek.loc env in
        let body_loc = Loc.btwn body_start_loc body_end_loc in
        let body = body_loc, { Statement.Block.body; } in
        let loc = Loc.btwn start_loc (fst body) in
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

      in let rec specifier_list env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
            let remote, err = Parse.identifier_or_reserved_keyword env in
            let specifier =
              if Peek.value env = "as" then begin
                Expect.contextual env "as";
                let local = Some (Parse.identifier env) in
                ImportNamedSpecifier { local; remote; }
              end else begin
                (match err with Some err -> error_at env err | None -> ());
                ImportNamedSpecifier { local = None; remote; }
              end
            in
            if Peek.token env = T_COMMA
            then Expect.token env T_COMMA;
            specifier_list env (specifier::acc)

      in let named_or_namespace_specifier env =
        let start_loc = Peek.loc env in
        match Peek.token env with
        | T_MULT ->
            Expect.token env T_MULT;
            Expect.contextual env "as";
            let id = Parse.identifier env in
            [ImportNamespaceSpecifier (Loc.btwn start_loc (fst id), id)]
        | _ ->
            Expect.token env T_LCURLY;
            let specifiers = specifier_list env [] in
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
                  named_or_namespace_specifier env
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
            let specifiers = named_or_namespace_specifier env in
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

  module Pattern = struct
    (* Reinterpret various expressions as patterns.
     * This is not the correct thing to do and is only used for assignment
     * expressions. This should be removed and replaced ASAP.
     *)
    let object_from_expr =
      let property env prop =
        Ast.Expression.Object.(match prop with
        | Property (loc, { Property.key; value; shorthand; _ }) ->
          let key = Property.(match key with
          | Literal lit -> Pattern.Object.Property.Literal lit
          | Identifier id -> Pattern.Object.Property.Identifier id
          | Computed expr -> Pattern.Object.Property.Computed expr) in
          let pattern = Parse.pattern_from_expr env value in
          Pattern.(Object.Property (loc, Object.Property.({
            key;
            pattern;
            shorthand;
          })))
        | SpreadProperty (loc, { SpreadProperty.argument; }) ->
            let argument = Parse.pattern_from_expr env argument in
            Pattern.Object.(RestProperty (loc, RestProperty.({
              argument;
            }))))

      in fun env (loc, obj) ->
        let properties =
          List.map (property env) obj.Ast.Expression.Object.properties in
        loc, Pattern.(Object Object.({
          properties;
          typeAnnotation = None;
        }))

    let array_from_expr =
      let element env = Ast.Expression.(function
        | None -> None
        | Some (Spread (loc, spread)) ->
            let argument = Parse.pattern_from_expr env (spread.SpreadElement.argument) in
            Some Pattern.Array.(RestElement (loc, { RestElement.argument; }))
        | Some (Expression (loc, expr)) ->
            Some Pattern.Array.(Element (Parse.pattern_from_expr env (loc, expr)))
      )

      in fun env (loc, arr) ->
        let elements =
          List.map (element env) arr.Ast.Expression.Array.elements in
        loc, Pattern.(Array Array.({
          elements;
          typeAnnotation = None;
        }))

    let from_expr env (loc, expr) =
      Ast.Expression.(match expr with
      | Object obj -> object_from_expr env (loc, obj)
      | Array arr ->  array_from_expr env (loc, arr)
      | Identifier name -> loc, Pattern.Identifier {
          Pattern.Identifier.name;
                             typeAnnotation=None;
                             optional=false;
      }
      | Assignment { Assignment.operator = Assignment.Assign; left; right } ->
          loc, Pattern.Assignment { Pattern.Assignment.left; right }
      | expr -> loc, Pattern.Expression (loc, expr))

    (* Parse object destructuring pattern *)
    let rec _object restricted_error =
      let rec property env =
        let start_loc = Peek.loc env in
        if Expect.maybe env T_ELLIPSIS
        then begin
          let argument = pattern env restricted_error in
          let loc = Loc.btwn start_loc (fst argument) in
          Some Pattern.Object.(RestProperty (loc, RestProperty.({
            argument
          })))
        end else begin
          let key = Ast.Expression.Object.Property.(
            match Parse.object_key env with
            | _, Literal lit -> Pattern.Object.Property.Literal lit
            | _, Identifier id -> Pattern.Object.Property.Identifier id
            | _, Computed expr -> Pattern.Object.Property.Computed expr
          ) in
          let prop = match Peek.token env with
            | T_COLON ->
              Expect.token env T_COLON;
              Some (pattern env restricted_error, false)
            | _ ->
              (match key with
              | Pattern.Object.Property.Identifier name ->
                let pattern = (fst name, Pattern.Identifier {
                  Pattern.Identifier.name;
                                     typeAnnotation=None;
                                     optional=false;
                }) in
                Some (pattern, true)
              | _ ->
                error_unexpected env; (* invalid shorthand destructuring *)
                None)
          in
          match prop with
          | Some (pattern, shorthand) ->
            let pattern = match Peek.token env with
              | T_ASSIGN ->
                Expect.token env T_ASSIGN;
                let default = Parse.assignment env in
                let loc = Loc.btwn (fst pattern) (fst default) in
                loc, Pattern.(Assignment Assignment.({
                  left = pattern;
                  right = default;
                }));
              | _ -> pattern
            in
            let loc = Loc.btwn start_loc (fst pattern) in
            Some Pattern.Object.(Property (loc, Property.({
              key;
              pattern;
              shorthand;
            })))
          | None -> None
        end

      and properties env acc =
        match Peek.token env with
        | T_EOF
        | T_RCURLY -> List.rev acc
        | _ ->
          (match property env with
          | Some prop ->
            if Peek.token env <> T_RCURLY
            then Expect.token env T_COMMA;
            properties env (prop::acc)
          | None -> properties env acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LCURLY;
        let properties = properties env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RCURLY;
        let end_loc, typeAnnotation =
          if Peek.token env = T_COLON then
            let typeAnnotation = Type.annotation env in
            fst typeAnnotation, Some typeAnnotation
          else end_loc, None
        in
        Loc.btwn start_loc end_loc, Pattern.(Object Object.({
          properties;
          typeAnnotation;
        }))

    (* Parse array destructuring pattern *)
    and _array restricted_error =
      let rec elements env acc =
        match Peek.token env with
        | T_EOF
        | T_RBRACKET -> List.rev acc
        | T_COMMA ->
          Expect.token env T_COMMA;
          elements env (None::acc)
        | T_ELLIPSIS ->
          let start_loc = Peek.loc env in
          Expect.token env T_ELLIPSIS;
          let argument = pattern env restricted_error in
          let loc = Loc.btwn start_loc (fst argument) in
          let element = Pattern.Array.(RestElement (loc, RestElement.({
            argument;
          }))) in
          elements env ((Some element)::acc)
        | _ ->
          let pattern = pattern env restricted_error in
          let pattern = match Peek.token env with
            | T_ASSIGN ->
              Expect.token env T_ASSIGN;
              let default = Parse.expression env in
              let loc = Loc.btwn (fst pattern) (fst default) in
              loc, Pattern.(Assignment Assignment.({
                left = pattern;
                right = default;
              }))
            | _ -> pattern
          in
          let element = Pattern.Array.(Element pattern) in
          if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
          elements env ((Some element)::acc)

      in fun env ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let elements = elements env [] in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        let end_loc, typeAnnotation =
          if Peek.token env = T_COLON then
            let typeAnnotation = Type.annotation env in
            fst typeAnnotation, Some typeAnnotation
          else end_loc, None
        in
        Loc.btwn start_loc end_loc, Pattern.(Array Array.({
          elements;
          typeAnnotation;
        }))

    and pattern env restricted_error =
      match Peek.token env with
      | T_LCURLY ->
          _object restricted_error env
      | T_LBRACKET ->
          _array restricted_error env
      | _ ->
          let loc, id = Parse.identifier_with_type env restricted_error in
          loc, Pattern.Identifier id
  end

  module JSX = struct
    let spread_attribute env =
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      Expect.token env T_ELLIPSIS;
      let argument = Expression.assignment env in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, JSX.SpreadAttribute.({
        argument;
      })

    let expression_container env =
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      let expression = if Peek.token env = T_RCURLY
        then
          let empty_loc = Loc.btwn_exclusive start_loc (Peek.loc env) in
          JSX.ExpressionContainer.EmptyExpression empty_loc
        else JSX.ExpressionContainer.Expression (Parse.expression env) in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Eat.pop_lex_mode env;
      Loc.btwn start_loc end_loc, JSX.ExpressionContainer.({
        expression;
      })

    let identifier env =
      let loc = Peek.loc env in
      let name = Peek.value env in
      Expect.token env T_JSX_IDENTIFIER;
      loc, JSX.Identifier.({ name; })

    let name =
      let rec member_expression env member =
        match Peek.token env with
        | T_PERIOD ->
            let _object = JSX.MemberExpression.MemberExpression member in
            Expect.token env T_PERIOD;
            let property = identifier env in
            let loc = Loc.btwn (fst member) (fst property) in
            let member = loc, JSX.MemberExpression.({
              _object;
              property;
            }) in
            member_expression env member
        | _ -> member

      in fun env ->
        let name = identifier env in
        match Peek.token env with
        | T_COLON ->
            let namespace = name in
            Expect.token env T_COLON;
            let name = identifier env in
            let loc = Loc.btwn (fst namespace) (fst name) in
            JSX.NamespacedName (loc, JSX.NamespacedName.({
              namespace;
              name;
            }))
        | T_PERIOD ->
            let _object = JSX.MemberExpression.Identifier name in
            Expect.token env T_PERIOD;
            let property = identifier env in
            let loc = Loc.btwn (fst name) (fst property) in
            let member = loc, JSX.MemberExpression.({
              _object;
              property;
            }) in
            JSX.MemberExpression (member_expression env member)
        | _ -> JSX.Identifier name


    let attribute env =
      let start_loc = Peek.loc env in
      let name = identifier env in
      let end_loc, name =
        if Peek.token env = T_COLON
        then begin
          Expect.token env T_COLON;
          let namespace = name in
          let name = identifier env in
          let loc = Loc.btwn (fst namespace) (fst name) in
          loc, JSX.Attribute.NamespacedName (loc, JSX.NamespacedName.({
            namespace;
            name;
          }))
        end else fst name, JSX.Attribute.Identifier name in
      let end_loc, value =
        if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          match Peek.token env with
          | T_LCURLY ->
              let loc, expression_container = expression_container env in
              begin
                let open JSX.ExpressionContainer in
                match expression_container.expression with
                | EmptyExpression _ ->
                    error_at env (loc, Error.JSXAttributeValueEmptyExpression);
                | _ -> ()
              end;
              loc, Some (JSX.Attribute.ExpressionContainer (loc, expression_container))
          | T_JSX_TEXT (loc, value, raw) as token ->
              Expect.token env token;
              let value = Ast.Literal.String value in
              loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
          | _ ->
              error env Error.InvalidJSXAttributeValue;
              let loc = Peek.loc env in
              let raw = "" in
              let value = Ast.Literal.String "" in
              loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
        end else end_loc, None in
      Loc.btwn start_loc end_loc, JSX.Attribute.({
        name;
        value;
      })

      let opening_element_without_lt =
        let rec attributes env acc =
          match Peek.token env with
          | T_EOF
          | T_DIV
          | T_GREATER_THAN -> List.rev acc
          | T_LCURLY ->
              let attribute = JSX.Opening.SpreadAttribute (spread_attribute env) in
              attributes env (attribute::acc)
          | _ ->
              let attribute = JSX.Opening.Attribute (attribute env) in
              attributes env (attribute::acc)

        in fun env start_loc ->
          let name = name env in
          let attributes = attributes env [] in
          let selfClosing = Peek.token env = T_DIV in
          if selfClosing then Expect.token env T_DIV;
          let end_loc = Peek.loc env in
          Expect.token env T_GREATER_THAN;
          Eat.pop_lex_mode env;
          Loc.btwn start_loc end_loc, JSX.Opening.({
            name;
            selfClosing;
            attributes;
          })

      let closing_element_without_lt env start_loc =
        Expect.token env T_DIV;
        let name = name env in
        let end_loc = Peek.loc env in
        Expect.token env T_GREATER_THAN;
        (* We double pop to avoid going back to childmode and re-lexing the
         * lookahead *)
        Eat.double_pop_lex_mode env;
        Loc.btwn start_loc end_loc, JSX.Closing.({
          name;
        })

      type element_or_closing =
        | Closing of JSX.Closing.t
        | ChildElement of (Loc.t * JSX.element)


      let rec child env =
        match Peek.token env with
        | T_LCURLY ->
            let expression_container = expression_container env in
            fst expression_container, JSX.ExpressionContainer (snd expression_container)
        | T_JSX_TEXT (loc, value, raw) as token ->
            Expect.token env token;
            loc, JSX.Text { JSX.Text.value; raw; }
        | _ ->
            let element = element env in
            fst element, JSX.Element (snd element)

      and element_without_lt =
        let element_or_closing env =
          Eat.push_lex_mode env Lex_mode.JSX_TAG;
          let start_loc = Peek.loc env in
          Expect.token env T_LESS_THAN;
          match Peek.token env with
          | T_EOF
          | T_DIV -> Closing (closing_element_without_lt env start_loc)
          | _ -> ChildElement (element_without_lt env start_loc)

        in let rec children_and_closing env acc =
          match Peek.token env with
          | T_LESS_THAN -> (
              match element_or_closing env with
              | Closing closingElement ->
                  List.rev acc, Some closingElement
              | ChildElement element ->
                  let element = fst element, JSX.Element (snd element) in
                  children_and_closing env (element::acc))
          | T_EOF ->
              error_unexpected env;
              List.rev acc, None
          | _ ->
              children_and_closing env ((child env)::acc)

        in let rec normalize name = JSX.(match name with
          | Identifier (_, { Identifier.name }) -> name
          | NamespacedName (_, { NamespacedName.namespace; name; }) ->
              (snd namespace).Identifier.name ^ ":" ^ (snd name).Identifier.name
          | MemberExpression (_, { MemberExpression._object; property; }) ->
              let _object = match _object with
              | MemberExpression.Identifier (_, {Identifier.name=id; _;}) -> id
              | MemberExpression.MemberExpression e ->
                  normalize (JSX.MemberExpression e) in
                  _object ^ "." ^ (snd property).Identifier.name
        )

        in fun env start_loc ->
          let openingElement = opening_element_without_lt env start_loc in
          let children, closingElement =
            if (snd openingElement).JSX.Opening.selfClosing
            then [], None
            else begin
              Eat.push_lex_mode env Lex_mode.JSX_CHILD;
              let ret = children_and_closing env [] in
              ret
            end in
          let end_loc = match closingElement with
          | Some (loc, { JSX.Closing.name }) ->
              let opening_name = normalize (snd openingElement).JSX.Opening.name in
              if normalize name <> opening_name
              then error env (Error.ExpectedJSXClosingTag opening_name);
              loc
          | _ -> fst openingElement in
          Loc.btwn (fst openingElement) end_loc, JSX.({
            openingElement;
            closingElement;
            children;
          })

      and element env =
        let start_loc = Peek.loc env in
        Eat.push_lex_mode env Lex_mode.JSX_TAG;
        Expect.token env T_LESS_THAN;
        element_without_lt env start_loc
  end

  let rec program env =
    let stmts = module_body_with_directives env (fun _ -> false) in
    let end_loc = Peek.loc env in
    Expect.token env T_EOF;
    let loc = match stmts with
    | [] -> end_loc
    | _ -> Loc.btwn (fst (List.hd stmts)) (fst (List.hd (List.rev stmts))) in
    let comments = List.rev (comments env) in
    loc, stmts, comments

  and directives =
      let check env (loc, token) =
        match token with
        | T_STRING (_, _, _, octal) ->
            if octal then strict_error_at env (loc, Error.StrictOctalLiteral)
        | _ -> failwith ("Nooo: "^(token_to_string token)^"\n")

      in let rec statement_list env term_fn item_fn (string_tokens, stmts) =
        match Peek.token env with
        | T_EOF -> env, string_tokens, stmts
        | t when term_fn t -> env, string_tokens, stmts
        | _ ->
            let string_token = Peek.loc env, Peek.token env in
            let possible_directive = item_fn env in
            let stmts = possible_directive::stmts in
            (match possible_directive with
            | _, Ast.Statement.Expression {
                Ast.Statement.Expression.expression = loc, Ast.Expression.Literal {
                  Ast.Literal.value = Ast.Literal.String str;
                  _;
                }
              } ->
                (* 14.1.1 says that it has to be "use strict" without any
                  * escapes, so "use\x20strict" is disallowed. We could in theory
                  * keep the raw string around, but that's a pain. This is a hack
                  * that actually seems to work pretty well (make sure the string
                  * has the right length)
                  *)
                let len = Loc.(loc._end.column - loc.start.column) in
                let strict =
                  (in_strict_mode env) ||
                  (str = "use strict" && len = 12)
                in
                let string_tokens = string_token::string_tokens in
                statement_list
                  (env |> with_strict strict)
                  term_fn
                  item_fn
                  (string_tokens, stmts)
            | _ ->
                env, string_tokens, stmts)

      in fun env term_fn item_fn ->
        let env, string_tokens, stmts = statement_list env term_fn item_fn ([], []) in
        List.iter (check env) (List.rev string_tokens);
        env, stmts

  (* 15.2 *)
  and module_item env =
    let decorators = Object.decorator_list env in
    match Peek.token env with
    | T_EXPORT -> Statement.export_declaration env decorators
    | T_IMPORT ->
        error_on_decorators env decorators;
        Statement.import_declaration env
    | T_DECLARE when Peek.token ~i:1 env = T_EXPORT ->
        error_on_decorators env decorators;
        Statement.declare_export_declaration env
    | _ -> statement_list_item env ~decorators

  and module_body_with_directives env term_fn =
    let env, directives = directives env term_fn module_item in
    let stmts = module_body ~term_fn env in
    (* Prepend the directives *)
    List.fold_left (fun acc stmt -> stmt::acc) stmts directives

  and module_body =
    let rec module_item_list env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> module_item_list env term_fn (module_item env::acc)

    in fun ~term_fn env ->
      module_item_list env term_fn []

  and statement_list_with_directives ~term_fn env =
    let env, directives = directives env term_fn statement_list_item in
    let stmts = statement_list ~term_fn env in
    (* Prepend the directives *)
    let stmts = List.fold_left (fun acc stmt -> stmt::acc) stmts directives in
    stmts, (in_strict_mode env)

  and statement_list =
    let rec statements env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> statements env term_fn ((statement_list_item env)::acc)

    in fun ~term_fn env -> statements env term_fn []


  and statement_list_item ?(decorators=[]) env =
    if not (Peek.is_class env)
    then error_on_decorators env decorators;
    Statement.(match Peek.token env with
    (* Remember kids, these look like statements but they're not
      * statements... (see section 13) *)
    | T_LET -> _let env
    | T_CONST -> var_or_const env
    | _ when Peek.is_function env -> Declaration._function env
    | _ when Peek.is_class env -> class_declaration env decorators
    | T_INTERFACE -> interface env
    | T_DECLARE -> declare env
    | T_TYPE -> type_alias env
    | _ -> statement env)

  and statement env =
    Statement.(match Peek.token env with
    | T_EOF ->
        error_unexpected env;
        Peek.loc env, Ast.Statement.Empty
    | T_SEMICOLON -> empty env
    | T_LCURLY -> block env
    | T_VAR -> var_or_const env
    | T_BREAK -> break env
    | T_CONTINUE -> continue env
    | T_DEBUGGER -> debugger env
    | T_DO -> do_while env
    | T_FOR -> _for env
    | T_IF -> _if env
    | T_RETURN -> return env
    | T_SWITCH -> switch env
    | T_THROW -> throw env
    | T_TRY -> _try env
    | T_WHILE -> _while env
    | T_WITH -> _with env
    | _ when Peek.is_identifier env -> maybe_labeled env
    (* If we see an else then it's definitely an error, but we can probably
     * assume that this is a malformed if statement that is missing the if *)
    | T_ELSE -> _if env
    (* There are a bunch of tokens that aren't the start of any valid
     * statement. We list them here in order to skip over them, rather than
     * getting stuck *)
    | T_COLON
    | T_RPAREN
    | T_RCURLY
    | T_RBRACKET
    | T_COMMA
    | T_PERIOD
    | T_ARROW
    | T_IN
    | T_INSTANCEOF
    | T_CATCH
    | T_FINALLY
    | T_CASE
    | T_DEFAULT
    | T_EXTENDS
    | T_STATIC
    | T_IMPORT (* TODO *)
    | T_EXPORT (* TODO *)
    | T_ELLIPSIS ->
        error_unexpected env;
        Eat.token env;
        statement env
    | _ -> expression env)

  and expression env =
    let expr = Expression.assignment env in
    match Peek.token env with
    | T_COMMA -> Expression.sequence env [expr]
    | _ ->
        expr

  and conditional = Expression.conditional
  and assignment = Expression.assignment
  and left_hand_side = Expression.left_hand_side
  and object_initializer = Object._initializer
  and object_key = Object.key
  and class_declaration = Object.class_declaration
  and class_expression = Object.class_expression
  and array_initializer = Expression.array_initializer

  and is_assignable_lhs = Expression.is_assignable_lhs

  and identifier ?restricted_error env =
    let loc = Peek.loc env in
    let name = Peek.value env in
    (match Peek.token env with
    | T_LET ->
    (* So "let" is disallowed as an identifier in a few situations. 11.6.2.1
     * lists them out. It is always disallowed in strict mode *)
      if in_strict_mode env
      then strict_error env Error.StrictReservedWord
      else
        if no_let env
        then error env (Error.UnexpectedToken name);
      Eat.token env
    | _ when is_strict_reserved name ->
      strict_error env Error.StrictReservedWord;
      Eat.token env
    | T_DECLARE
    | T_OF
    | T_ASYNC
    | T_AWAIT
    | T_TYPE as t ->
        (* These aren't real identifiers *)
        Expect.token env t
    | _ -> Expect.token env T_IDENTIFIER);
    (match restricted_error with
    | Some err when is_restricted name -> strict_error_at env (loc, err)
    | _ -> ());
    loc, name

  and identifier_or_reserved_keyword = Expression.identifier_or_reserved_keyword

  and identifier_with_type =
    let with_loc_helper no_optional restricted_error env =
      let name = identifier ~restricted_error env in
      let optional = not no_optional && Peek.token env = T_PLING in
      if optional then begin
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeAnnotation;
        Expect.token env T_PLING
      end;
      let typeAnnotation =
        if Peek.token env = T_COLON
        then Some (Type.annotation env)
        else None in
      Ast.Pattern.Identifier.({
        name;
        optional;
        typeAnnotation;
      })

    in fun env ?(no_optional=false) restricted_error ->
      with_loc (with_loc_helper no_optional restricted_error) env

  and block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body = statement_list ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }

  and function_block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body, strict = statement_list_with_directives ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }, strict

  and jsx_element = JSX.element

  and pattern = Pattern.pattern
  and pattern_from_expr = Pattern.from_expr
end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let do_parse env parser fail =
  let ast = parser env in
  let error_list = filter_duplicate_errors (errors env) in
  if fail && error_list <> []
  then raise (Error.Error error_list);
  ast, error_list

let parse_program fail ?(token_sink=None) ?(parse_options=None) filename content =
  let env = init_env ~token_sink ~parse_options filename content in
  do_parse env Parse.program fail

let program ?(fail=true) ?(token_sink=None) ?(parse_options=None) content =
  parse_program fail ~token_sink ~parse_options None content

let program_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  parse_program fail ~token_sink ~parse_options filename content

(* even if fail=false, still raises an error on a totally invalid token, since
   there's no legitimate fallback. *)
let json_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  let env = init_env ~token_sink ~parse_options filename content in
  match Peek.token env with
  | T_LBRACKET
  | T_LCURLY
  | T_STRING _
  | T_NUMBER _
  | T_TRUE
  | T_FALSE
  | T_NULL ->
    do_parse env Parse.expression fail
  | T_MINUS ->
    (match Peek.token ~i:1 env with
    | T_NUMBER _ ->
      do_parse env Parse.expression fail
    | _ ->
      error_unexpected env;
      raise (Error.Error (errors env)))
  | _ ->
    error_unexpected env;
    raise (Error.Error (errors env))

let jsx_pragma_expression =
  let left_hand_side env =
    let ast = Parse.left_hand_side (with_no_new true env) in
    Expect.token env T_EOF;
    ast

  in fun content filename ->
    let env = init_env ~token_sink:None ~parse_options:None filename content in
    do_parse env left_hand_side true
