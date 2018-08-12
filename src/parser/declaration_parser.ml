(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_common
open Parser_env
open Ast
module Error = Parse_error
module SSet = Set.Make(String)

module type DECLARATION = sig
  val async: env -> bool
  val generator: env -> bool
  val variance: env -> bool -> bool -> Loc.t Variance.t option
  val function_params: await:bool -> yield:bool -> env -> (Loc.t, Loc.t) Ast.Function.Params.t
  val function_body: env -> async:bool -> generator:bool -> Loc.t * (Loc.t, Loc.t) Function.body * bool
  val is_simple_function_params: (Loc.t, Loc.t) Ast.Function.Params.t -> bool
  val strict_post_check: env -> strict:bool -> simple:bool -> Loc.t Identifier.t option -> (Loc.t, Loc.t) Ast.Function.Params.t -> unit
  val concise_function_body: env -> async:bool -> generator:bool -> (Loc.t, Loc.t) Function.body * bool
  val variable: env -> (Loc.t, Loc.t) Statement.t * (Loc.t * Error.t) list
  val variable_declaration_list: env -> (Loc.t, Loc.t) Statement.VariableDeclaration.Declarator.t list * (Loc.t * Error.t) list
  val let_: env -> (Loc.t, Loc.t) Statement.VariableDeclaration.t * (Loc.t * Error.t) list
  val const: env -> (Loc.t, Loc.t) Statement.VariableDeclaration.t * (Loc.t * Error.t) list
  val var: env -> (Loc.t, Loc.t) Statement.VariableDeclaration.t * (Loc.t * Error.t) list
  val _function: env -> (Loc.t, Loc.t) Statement.t
end

module Declaration
  (Parse: Parser_common.PARSER)
  (Type: Type_parser.TYPE)
: DECLARATION
= struct
  let check_param =
    let rec pattern ((env, _) as check_env) (loc, p) = Pattern.(match p with
      | Object o -> _object check_env o
      | Array arr -> _array check_env arr
      | Assignment { Assignment.left; _ } -> pattern check_env left
      | Identifier id -> identifier_pattern check_env id
      | Expression _ -> (
          error_at env (loc, Error.ExpectedPatternFoundExpression);
          check_env
        )
    )

    and _object check_env o =
      List.fold_left
        object_property
        check_env
        o.Pattern.Object.properties

    and object_property check_env = Pattern.Object.(function
      | Property (_, property) -> Property.(
          let check_env = match property.key with
          | Identifier id -> identifier_no_dupe_check check_env id
          | _ -> check_env in
          pattern check_env property.pattern)
      | RestProperty (_, { RestProperty.argument; }) ->
          pattern check_env argument)

    and _array check_env arr =
      List.fold_left
      array_element
      check_env
      arr.Pattern.Array.elements

    and array_element check_env = Pattern.Array.(function
      | None -> check_env
      | Some (Element p) -> pattern check_env p
      | Some (RestElement (_, { RestElement.argument; })) ->
          pattern check_env argument)

    and identifier_pattern check_env {Pattern.Identifier.name=id; _;} =
      identifier check_env id

    and identifier (env, param_names) (loc, name as id) =
      if SSet.mem name param_names
      then error_at env (loc, Error.StrictParamDupe);
      let env, param_names =
        identifier_no_dupe_check (env, param_names) id in
      env, SSet.add name param_names

    and identifier_no_dupe_check (env, param_names) (loc, name) =
      if is_restricted name
      then strict_error_at env (loc, Error.StrictParamName);
      if is_future_reserved name || is_strict_reserved name
      then strict_error_at env (loc, Error.StrictReservedWord);
      env, param_names

    in pattern

  (* Strict is true if we were already in strict mode or if we are newly in
   * strict mode due to a directive in the function.
   * Simple is the IsSimpleParameterList thing from the ES6 spec *)
  let strict_post_check env ~strict ~simple id (_, { Ast.Function.Params.params; rest }) =
    if strict || not simple
    then
      (* If we are doing this check due to strict mode than there are two
       * cases to consider. The first is when we were already in strict mode
       * and therefore already threw strict errors. In this case we want to
       * do these checks outside of strict mode. The other is if we
       * originally parsed in non-strict mode but now are strict. Then we
       * want to do these checks in strict mode *)
      let env =
        if strict
        then env |> with_strict (not (Parser_env.in_strict_mode env))
        else env in
      (match id with
      | Some (loc, name) ->
          if is_restricted name
          then strict_error_at env (loc, Error.StrictFunctionName);
          if is_future_reserved name || is_strict_reserved name
          then strict_error_at env (loc, Error.StrictReservedWord)
      | None -> ());
      let acc = List.fold_left check_param (env, SSet.empty) params in
      match rest with
      | Some (_, { Function.RestElement.argument }) ->
        ignore (check_param acc argument)
      | None ->
        ()

  let function_params =
    let rec param env =
      let left = Parse.pattern env Error.StrictParamName in
      (* TODO: shouldn't Parse.pattern recognize Assignment patterns? *)
      if Peek.token env = T_ASSIGN
      then begin
        Expect.token env T_ASSIGN;
        let right = Parse.assignment env in
        let loc = Loc.btwn (fst left) (fst right) in
        (loc, Pattern.Assignment { Pattern.Assignment.left; right })
      end else
        left
    and param_list env acc =
      match Peek.token env with
      | T_EOF
      | T_RPAREN
      | T_ELLIPSIS as t ->
          let rest =
            if t = T_ELLIPSIS then begin
              let start_loc = Peek.loc env in
              Expect.token env T_ELLIPSIS;
              let id = Parse.pattern env Error.StrictParamName in
              let loc = Loc.btwn start_loc (fst id) in
              Some (loc, { Function.RestElement.argument = id; })
            end else
              None
          in
          if Peek.token env <> T_RPAREN
          then error env Error.ParameterAfterRestParameter;
          { Ast.Function.Params.params = List.rev acc; rest }
      | _ ->
          let the_param = param env in
          if Peek.token env <> T_RPAREN
          then Expect.token env T_COMMA;
          param_list env (the_param::acc)

    in fun ~await ~yield -> with_loc (fun env ->
      let env = env
        |> with_allow_await await
        |> with_allow_yield yield
        |> with_in_formal_parameters true
      in
      Expect.token env T_LPAREN;
      let params = param_list env [] in
      Expect.token env T_RPAREN;
      params
    )

  let function_body env ~async ~generator =
    let env = enter_function env ~async ~generator in
    let loc, block, strict = Parse.function_block_body env in
    loc, Function.BodyBlock (loc, block), strict

  let concise_function_body env ~async ~generator =
    let env = env |> with_in_function true in
    match Peek.token env with
    | T_LCURLY ->
        let _, body, strict = function_body env ~async ~generator in
        body, strict
    | _ ->
        let env = enter_function env ~async ~generator in
        let expr = Parse.assignment env in
        Function.BodyExpression expr, in_strict_mode env

  let variance env is_async is_generator =
    let loc = Peek.loc env in
    let variance = match Peek.token env with
    | T_PLUS ->
        Eat.token env;
        Some (loc, Variance.Plus)
    | T_MINUS ->
        Eat.token env;
        Some (loc, Variance.Minus)
    | _ ->
        None
    in
    match variance with
    | Some (loc, _) when is_async || is_generator ->
        error_at env (loc, Error.UnexpectedVariance);
        None
    | _ ->
        variance

  let generator env = Expect.maybe env T_MULT

  let async env = Expect.maybe env T_ASYNC

  let is_simple_function_params =
    let is_simple_param = function
    | _, Pattern.Identifier _ ->  true
    | _ -> false

    in fun (_, { Ast.Function.Params.params; rest }) ->
      rest = None && List.for_all is_simple_param params

  let _function env =
    let start_loc = Peek.loc env in
    let async = async env in
    Expect.token env T_FUNCTION;
    let generator = generator env in
    let (tparams, id) = (
      match in_export env, Peek.token env with
      | true, T_LPAREN -> (None, None)
      | true, T_LESS_THAN ->
        let typeParams = Type.type_parameter_declaration env in
        let id = if Peek.token env = T_LPAREN then None else Some (
          Parse.identifier ~restricted_error:Error.StrictFunctionName env
        ) in
        (typeParams, id)
      | _ ->
        let id =
          Parse.identifier ~restricted_error:Error.StrictFunctionName env
        in
        (Type.type_parameter_declaration env, Some id)
    ) in
    let params =
      let yield, await = match async, generator with
      | true, true -> true, true (* proposal-async-iteration/#prod-AsyncGeneratorDeclaration *)
      | true, false -> false, allow_await env (* #prod-AsyncFunctionDeclaration *)
      | false, true -> true, false (* #prod-GeneratorDeclaration *)
      | false, false -> false, false (* #prod-FunctionDeclaration *)
      in
      function_params ~await ~yield env
    in
    let (return, predicate) = Type.annotation_and_predicate_opt env in
    let _, body, strict = function_body env ~async ~generator in
    let simple = is_simple_function_params params in
    strict_post_check env ~strict ~simple id params;
    let end_loc, expression = Ast.Function.(
      match body with
      | BodyBlock (loc, _) -> loc, false
      | BodyExpression (loc, _) -> loc, true) in
    Loc.btwn start_loc end_loc, Statement.(FunctionDeclaration Function.({
      id;
      params;
      body;
      generator;
      async;
      predicate;
      expression;
      return;
      tparams;
    }))

  let variable_declaration_list =
    let variable_declaration env =
      let loc, (decl, errs) = with_loc (fun env ->
        let id = Parse.pattern env Error.StrictVarName in
        let init, errs = if Peek.token env = T_ASSIGN
        then begin
          Expect.token env T_ASSIGN;
          Some (Parse.assignment env), []
        end else Ast.Pattern.(
          match id with
          | _, Identifier _ -> None, []
          | loc, _ -> None, [(loc, Error.NoUninitializedDestructuring)]
        ) in
        Ast.Statement.VariableDeclaration.Declarator.({
          id;
          init;
        }), errs
      ) env in
      (loc, decl), errs

    in let rec helper env decls errs =
      let decl, errs_ = variable_declaration env in
      let decls = decl::decls in
      let errs = errs_ @ errs in
      if Peek.token env = T_COMMA
      then begin
        Expect.token env T_COMMA;
        helper env decls errs
      end else
        List.rev decls, List.rev errs

    in fun env -> helper env [] []

  let declarations token kind env =
    Expect.token env token;
    let declarations, errs = variable_declaration_list env in
    Statement.VariableDeclaration.({
      kind;
      declarations;
    }), errs

  let var = declarations T_VAR Statement.VariableDeclaration.Var

  let const env =
    let env = env |> with_no_let true in
    let variable, errs =
      declarations T_CONST Statement.VariableDeclaration.Const env in
    (* Make sure all consts defined are initialized *)
    let errs = Statement.VariableDeclaration.(
      List.fold_left (fun errs decl ->
        match decl with
        | loc, { Declarator.init = None; _ } ->
            (loc, Error.NoUninitializedConst)::errs
        | _ -> errs
      ) errs variable.declarations
    ) in
    variable, List.rev errs

  let let_ env =
    let env = env |> with_no_let true in
    declarations T_LET Statement.VariableDeclaration.Let env

  let variable env =
    let loc, (decl, errs) = with_loc (fun env ->
      let variable, errs = match Peek.token env with
      | T_CONST -> const env
      | T_LET   -> let_ env
      | T_VAR   -> var env
      | _ ->
          error_unexpected env;
          (* We need to return something. This is as good as anything else *)
          var env in
      Statement.VariableDeclaration variable, errs
    ) env in
    (loc, decl), errs
end
