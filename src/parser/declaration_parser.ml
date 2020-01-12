(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Token
open Parser_common
open Parser_env
open Flow_ast
module SSet = Set.Make (String)

module type DECLARATION = sig
  val async : env -> bool

  val generator : env -> bool

  val variance : env -> bool -> bool -> Loc.t Variance.t option

  val function_params : await:bool -> yield:bool -> env -> (Loc.t, Loc.t) Ast.Function.Params.t

  val function_body : env -> async:bool -> generator:bool -> (Loc.t, Loc.t) Function.body * bool

  val is_simple_function_params : (Loc.t, Loc.t) Ast.Function.Params.t -> bool

  val strict_post_check :
    env ->
    strict:bool ->
    simple:bool ->
    (Loc.t, Loc.t) Identifier.t option ->
    (Loc.t, Loc.t) Ast.Function.Params.t ->
    unit

  val let_ : env -> (Loc.t, Loc.t) Statement.VariableDeclaration.t * (Loc.t * Parse_error.t) list

  val const : env -> (Loc.t, Loc.t) Statement.VariableDeclaration.t * (Loc.t * Parse_error.t) list

  val var : env -> (Loc.t, Loc.t) Statement.VariableDeclaration.t * (Loc.t * Parse_error.t) list

  val _function : env -> (Loc.t, Loc.t) Statement.t

  val enum_declaration : env -> (Loc.t, Loc.t) Statement.t
end

module Declaration (Parse : Parser_common.PARSER) (Type : Type_parser.TYPE) : DECLARATION = struct
  module Enum = Enum_parser.Enum (Parse)

  let check_param =
    let rec pattern ((env, _) as check_env) (loc, p) =
      Pattern.(
        match p with
        | Object o -> _object check_env o
        | Array arr -> _array check_env arr
        | Identifier id -> identifier_pattern check_env id
        | Expression _ ->
          error_at env (loc, Parse_error.ExpectedPatternFoundExpression);
          check_env)
    and _object check_env o = List.fold_left object_property check_env o.Pattern.Object.properties
    and object_property check_env =
      Pattern.Object.(
        function
        | Property (_, property) ->
          Property.(
            let check_env =
              match property.key with
              | Identifier id -> identifier_no_dupe_check check_env id
              | _ -> check_env
            in
            pattern check_env property.pattern)
        | RestProperty (_, { RestProperty.argument }) -> pattern check_env argument)
    and _array check_env arr = List.fold_left array_element check_env arr.Pattern.Array.elements
    and array_element check_env =
      Pattern.Array.(
        function
        | None -> check_env
        | Some (Element (_, { Element.argument; default = _ })) -> pattern check_env argument
        | Some (RestElement (_, { RestElement.argument })) -> pattern check_env argument)
    and identifier_pattern check_env { Pattern.Identifier.name = id; _ } = identifier check_env id
    and identifier (env, param_names) ((loc, { Identifier.name; comments = _ }) as id) =
      if SSet.mem name param_names then error_at env (loc, Parse_error.StrictParamDupe);
      let (env, param_names) = identifier_no_dupe_check (env, param_names) id in
      (env, SSet.add name param_names)
    and identifier_no_dupe_check (env, param_names) (loc, { Identifier.name; comments = _ }) =
      if is_restricted name then strict_error_at env (loc, Parse_error.StrictParamName);
      if is_future_reserved name || is_strict_reserved name then
        strict_error_at env (loc, Parse_error.StrictReservedWord);
      (env, param_names)
    in
    pattern

  (* Strict is true if we were already in strict mode or if we are newly in
   * strict mode due to a directive in the function.
   * Simple is the IsSimpleParameterList thing from the ES6 spec *)
  let strict_post_check env ~strict ~simple id (_, { Ast.Function.Params.params; rest }) =
    if strict || not simple then (
      (* If we are doing this check due to strict mode than there are two
       * cases to consider. The first is when we were already in strict mode
       * and therefore already threw strict errors. In this case we want to
       * do these checks outside of strict mode. The other is if we
       * originally parsed in non-strict mode but now are strict. Then we
       * want to do these checks in strict mode *)
      let env =
        if strict then
          env |> with_strict (not (Parser_env.in_strict_mode env))
        else
          env
      in
      (match id with
      | Some (loc, { Identifier.name; comments = _ }) ->
        if is_restricted name then strict_error_at env (loc, Parse_error.StrictFunctionName);
        if is_future_reserved name || is_strict_reserved name then
          strict_error_at env (loc, Parse_error.StrictReservedWord)
      | None -> ());
      let acc =
        List.fold_left
          (fun acc (_, { Function.Param.argument; default = _ }) -> check_param acc argument)
          (env, SSet.empty)
          params
      in
      match rest with
      | Some (_, { Function.RestParam.argument }) -> ignore (check_param acc argument)
      | None -> ()
    )

  let function_params =
    let rec param =
      with_loc (fun env ->
          let argument = Parse.pattern env Parse_error.StrictParamName in
          let default =
            if Peek.token env = T_ASSIGN then (
              Expect.token env T_ASSIGN;
              Some (Parse.assignment env)
            ) else
              None
          in
          { Function.Param.argument; default })
    and param_list env acc =
      match Peek.token env with
      | (T_EOF | T_RPAREN | T_ELLIPSIS) as t ->
        let rest =
          if t = T_ELLIPSIS then
            let (loc, id) =
              with_loc
                (fun env ->
                  Expect.token env T_ELLIPSIS;
                  Parse.pattern env Parse_error.StrictParamName)
                env
            in
            Some (loc, { Function.RestParam.argument = id })
          else
            None
        in
        if Peek.token env <> T_RPAREN then error env Parse_error.ParameterAfterRestParameter;
        { Ast.Function.Params.params = List.rev acc; rest }
      | _ ->
        let the_param = param env in
        if Peek.token env <> T_RPAREN then Expect.token env T_COMMA;
        param_list env (the_param :: acc)
    in
    fun ~await ~yield ->
      with_loc (fun env ->
          let env =
            env
            |> with_allow_await await
            |> with_allow_yield yield
            |> with_in_formal_parameters true
          in
          Expect.token env T_LPAREN;
          let params = param_list env [] in
          Expect.token env T_RPAREN;
          params)

  let function_body env ~async ~generator =
    let env = enter_function env ~async ~generator in
    let (loc, block, strict) = Parse.function_block_body env in
    (Function.BodyBlock (loc, block), strict)

  let variance env is_async is_generator =
    let loc = Peek.loc env in
    let variance =
      match Peek.token env with
      | T_PLUS ->
        Eat.token env;
        Some (loc, Variance.Plus)
      | T_MINUS ->
        Eat.token env;
        Some (loc, Variance.Minus)
      | _ -> None
    in
    match variance with
    | Some (loc, _) when is_async || is_generator ->
      error_at env (loc, Parse_error.UnexpectedVariance);
      None
    | _ -> variance

  let generator env = Expect.maybe env T_MULT

  (* Returns true and consumes a token if the token is `async` and the token after it is on
     the same line (see https://tc39.github.io/ecma262/#sec-async-function-definitions) *)
  let async env =
    if Peek.token env = T_ASYNC && not (Peek.ith_is_line_terminator ~i:1 env) then
      let () = Eat.token env in
      true
    else
      false

  let is_simple_function_params =
    let is_simple_param = function
      | (_, { Ast.Function.Param.argument = (_, Pattern.Identifier _); default = None }) -> true
      | _ -> false
    in
    fun (_, { Ast.Function.Params.params; rest }) ->
      rest = None && List.for_all is_simple_param params

  let _function =
    with_loc (fun env ->
        let async = async env in
        let (sig_loc, (generator, tparams, id, params, return, predicate)) =
          with_loc
            (fun env ->
              Expect.token env T_FUNCTION;
              let generator = generator env in
              let (tparams, id) =
                match (in_export env, Peek.token env) with
                | (true, T_LPAREN) -> (None, None)
                | (true, T_LESS_THAN) ->
                  let tparams = Type.type_params env in
                  let id =
                    if Peek.token env = T_LPAREN then
                      None
                    else
                      Some (Parse.identifier ~restricted_error:Parse_error.StrictFunctionName env)
                  in
                  (tparams, id)
                | _ ->
                  let id = Parse.identifier ~restricted_error:Parse_error.StrictFunctionName env in
                  (Type.type_params env, Some id)
              in
              let params = function_params ~await:async ~yield:generator env in
              let (return, predicate) = Type.annotation_and_predicate_opt env in
              (generator, tparams, id, params, return, predicate))
            env
        in
        let (body, strict) = function_body env ~async ~generator in
        let simple = is_simple_function_params params in
        strict_post_check env ~strict ~simple id params;
        Statement.FunctionDeclaration
          { Function.id; params; body; generator; async; predicate; return; tparams; sig_loc })

  let variable_declaration_list =
    let variable_declaration env =
      let (loc, (decl, err)) =
        with_loc
          (fun env ->
            let id = Parse.pattern env Parse_error.StrictVarName in
            let (init, err) =
              if Expect.maybe env T_ASSIGN then
                (Some (Parse.assignment env), None)
              else
                match id with
                | (_, Ast.Pattern.Identifier _) -> (None, None)
                | (loc, _) -> (None, Some (loc, Parse_error.NoUninitializedDestructuring))
            in
            (Ast.Statement.VariableDeclaration.Declarator.{ id; init }, err))
          env
      in
      ((loc, decl), err)
    in
    let rec helper env decls errs =
      let (decl, err) = variable_declaration env in
      let decls = decl :: decls in
      let errs =
        match err with
        | Some x -> x :: errs
        | None -> errs
      in
      if Expect.maybe env T_COMMA then
        helper env decls errs
      else
        (List.rev decls, List.rev errs)
    in
    (fun env -> helper env [] [])

  let declarations token kind env =
    Expect.token env token;
    let (declarations, errs) = variable_declaration_list env in
    (Statement.VariableDeclaration.{ kind; declarations }, errs)

  let var = declarations T_VAR Statement.VariableDeclaration.Var

  let const env =
    let env = env |> with_no_let true in
    let (variable, errs) = declarations T_CONST Statement.VariableDeclaration.Const env in
    (* Make sure all consts defined are initialized *)
    let errs =
      List.fold_left
        (fun errs decl ->
          match decl with
          | (loc, { Statement.VariableDeclaration.Declarator.init = None; _ }) ->
            (loc, Parse_error.NoUninitializedConst) :: errs
          | _ -> errs)
        errs
        variable.Statement.VariableDeclaration.declarations
    in
    (variable, List.rev errs)

  let let_ env =
    let env = env |> with_no_let true in
    declarations T_LET Statement.VariableDeclaration.Let env

  let enum_declaration = Enum.declaration
end
