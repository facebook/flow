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

module type TYPE = sig
  val _type : env -> Ast.Type.t
  val type_parameter_declaration : env -> Ast.Type.ParameterDeclaration.t option
  val type_parameter_declaration_with_defaults : env -> Ast.Type.ParameterDeclaration.t option
  val type_parameter_instantiation : env -> Ast.Type.ParameterInstantiation.t option
  val generic : env -> Loc.t * Ast.Type.Generic.t
  val _object : allow_static:bool -> env -> Loc.t * Type.Object.t
  val function_param_list : env -> Type.Function.Param.t list * Type.Function.RestParam.t option
  val annotation : env -> Ast.Type.annotation
  val annotation_opt : env -> Ast.Type.annotation option
  val predicate_opt : env -> Ast.Type.Predicate.t option
  val annotation_and_predicate_opt : env -> Ast.Type.annotation option * Ast.Type.Predicate.t option
end

module Type (Parse: Parser_common.PARSER) : TYPE = struct
  type param_list_or_type =
    | ParamList of (Type.Function.Param.t list * Type.Function.RestParam.t option)
    | Type of Type.t

  let rec _type env = union env

  and annotation env =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeAnnotation;
    let start_loc = Peek.loc env in
    Expect.token env T_COLON;
    let typeAnnotation = _type env in
    let end_loc = match last_loc env with
    | Some loc -> loc
    | None -> assert false in
    Loc.btwn start_loc end_loc, typeAnnotation

  and variance env =
    let loc = Peek.loc env in
    match Peek.token env with
     | T_PLUS ->
         Eat.token env;
         Some (loc, Variance.Plus)
     | T_MINUS ->
         Eat.token env;
         Some (loc, Variance.Minus)
     | _ ->
         None

  and rev_nonempty_acc acc =
    let end_loc = match acc with
    | (loc, _)::_ -> loc
    | _ -> assert false in
    let acc = List.rev acc in
    let start_loc = match acc with
    | (loc, _)::_ -> loc
    | _ -> assert false in
    Loc.btwn start_loc end_loc, acc

  and union env =
    let _ = Expect.maybe env T_BIT_OR in
    let left = intersection env in
    union_with env left

  and union_with =
    let rec unions env acc =
      match Peek.token env with
      | T_BIT_OR ->
          Expect.token env T_BIT_OR;
          unions env (intersection env::acc)
      | _ ->
          let loc, acc = rev_nonempty_acc acc in
          match acc with
          | t0::t1::ts -> loc, Type.Union (t0, t1, ts)
          | _ -> assert false
    in fun env left ->
      if Peek.token env = T_BIT_OR
      then unions env [left]
      else left

  and intersection env =
    let _ = Expect.maybe env T_BIT_AND in
    let left = anon_function_without_parens env in
    intersection_with env left

  and intersection_with =
    let rec intersections env acc =
      match Peek.token env with
      | T_BIT_AND ->
          Expect.token env T_BIT_AND;
          intersections env (anon_function_without_parens env::acc)
      | _ ->
          let loc, acc = rev_nonempty_acc acc in
          match acc with
          | t0::t1::ts -> loc, Type.Intersection (t0, t1, ts)
          | _ -> assert false
    in fun env left ->
      if Peek.token env = T_BIT_AND
      then intersections env [left]
      else left


  and anon_function_without_parens env =
    let param = prefix env in
    anon_function_without_parens_with env param

  and anon_function_without_parens_with env param =
    match Peek.token env with
    | T_ARROW when not (no_anon_function_type env)->
      let start_loc, typeParameters, params =
        let param = anonymous_function_param env param in
        fst param, None, ([param], None)
      in
      function_with_params env start_loc typeParameters params
    | _ -> param

  and prefix env =
    match Peek.token env with
    | T_PLING ->
        let loc = Peek.loc env in
        Expect.token env T_PLING;
        let t = prefix env in
        Loc.btwn loc (fst t), Type.Nullable t
    | _ ->
        postfix env

  and postfix env =
    let t = primary env in
    postfix_with env t

  and postfix_with env t =
    if not (Peek.is_line_terminator env) && Expect.maybe env T_LBRACKET
    then begin
      let end_loc = Peek.loc env in
      Expect.token env T_RBRACKET;
      let loc = Loc.btwn (fst t) end_loc in
      let t = loc, Type.Array t in
      postfix_with env t
    end else t

  and primary env =
    let loc = Peek.loc env in
    match Peek.token env with
    | T_MULT ->
        Expect.token env T_MULT;
        loc, Type.Exists
    | T_LESS_THAN -> _function env
    | T_LPAREN -> function_or_group env
    | T_LCURLY
    | T_LCURLYBAR ->
      let loc, o = _object env
        ~allow_static:false ~allow_exact:true ~allow_spread:true in
      loc, Type.Object o
    | T_TYPEOF ->
        let start_loc = Peek.loc env in
        Expect.token env T_TYPEOF;
        let t = primary env in
        Loc.btwn start_loc (fst t), Type.Typeof t
    | T_LBRACKET -> tuple env
    | T_IDENTIFIER ->
        let loc, g = generic env in
        loc, Type.Generic g
    | T_STRING (loc, value, raw, octal)  ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        loc, Type.StringLiteral {
          Type.StringLiteral.value;
          raw;
        }
    | T_NUMBER_SINGLETON_TYPE (number_type, value) ->
        let raw = Peek.value env in
        Expect.token env (T_NUMBER_SINGLETON_TYPE (number_type, value));
        if number_type = LEGACY_OCTAL
        then strict_error env Error.StrictOctalLiteral;
        loc, Type.NumberLiteral {
          Type.NumberLiteral.value;
          raw;
        }
    | (T_TRUE | T_FALSE) as token ->
        let raw = Peek.value env in
        Expect.token env token;
        let value = token = T_TRUE in
        loc, Type.BooleanLiteral {
          Type.BooleanLiteral.value;
          raw;
        }
    | token ->
        match primitive token with
        | Some t ->
            Expect.token env token;
            loc, t
        | None ->
            error_unexpected env;
            loc, Type.Any

  and primitive = function
    | T_ANY_TYPE     -> Some Type.Any
    | T_MIXED_TYPE   -> Some Type.Mixed
    | T_EMPTY_TYPE   -> Some Type.Empty
    | T_BOOLEAN_TYPE -> Some Type.Boolean
    | T_NUMBER_TYPE  -> Some Type.Number
    | T_STRING_TYPE  -> Some Type.String
    | T_VOID_TYPE    -> Some Type.Void
    | T_NULL         -> Some Type.Null
    | _ -> None

  and tuple =
    let rec types env acc =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET -> List.rev acc
      | _ ->
          let acc = (_type env)::acc in
          (* Trailing comma support (like [number, string,]) *)
          if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
          types env acc

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LBRACKET;
      let tl = types env [] in
      let end_loc = Peek.loc env in
      Expect.token env T_RBRACKET;
      Loc.btwn start_loc end_loc, Type.Tuple tl

  and anonymous_function_param _env typeAnnotation =
    fst typeAnnotation, Type.Function.Param.({
      name = None;
      typeAnnotation;
      optional = false;
    })


  and function_param_with_id env name =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeAnnotation;
    let optional = Expect.maybe env T_PLING in
    Expect.token env T_COLON;
    let typeAnnotation = _type env in
    Loc.btwn (fst name) (fst typeAnnotation), Type.Function.Param.({
      name = Some name;
      typeAnnotation;
      optional;
    })

  and function_param_list_without_parens =
    let param env =
      match Peek.token ~i:1 env with
      | T_COLON | T_PLING ->
          let name, _ = Parse.identifier_or_reserved_keyword env in
          function_param_with_id env name
      | _ ->
          let typeAnnotation = _type env in
          anonymous_function_param env typeAnnotation

    in let rec param_list env acc =
      match Peek.token env with
      | T_EOF
      | T_ELLIPSIS
      | T_RPAREN as t ->
        let rest =
          if t = T_ELLIPSIS then begin
            let start_loc = Peek.loc env in
            Expect.token env T_ELLIPSIS;
            let argument = param env in
            let loc = Loc.btwn start_loc (fst argument) in
            Some (loc, Type.Function.RestParam.({
              argument;
            }))
          end else
            None
        in
        List.rev acc, rest
      | _ ->
        let acc = (param env)::acc in
        if Peek.token env <> T_RPAREN
        then Expect.token env T_COMMA;
        param_list env acc

    in fun env -> param_list env

  and function_param_list env =
    Expect.token env T_LPAREN;
    let ret = function_param_list_without_parens env [] in
    Expect.token env T_RPAREN;
    ret

  and param_list_or_type env =
    Expect.token env T_LPAREN;
    let ret =
      let env = with_no_anon_function_type false env in
      match Peek.token env with
      | T_EOF
      | T_ELLIPSIS ->
          (* (... is definitely the beginning of a param list *)
          ParamList (function_param_list_without_parens env [])
      | T_RPAREN ->
          (* () or is definitely a param list *)
          ParamList ([], None)
      | T_IDENTIFIER ->
          (* This could be a function parameter or a generic type *)
          function_param_or_generic_type env
      | token ->
          (match primitive token with
          | None ->
              (* All params start with an identifier or `...` *)
              Type (_type env)
          | Some _ ->
              (* Don't know if this is (number) or (number: number). The first
               * is a type, the second is a param. *)
              match Peek.token ~i:1 env with
              | T_PLING | T_COLON ->
                (* Ok this is definitely a parameter *)
                ParamList (function_param_list_without_parens env [])
              | _ ->
                Type (_type env)
          )
    in
    (* Now that we allow anonymous parameters in function types, we need to
     * disambiguate a little bit more *)
    let ret = match ret with
    | ParamList _ -> ret
    | Type _ when no_anon_function_type env -> ret
    | Type t ->
        (match Peek.token env with
        | T_RPAREN ->
            (* Reinterpret `(type) =>` as a ParamList *)
            if Peek.token ~i:1 env = T_ARROW
            then
              let param = anonymous_function_param env t in
              ParamList (function_param_list_without_parens env [param])
            else Type t
        | T_COMMA ->
            (* Reinterpret `(type,` as a ParamList *)
            Expect.token env T_COMMA;
            let param = anonymous_function_param env t in
            ParamList (function_param_list_without_parens env [param])
        | _ -> ret) in
    Expect.token env T_RPAREN;
    ret

  and function_param_or_generic_type env =
    let id = Parse.identifier env in
    match Peek.token env with
    | T_PLING (* optional param *)
    | T_COLON ->
        let param = function_param_with_id env id in
        ignore (Expect.maybe env T_COMMA);
        ParamList (function_param_list_without_parens env [param])
    | _ ->
        Type (
          generic_type_with_identifier env id
          |> postfix_with env
          |> anon_function_without_parens_with env
          |> intersection_with env
          |> union_with env
        )

  and function_or_group env =
    let start_loc = Peek.loc env in
    match param_list_or_type env with
    | ParamList params -> function_with_params env start_loc None params
    | Type _type -> _type

  and _function env =
    let start_loc = Peek.loc env in
    let typeParameters = type_parameter_declaration ~allow_default:false env in
    let params = function_param_list env in
    function_with_params env start_loc typeParameters params

  and function_with_params env start_loc typeParameters params =
    Expect.token env T_ARROW;
    let returnType = _type env in
    let end_loc = fst returnType in
    Loc.btwn start_loc end_loc, Type.(Function Function.({
      params;
      returnType;
      typeParameters;
    }))

  and _object =
    let methodish env start_loc type_params =
      let params = function_param_list env in
      Expect.token env T_COLON;
      let returnType = _type env in
      let loc = Loc.btwn start_loc (fst returnType) in
      loc, Type.Function.({
        params;
        returnType;
        typeParameters = type_params;
      })

    in let method_property env start_loc static key =
      let type_params = type_parameter_declaration ~allow_default:false env in
      let value = methodish env start_loc type_params in
      let value = fst value, Type.Function (snd value) in
      Type.Object.(Property (fst value, Property.({
        key;
        value = Init value;
        optional = false;
        static;
        _method = true;
        variance = None;
      })))

    in let call_property env start_loc static =
      let type_params = type_parameter_declaration ~allow_default:false env in
      let value = methodish env (Peek.loc env) type_params in
      Type.Object.(CallProperty (Loc.btwn start_loc (fst value), CallProperty.({
        value;
        static;
      })))

    in let property env start_loc static variance key =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAnnotation;
      let optional = Expect.maybe env T_PLING in
      Expect.token env T_COLON;
      let value = _type env in
      Type.Object.(Property (Loc.btwn start_loc (fst value), Property.({
        key;
        value = Init value;
        optional;
        static;
        _method = false;
        variance;
      })))

    in let getter_or_setter ~is_getter env start_loc static key =
      let value = methodish env start_loc None in
      let (key_loc, key) = key in
      let (_, { Type.Function.params; _ }) = value in
      begin match is_getter, params with
      | true, ([], None) -> ()
      | false, (_, Some _rest) ->
          (* rest params don't make sense on a setter *)
          error_at env (key_loc, Error.SetterArity)
      | false, ([_], _) -> ()
      | true, _ -> error_at env (key_loc, Error.GetterArity)
      | false, _ -> error_at env (key_loc, Error.SetterArity)
      end;
      Type.Object.(Property (Loc.btwn start_loc (fst value), Property.({
        key;
        value = if is_getter then Get value else Set value;
        optional = false;
        static;
        _method = false;
        variance = None;
      })))

    in let indexer_property env start_loc static variance =
      Expect.token env T_LBRACKET;
      let id =
        if Peek.token ~i:1 env = T_COLON
        then begin
          let id, _ = Parse.identifier_or_reserved_keyword env in
          Expect.token env T_COLON;
          Some id
        end else None in
      let key = _type env in
      Expect.token env T_RBRACKET;
      Expect.token env T_COLON;
      let value = _type env in
      Type.Object.(Indexer (Loc.btwn start_loc (fst value), Indexer.({
        id;
        key;
        value;
        static;
        variance;
      })))

    in let semicolon exact env =
      match Peek.token env with
      | T_COMMA | T_SEMICOLON -> Eat.token env
      | T_RCURLYBAR when exact -> ()
      | T_RCURLY when not exact -> ()
      | _ -> error_unexpected env

    in let error_unsupported_variance env = function
    | Some (loc, _) -> error_at env (loc, Error.UnexpectedVariance)
    | None -> ()

    in let rec properties ~allow_static ~allow_spread ~exact env acc =
      assert (not (allow_static && allow_spread)); (* no `static ...A` *)
      let start_loc = Peek.loc env in
      let static = allow_static && Expect.maybe env T_STATIC in
      let variance = variance env in
      match Peek.token env with
      | T_EOF ->
        List.rev acc
      | T_RCURLYBAR when exact ->
        List.rev acc
      | T_RCURLY when not exact ->
        List.rev acc
      | T_LBRACKET ->
        let indexer = indexer_property env start_loc static variance in
        semicolon exact env;
        properties ~allow_static ~allow_spread ~exact env (indexer::acc)
      | T_LESS_THAN
      | T_LPAREN ->
        error_unsupported_variance env variance;
        let call_prop = call_property env start_loc static in
        semicolon exact env;
        properties ~allow_static ~allow_spread ~exact env (call_prop::acc)
      | T_ELLIPSIS when allow_spread ->
        error_unsupported_variance env variance;
        Eat.token env;
        let (arg_loc, _) as argument = _type env in
        let loc = Loc.btwn start_loc arg_loc in
        let property = Type.Object.(SpreadProperty (loc, { SpreadProperty.
          argument;
        })) in
        semicolon exact env;
        properties ~allow_static ~allow_spread ~exact env (property::acc)
      | token ->
        let property = match static, variance, token with
        | true, None, T_COLON ->
            strict_error_at env (start_loc, Error.StrictReservedWord);
            let key = Expression.Object.Property.Identifier (
              start_loc,
              "static"
            ) in
            let static = false in
            begin match Peek.token env with
            | T_LESS_THAN
            | T_LPAREN ->
              error_unsupported_variance env variance;
              method_property env start_loc static key
            | _ ->
              property env start_loc static variance key
            end
        | _ ->
            let object_key env =
              Eat.push_lex_mode env Lex_mode.NORMAL;
              let result = Parse.object_key env in
              Eat.pop_lex_mode env;
              result
            in
            begin match object_key env with
            | _, (Expression.Object.Property.Identifier
                  (_, ("get" | "set" as name)) as key) ->
                begin match Peek.token env with
                | T_LESS_THAN
                | T_LPAREN ->
                  error_unsupported_variance env variance;
                  method_property env start_loc static key
                | T_COLON
                | T_PLING ->
                  property env start_loc static variance key
                | _ ->
                  let key = object_key env in
                  let is_getter = name = "get" in
                  error_unsupported_variance env variance;
                  getter_or_setter ~is_getter env start_loc static key
                end
            | _, key ->
                begin match Peek.token env with
                | T_LESS_THAN
                | T_LPAREN ->
                  error_unsupported_variance env variance;
                  method_property env start_loc static key
                | _ ->
                  property env start_loc static variance key
                end
            end
        in
        semicolon exact env;
        properties ~allow_static ~allow_spread ~exact env (property::acc)

    in fun ~allow_static ~allow_exact ~allow_spread env ->
      let exact = allow_exact && Peek.token env = T_LCURLYBAR in
      let start_loc = Peek.loc env in
      Expect.token env (if exact then T_LCURLYBAR else T_LCURLY);
      let properties = properties ~allow_static ~exact ~allow_spread env [] in
      let end_loc = Peek.loc env in
      Expect.token env (if exact then T_RCURLYBAR else T_RCURLY);
      Loc.btwn start_loc end_loc, Type.Object.({
        exact;
        properties;
      })

  and type_parameter_declaration =
    let rec params env ~allow_default ~require_default acc = Type.ParameterDeclaration.TypeParam.(
      let variance = variance env in
      let loc, {
        Pattern.Identifier.name = (_, name);
        typeAnnotation = bound;
        _;
      } = Parse.identifier_with_type env Error.StrictParamName in
      let default, require_default = match allow_default, Peek.token env with
      | false, _ -> None, false
      | true, T_ASSIGN ->
          Eat.token env;
          Some (_type env), true
      | true, _ ->
          if require_default
          then error_at env (loc, Error.MissingTypeParamDefault);
          None, require_default in
      let param = loc, {
        name;
        bound;
        variance;
        default;
      } in
      let acc = param::acc in
      match Peek.token env with
      | T_EOF
      | T_GREATER_THAN -> List.rev acc
      | _ ->
        Expect.token env T_COMMA;
        if Peek.token env = T_GREATER_THAN
        then List.rev acc
        else params env ~allow_default ~require_default acc
    )
    in fun ~allow_default env ->
        let start_loc = Peek.loc env in
        if Peek.token env = T_LESS_THAN
        then begin
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeAnnotation;
          Expect.token env T_LESS_THAN;
          let params = params env ~allow_default ~require_default:false [] in
          let loc = Loc.btwn start_loc (Peek.loc env) in
          Expect.token env T_GREATER_THAN;
          Some (loc, Type.ParameterDeclaration.({
            params;
          }))
        end else None

  and type_parameter_instantiation =
    let rec params env acc =
      match Peek.token env with
      | T_EOF
      | T_GREATER_THAN -> List.rev acc
      | _ ->
        let acc = (_type env)::acc in
        if Peek.token env <> T_GREATER_THAN
        then Expect.token env T_COMMA;
        params env acc

    in fun env ->
        let start_loc = Peek.loc env in
        if Peek.token env = T_LESS_THAN
        then begin
          Expect.token env T_LESS_THAN;
          let params = params env [] in
          let loc = Loc.btwn start_loc (Peek.loc env) in
          Expect.token env T_GREATER_THAN;
          Some (loc, Type.ParameterInstantiation.({
            params;
          }))
        end else None

  and generic env = raw_generic_with_identifier env (Parse.identifier env)

  and raw_generic_with_identifier =
    let rec identifier env (q_loc, qualification) =
      if Peek.token env = T_PERIOD
      then begin
        Expect.token env T_PERIOD;
        let id = Parse.identifier env in
        let loc = Loc.btwn q_loc (fst id) in
        let qualification = Type.Generic.Identifier.(Qualified (loc, {
          qualification;
          id;
        })) in
        identifier env (loc, qualification)
      end else (q_loc, qualification)

    in fun env id ->
      let id = fst id, Type.Generic.Identifier.Unqualified id in
      let id_loc, id = identifier env id in
      let typeParameters = type_parameter_instantiation env in
      let loc = match typeParameters with
      | None -> id_loc
      | Some (loc, _) -> Loc.btwn id_loc loc in
      loc, Type.Generic.({
        id;
        typeParameters;
      })

  and generic_type_with_identifier env id =
    let loc, generic = raw_generic_with_identifier env id in
    loc, Type.Generic generic

  and annotation_opt env =
    match Peek.token env with
    | T_COLON -> Some (annotation env)
    | _ -> None

  let predicate env =
    let checks_loc = Peek.loc env in
    Expect.token env T_CHECKS;
    if Peek.token env = T_LPAREN then begin
      Expect.token env T_LPAREN;
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let exp = Parse.conditional env in
      Eat.pop_lex_mode env;
      let rparen_loc = Peek.loc env in
      Expect.token env T_RPAREN;
      let loc = Loc.btwn checks_loc rparen_loc in
      (loc, Ast.Type.Predicate.Declared exp)
    end else
      (checks_loc, Ast.Type.Predicate.Inferred)

  let predicate_opt env =
    let env = with_no_anon_function_type false env in
    match Peek.token env with
    | T_CHECKS -> Some (predicate env)
    | _ -> None

  let annotation_and_predicate_opt env =
    match Peek.token env, Peek.token ~i:1 env with
    | T_COLON, T_CHECKS ->
      Expect.token env T_COLON;
      (None, predicate_opt env)
    | T_COLON, _ ->
       let annotation = annotation_opt env in
       let predicate = predicate_opt env in
       (annotation, predicate)
    | _ -> None, None

  let wrap f env =
    let env = env |> with_strict true in
    Eat.push_lex_mode env Lex_mode.TYPE;
    let ret = f env in
    Eat.pop_lex_mode env;
    ret

  let _type = wrap _type
  let type_parameter_declaration_with_defaults =
    wrap (type_parameter_declaration ~allow_default:true)
  let type_parameter_declaration =
    wrap (type_parameter_declaration ~allow_default:false)
  let type_parameter_instantiation = wrap type_parameter_instantiation
  let _object ~allow_static env =
    wrap (_object ~allow_static ~allow_exact:false ~allow_spread:false) env
  let function_param_list = wrap function_param_list
  let annotation = wrap annotation
  let annotation_opt = wrap annotation_opt
  let predicate_opt = wrap predicate_opt
  let annotation_and_predicate_opt = wrap annotation_and_predicate_opt
  let generic = wrap generic
end
