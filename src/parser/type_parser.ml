(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_env
open Ast
open Parser_common
module Error = Parse_error

module type TYPE = sig
  val _type : env -> Loc.t Ast.Type.t
  val type_identifier : env -> Loc.t * string
  val type_parameter_declaration : env -> Loc.t Ast.Type.ParameterDeclaration.t option
  val type_parameter_declaration_with_defaults : env -> Loc.t Ast.Type.ParameterDeclaration.t option
  val type_parameter_instantiation : env -> Loc.t Ast.Type.ParameterInstantiation.t option
  val generic : env -> Loc.t * Loc.t Ast.Type.Generic.t
  val _object : allow_static:bool -> env -> Loc.t * Loc.t Type.Object.t
  val function_param_list : env -> Loc.t Type.Function.Params.t
  val annotation : env -> Loc.t Ast.Type.annotation
  val annotation_opt : env -> Loc.t Ast.Type.annotation option
  val predicate_opt : env -> Loc.t Ast.Type.Predicate.t option
  val annotation_and_predicate_opt : env -> Loc.t Ast.Type.annotation option * Loc.t Ast.Type.Predicate.t option
end

module Type (Parse: Parser_common.PARSER) : TYPE = struct
  type param_list_or_type =
    | ParamList of Loc.t Type.Function.Params.t'
    | Type of Loc.t Type.t

  let rec _type env = union env

  and annotation env =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeAnnotation;
    with_loc (fun env ->
      Expect.token env T_COLON;
      _type env
    ) env

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

  and union env =
    let _ = Expect.maybe env T_BIT_OR in
    let left = intersection env in
    union_with env left

  and union_with =
    let rec unions acc env =
      match Peek.token env with
      | T_BIT_OR ->
          Expect.token env T_BIT_OR;
          unions (intersection env::acc) env
      | _ ->
          match List.rev acc with
          | t0::t1::ts -> Type.Union (t0, t1, ts)
          | _ -> assert false
    in fun env left ->
      if Peek.token env = T_BIT_OR
      then with_loc ~start_loc:(fst left) (unions [left]) env
      else left

  and intersection env =
    let _ = Expect.maybe env T_BIT_AND in
    let left = anon_function_without_parens env in
    intersection_with env left

  and intersection_with =
    let rec intersections acc env =
      match Peek.token env with
      | T_BIT_AND ->
          Expect.token env T_BIT_AND;
          intersections (anon_function_without_parens env::acc) env
      | _ ->
          match List.rev acc with
          | t0::t1::ts -> Type.Intersection (t0, t1, ts)
          | _ -> assert false
    in fun env left ->
      if Peek.token env = T_BIT_AND
      then with_loc ~start_loc:(fst left) (intersections [left]) env
      else left


  and anon_function_without_parens env =
    let param = prefix env in
    anon_function_without_parens_with env param

  and anon_function_without_parens_with env param =
    match Peek.token env with
    | T_ARROW when not (no_anon_function_type env)->
      let start_loc, tparams, params =
        let param = anonymous_function_param env param in
        fst param, None, (fst param, { Ast.Type.Function.Params.
          params = [param];
          rest = None;
        })
      in
      function_with_params env start_loc tparams params
    | _ -> param

  and prefix env =
    match Peek.token env with
    | T_PLING ->
        with_loc (fun env ->
          Expect.token env T_PLING;
          Type.Nullable (prefix env)
        ) env
    | _ ->
        postfix env

  and postfix env =
    let t = primary env in
    postfix_with env t

  and postfix_with env t =
    if not (Peek.is_line_terminator env) && Expect.maybe env T_LBRACKET
    then begin
      let t = with_loc ~start_loc:(fst t) (fun env ->
        Expect.token env T_RBRACKET;
        Type.Array t
      ) env in
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
        with_loc (fun env ->
          Expect.token env T_TYPEOF;
          Type.Typeof (primary env)
        ) env
    | T_LBRACKET -> tuple env
    | T_IDENTIFIER _
    | T_STATIC (* `static` is reserved in strict mode, but still an identifier *) ->
        let loc, g = generic env in
        loc, Type.Generic g
    | T_STRING (loc, value, raw, octal)  ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        loc, Type.StringLiteral {
          Ast.StringLiteral.value;
          raw;
        }
    | T_NUMBER_SINGLETON_TYPE { kind; value; raw } ->
        Expect.token env (T_NUMBER_SINGLETON_TYPE { kind; value; raw });
        if kind = LEGACY_OCTAL
        then strict_error env Error.StrictOctalLiteral;
        loc, Type.NumberLiteral {
          Ast.NumberLiteral.value;
          raw;
        }
    | (T_TRUE | T_FALSE) as token ->
        Expect.token env token;
        let value = token = T_TRUE in
        loc, Type.BooleanLiteral value
    | token ->
        match primitive token with
        | Some t ->
            Expect.token env token;
            loc, t
        | None ->
            error_unexpected env;
            loc, Type.Any

  and primitive = function
    | T_ANY_TYPE -> Some Type.Any
    | T_MIXED_TYPE -> Some Type.Mixed
    | T_EMPTY_TYPE -> Some Type.Empty
    | T_BOOLEAN_TYPE _ -> Some Type.Boolean
    | T_NUMBER_TYPE -> Some Type.Number
    | T_STRING_TYPE -> Some Type.String
    | T_VOID_TYPE -> Some Type.Void
    | T_NULL -> Some Type.Null
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
      with_loc (fun env ->
        Expect.token env T_LBRACKET;
        let tl = types env [] in
        Expect.token env T_RBRACKET;
        Type.Tuple tl
      ) env

  and anonymous_function_param _env annot =
    fst annot, Type.Function.Param.({
      name = None;
      annot;
      optional = false;
    })


  and function_param_with_id env name =
    if not (should_parse_types env)
    then error env Error.UnexpectedTypeAnnotation;
    with_loc ~start_loc:(fst name) (fun env ->
      let optional = Expect.maybe env T_PLING in
      Expect.token env T_COLON;
      let annot = _type env in
      { Type.Function.Param.
        name = Some name;
        annot;
        optional;
      }
    ) env

  and function_param_list_without_parens =
    let param env =
      match Peek.ith_token ~i:1 env with
      | T_COLON | T_PLING ->
          let id = Parse.identifier env in
          function_param_with_id env id
      | _ ->
          let annot = _type env in
          anonymous_function_param env annot

    in let rec param_list env acc =
      match Peek.token env with
      | T_EOF
      | T_ELLIPSIS
      | T_RPAREN as t ->
        let rest =
          if t = T_ELLIPSIS then begin
            let rest = with_loc (fun env ->
              Expect.token env T_ELLIPSIS;
              { Type.Function.RestParam.argument = param env }
            ) env in
            Some rest
          end else
            None
        in
        { Ast.Type.Function.Params.params = List.rev acc; rest; }
      | _ ->
        let acc = (param env)::acc in
        if Peek.token env <> T_RPAREN
        then Expect.token env T_COMMA;
        param_list env acc

    in fun env -> param_list env

  and function_param_list env =
    with_loc (fun env ->
      Expect.token env T_LPAREN;
      let ret = function_param_list_without_parens env [] in
      Expect.token env T_RPAREN;
      ret
    ) env

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
          ParamList ({ Ast.Type.Function.Params.params = []; rest = None })
      | T_IDENTIFIER _
      | T_STATIC (* `static` is reserved in strict mode, but still an identifier *) ->
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
              match Peek.ith_token ~i:1 env with
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
            if Peek.ith_token ~i:1 env = T_ARROW
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
    match Peek.ith_token ~i:1 env with
    | T_PLING (* optional param *)
    | T_COLON ->
        let id = Parse.identifier env in
        let param = function_param_with_id env id in
        ignore (Expect.maybe env T_COMMA);
        ParamList (function_param_list_without_parens env [param])
    | _ ->
        let id = type_identifier env in
        Type (
          generic_type_with_identifier env id
          |> postfix_with env
          |> anon_function_without_parens_with env
          |> intersection_with env
          |> union_with env
        )

  and function_or_group env =
    let start_loc = Peek.loc env in
    match with_loc param_list_or_type env with
    | loc, ParamList params -> function_with_params env start_loc None (loc, params)
    | _, Type _type -> _type

  and _function env =
    let start_loc = Peek.loc env in
    let tparams = type_parameter_declaration ~allow_default:false env in
    let params = function_param_list env in
    function_with_params env start_loc tparams params

  and function_with_params env start_loc tparams (params: Loc.t Ast.Type.Function.Params.t) =
    with_loc ~start_loc (fun env ->
      Expect.token env T_ARROW;
      let return = _type env in
      Type.(Function { Function.params; return; tparams })
    ) env

  and _object =
    let methodish env start_loc tparams =
      with_loc ~start_loc (fun env ->
        let params = function_param_list env in
        Expect.token env T_COLON;
        let return = _type env in
        { Type.Function.
          params;
          return;
          tparams;
        }
      ) env

    in let method_property env start_loc static key =
      let tparams = type_parameter_declaration ~allow_default:false env in
      let value = methodish env start_loc tparams in
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
      let prop = with_loc ~start_loc (fun env ->
        let tparams = type_parameter_declaration ~allow_default:false env in
        let value = methodish env (Peek.loc env) tparams in
        { Type.Object.CallProperty.value; static }
      ) env in
      Type.Object.CallProperty prop

    in let property env start_loc static variance key =
      if not (should_parse_types env)
      then error env Error.UnexpectedTypeAnnotation;
      let prop = with_loc ~start_loc (fun env ->
        let optional = Expect.maybe env T_PLING in
        Expect.token env T_COLON;
        let value = _type env in
        Type.Object.Property.({
          key;
          value = Init value;
          optional;
          static;
          _method = false;
          variance;
        })
      ) env in
      Type.Object.Property prop

    in let getter_or_setter ~is_getter env start_loc static key =
      let prop = with_loc ~start_loc (fun env ->
        let value = methodish env start_loc None in
        let (key_loc, key) = key in
        let (_, { Type.Function.params; _ }) = value in
        begin match is_getter, params with
        | true, (_, { Type.Function.Params.params = []; rest = None }) -> ()
        | false, (_, { Type.Function.Params.rest = Some _; _ }) ->
            (* rest params don't make sense on a setter *)
            error_at env (key_loc, Error.SetterArity)
        | false, (_, { Type.Function.Params.params = [_]; _ }) -> ()
        | true, _ -> error_at env (key_loc, Error.GetterArity)
        | false, _ -> error_at env (key_loc, Error.SetterArity)
        end;
        Type.Object.Property.({
          key;
          value = if is_getter then Get value else Set value;
          optional = false;
          static;
          _method = false;
          variance = None;
        })
      ) env in
      Type.Object.Property prop

    in let indexer_property env start_loc static variance =
      let indexer = with_loc ~start_loc (fun env ->
        Expect.token env T_LBRACKET;
        let id =
          if Peek.ith_token ~i:1 env = T_COLON
          then begin
            let id = identifier_name env in
            Expect.token env T_COLON;
            Some id
          end else None in
        let key = _type env in
        Expect.token env T_RBRACKET;
        Expect.token env T_COLON;
        let value = _type env in
        { Type.Object.Indexer.
          id;
          key;
          value;
          static;
          variance;
        }
      ) env in
      Type.Object.Indexer indexer

    in let semicolon exact env =
      match Peek.token env with
      | T_COMMA | T_SEMICOLON -> Eat.token env
      | T_RCURLYBAR when exact -> ()
      | T_RCURLY when not exact -> ()
      | _ -> error_unexpected env

    in let error_unsupported_variance env = function
    | Some (loc, _) -> error_at env (loc, Error.UnexpectedVariance)
    | None -> ()

    in let error_if_unexpected_static env ~allow_static = function
      | Some loc when not allow_static ->
        error_at env (loc, Error.UnexpectedStatic)
      | _ -> ()

    in let rec properties ~allow_static ~allow_spread ~exact env acc =
      assert (not (allow_static && allow_spread)); (* no `static ...A` *)
      let start_loc = Peek.loc env in
      let static, static_loc = match Peek.token env with
      | T_STATIC ->
        Eat.token env;
        true, Some start_loc
      | _ -> false, None
      in
      let variance = variance env in
      match Peek.token env with
      | T_EOF ->
        error_if_unexpected_static env ~allow_static static_loc;
        List.rev acc
      | T_RCURLYBAR when exact ->
        error_if_unexpected_static env ~allow_static static_loc;
        List.rev acc
      | T_RCURLY when not exact ->
        error_if_unexpected_static env ~allow_static static_loc;
        List.rev acc
      | T_LBRACKET ->
        error_if_unexpected_static env ~allow_static static_loc;
        let indexer = indexer_property env start_loc static variance in
        semicolon exact env;
        properties ~allow_static ~allow_spread ~exact env (indexer::acc)
      | T_LESS_THAN
      | T_LPAREN ->
        (* If `not allow_static`, we want to treat `static(): T` and
           `static<T>(): U` as a method property, which is handled in the
           catch-all case below. *)
        error_unsupported_variance env variance;
        begin match static_loc with
        | Some static_loc when not allow_static ->
          let key = Expression.Object.Property.Identifier (
            static_loc,
            "static"
          ) in
          let static = false in
          let property = method_property env start_loc static key in
          semicolon exact env;
          properties ~allow_static ~allow_spread ~exact env (property::acc)
        | _ ->
          let call_prop = call_property env start_loc static in
          semicolon exact env;
          properties ~allow_static ~allow_spread ~exact env (call_prop::acc)
        end
      | T_ELLIPSIS when allow_spread ->
        error_if_unexpected_static env ~allow_static static_loc;
        error_unsupported_variance env variance;
        let spread = with_loc ~start_loc (fun env ->
          Eat.token env;
          { Type.Object.SpreadProperty.
            argument = _type env;
          }
        ) env in
        let property = Type.Object.SpreadProperty spread in
        semicolon exact env;
        properties ~allow_static ~allow_spread ~exact env (property::acc)
      | token ->
        let property = match static_loc, variance, token with
        | Some static_loc, None, (T_PLING | T_COLON) ->
            (* We speculatively parsed `static` as a static modifier, but now
               that we've parsed the next token, we changed our minds and want
               to parse `static` as the key of a named property. *)
            let key = Expression.Object.Property.Identifier (
              static_loc,
              "static"
            ) in
            let static = false in
            property env start_loc static variance key
        | _ ->
            error_if_unexpected_static env ~allow_static static_loc;
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
      with_loc (fun env ->
        Expect.token env (if exact then T_LCURLYBAR else T_LCURLY);
        let properties = properties ~allow_static ~exact ~allow_spread env [] in
        Expect.token env (if exact then T_RCURLYBAR else T_RCURLY);
        { Type.Object.exact; properties; }
      ) env

  and type_identifier env =
    let loc, name = identifier_name env in
    if is_reserved_type name then error_at env (loc, Parse_error.UnexpectedReservedType);
    loc, name

  and bounded_type env = with_loc (fun env ->
    let name = type_identifier env in
    let bound = if Peek.token env = T_COLON then Some (annotation env) else None in
    name, bound
  ) env

  and type_parameter_declaration =
    let rec params env ~allow_default ~require_default acc = Type.ParameterDeclaration.TypeParam.(
      let variance = variance env in
      let loc, (name, bound) = bounded_type env in
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
        if Peek.token env = T_LESS_THAN
        then begin
          if not (should_parse_types env)
          then error env Error.UnexpectedTypeAnnotation;
          Some (with_loc (fun env ->
            Expect.token env T_LESS_THAN;
            let params = params env ~allow_default ~require_default:false [] in
            Expect.token env T_GREATER_THAN;
            params
          ) env)
        end else None

  and type_parameter_instantiation =
    let rec args env acc =
      match Peek.token env with
      | T_EOF
      | T_GREATER_THAN -> List.rev acc
      | _ ->
        let acc = (_type env)::acc in
        if Peek.token env <> T_GREATER_THAN
        then Expect.token env T_COMMA;
        args env acc

    in fun env ->
        if Peek.token env = T_LESS_THAN then
          Some (with_loc (fun env ->
            Expect.token env T_LESS_THAN;
            let args = args env [] in
            Expect.token env T_GREATER_THAN;
            args
          ) env)
        else None

  and generic env = raw_generic_with_identifier env (type_identifier env)

  and raw_generic_with_identifier =
    let rec identifier env (q_loc, qualification) =
      if Peek.token env = T_PERIOD
      then begin
        let loc, q = with_loc ~start_loc:q_loc (fun env ->
          Expect.token env T_PERIOD;
          let id = type_identifier env in
          { Type.Generic.Identifier.
            qualification;
            id;
          }
        ) env in
        let qualification = Type.Generic.Identifier.Qualified (loc, q) in
        identifier env (loc, qualification)
      end else (q_loc, qualification)

    in fun env id ->
      with_loc ~start_loc:(fst id) (fun env ->
        let id = fst id, Type.Generic.Identifier.Unqualified id in
        let _id_loc, id = identifier env id in
        let targs = type_parameter_instantiation env in
        { Type.Generic.id; targs }
      ) env

  and generic_type_with_identifier env id =
    let loc, generic = raw_generic_with_identifier env id in
    loc, Type.Generic generic

  and annotation_opt env =
    match Peek.token env with
    | T_COLON -> Some (annotation env)
    | _ -> None

  let predicate = with_loc (fun env ->
    Expect.token env T_CHECKS;
    if Peek.token env = T_LPAREN then begin
      Expect.token env T_LPAREN;
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let exp = Parse.conditional env in
      Eat.pop_lex_mode env;
      Expect.token env T_RPAREN;
      Ast.Type.Predicate.Declared exp
    end else
      Ast.Type.Predicate.Inferred
  )

  let predicate_opt env =
    let env = with_no_anon_function_type false env in
    match Peek.token env with
    | T_CHECKS -> Some (predicate env)
    | _ -> None

  let annotation_and_predicate_opt env =
    match Peek.token env, Peek.ith_token ~i:1 env with
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
  let type_identifier = wrap type_identifier
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
