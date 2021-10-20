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
open Parser_common
open Comment_attachment

module type TYPE = sig
  val _type : env -> (Loc.t, Loc.t) Ast.Type.t

  val type_identifier : env -> (Loc.t, Loc.t) Ast.Identifier.t

  val type_params : env -> (Loc.t, Loc.t) Ast.Type.TypeParams.t option

  val type_args : env -> (Loc.t, Loc.t) Ast.Type.TypeArgs.t option

  val generic : env -> Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t

  val _object : is_class:bool -> env -> Loc.t * (Loc.t, Loc.t) Type.Object.t

  val interface_helper :
    env ->
    (Loc.t * (Loc.t, Loc.t) Ast.Type.Generic.t) list * (Loc.t * (Loc.t, Loc.t) Ast.Type.Object.t)

  val function_param_list : env -> (Loc.t, Loc.t) Type.Function.Params.t

  val annotation : env -> (Loc.t, Loc.t) Ast.Type.annotation

  val annotation_opt : env -> (Loc.t, Loc.t) Ast.Type.annotation_or_hint

  val predicate_opt : env -> (Loc.t, Loc.t) Ast.Type.Predicate.t option

  val annotation_and_predicate_opt :
    env -> (Loc.t, Loc.t) Ast.Type.annotation_or_hint * (Loc.t, Loc.t) Ast.Type.Predicate.t option
end

module Type (Parse : Parser_common.PARSER) : TYPE = struct
  type param_list_or_type =
    | ParamList of (Loc.t, Loc.t) Type.Function.Params.t'
    | Type of (Loc.t, Loc.t) Type.t

  let maybe_variance env =
    let loc = Peek.loc env in
    match Peek.token env with
    | T_PLUS ->
      let leading = Peek.comments env in
      Eat.token env;
      Some
        ( loc,
          { Variance.kind = Variance.Plus; comments = Flow_ast_utils.mk_comments_opt ~leading () }
        )
    | T_MINUS ->
      let leading = Peek.comments env in
      Eat.token env;
      Some
        ( loc,
          { Variance.kind = Variance.Minus; comments = Flow_ast_utils.mk_comments_opt ~leading () }
        )
    | _ -> None

  let rec _type env = union env

  and annotation env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
    with_loc
      (fun env ->
        Expect.token env T_COLON;
        _type env)
      env

  and union env =
    let leading =
      if Peek.token env = T_BIT_OR then (
        let leading = Peek.comments env in
        Eat.token env;
        leading
      ) else
        []
    in
    let left = intersection env in
    union_with env ~leading left

  and union_with =
    let rec unions leading acc env =
      match Peek.token env with
      | T_BIT_OR ->
        Expect.token env T_BIT_OR;
        unions leading (intersection env :: acc) env
      | _ ->
        (match List.rev acc with
        | t0 :: t1 :: ts ->
          Type.Union
            {
              Type.Union.types = (t0, t1, ts);
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            }
        | _ -> assert false)
    in
    fun env ?(leading = []) left ->
      if Peek.token env = T_BIT_OR then
        with_loc ~start_loc:(fst left) (unions leading [left]) env
      else
        left

  and intersection env =
    let leading =
      if Peek.token env = T_BIT_AND then (
        let leading = Peek.comments env in
        Eat.token env;
        leading
      ) else
        []
    in
    let left = anon_function_without_parens env in
    intersection_with env ~leading left

  and intersection_with =
    let rec intersections leading acc env =
      match Peek.token env with
      | T_BIT_AND ->
        Expect.token env T_BIT_AND;
        intersections leading (anon_function_without_parens env :: acc) env
      | _ ->
        (match List.rev acc with
        | t0 :: t1 :: ts ->
          Type.Intersection
            {
              Type.Intersection.types = (t0, t1, ts);
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            }
        | _ -> assert false)
    in
    fun env ?(leading = []) left ->
      if Peek.token env = T_BIT_AND then
        with_loc ~start_loc:(fst left) (intersections leading [left]) env
      else
        left

  and anon_function_without_parens env =
    let param = prefix env in
    anon_function_without_parens_with env param

  and anon_function_without_parens_with env param =
    match Peek.token env with
    | T_ARROW when not (no_anon_function_type env) ->
      let (start_loc, tparams, params) =
        let param = anonymous_function_param env param in
        ( fst param,
          None,
          ( fst param,
            {
              Ast.Type.Function.Params.params = [param];
              this_ = None;
              rest = None;
              comments = None;
            }
          )
        )
      in
      function_with_params env start_loc tparams params
    | _ -> param

  and prefix env =
    match Peek.token env with
    | T_PLING ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token env T_PLING;
          Type.Nullable
            {
              Type.Nullable.argument = prefix env;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | _ -> postfix env

  and postfix env =
    let t = primary env in
    postfix_with env t

  and postfix_with ?(in_optional_indexed_access = false) env t =
    if Peek.is_line_terminator env then
      t
    else
      match Peek.token env with
      | T_PLING_PERIOD ->
        Eat.token env;
        if Peek.token env <> T_LBRACKET then error env Parse_error.InvalidOptionalIndexedAccess;
        Expect.token env T_LBRACKET;
        postfix_brackets ~in_optional_indexed_access:true ~optional_indexed_access:true env t
      | T_LBRACKET ->
        Eat.token env;
        postfix_brackets ~in_optional_indexed_access ~optional_indexed_access:false env t
      | T_PERIOD ->
        (match Peek.ith_token ~i:1 env with
        | T_LBRACKET ->
          error env (Parse_error.InvalidIndexedAccess { has_bracket = true });
          Expect.token env T_PERIOD;
          Expect.token env T_LBRACKET;
          postfix_brackets ~in_optional_indexed_access ~optional_indexed_access:false env t
        | _ ->
          error env (Parse_error.InvalidIndexedAccess { has_bracket = false });
          t)
      | _ -> t

  and postfix_brackets ~in_optional_indexed_access ~optional_indexed_access env t =
    let t =
      with_loc
        ~start_loc:(fst t)
        (fun env ->
          (* Legacy Array syntax `Foo[]` *)
          if (not optional_indexed_access) && Eat.maybe env T_RBRACKET then
            let trailing = Eat.trailing_comments env in
            Type.Array
              { Type.Array.argument = t; comments = Flow_ast_utils.mk_comments_opt ~trailing () }
          else
            let index = _type env in
            Expect.token env T_RBRACKET;
            let trailing = Eat.trailing_comments env in
            let indexed_access =
              {
                Type.IndexedAccess._object = t;
                index;
                comments = Flow_ast_utils.mk_comments_opt ~trailing ();
              }
            in
            if in_optional_indexed_access then
              Type.OptionalIndexedAccess
                { Type.OptionalIndexedAccess.indexed_access; optional = optional_indexed_access }
            else
              Type.IndexedAccess indexed_access)
        env
    in
    postfix_with env ~in_optional_indexed_access t

  and primary env =
    let loc = Peek.loc env in
    match Peek.token env with
    | T_MULT ->
      let leading = Peek.comments env in
      Expect.token env T_MULT;
      let trailing = Eat.trailing_comments env in
      (loc, Type.Exists (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_LESS_THAN -> _function env
    | T_LPAREN -> function_or_group env
    | T_LCURLY
    | T_LCURLYBAR ->
      let (loc, o) = _object env ~is_class:false ~allow_exact:true ~allow_spread:true in
      (loc, Type.Object o)
    | T_INTERFACE ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token env T_INTERFACE;
          let (extends, body) = interface_helper env in
          Type.Interface
            { Type.Interface.extends; body; comments = Flow_ast_utils.mk_comments_opt ~leading () })
        env
    | T_TYPEOF ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token env T_TYPEOF;
          Type.Typeof
            {
              Type.Typeof.argument = primary env;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | T_LBRACKET -> tuple env
    | T_IDENTIFIER _
    | T_STATIC (* `static` is reserved in strict mode, but still an identifier *) ->
      let (loc, g) = generic env in
      (loc, Type.Generic g)
    | T_STRING (loc, value, raw, octal) ->
      if octal then strict_error env Parse_error.StrictOctalLiteral;
      let leading = Peek.comments env in
      Expect.token env (T_STRING (loc, value, raw, octal));
      let trailing = Eat.trailing_comments env in
      ( loc,
        Type.StringLiteral
          {
            Ast.StringLiteral.value;
            raw;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
      )
    | T_NUMBER_SINGLETON_TYPE { kind; value; raw } ->
      let leading = Peek.comments env in
      Expect.token env (T_NUMBER_SINGLETON_TYPE { kind; value; raw });
      let trailing = Eat.trailing_comments env in
      if kind = LEGACY_OCTAL then strict_error env Parse_error.StrictOctalLiteral;
      ( loc,
        Type.NumberLiteral
          {
            Ast.NumberLiteral.value;
            raw;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
      )
    | T_BIGINT_SINGLETON_TYPE { kind; approx_value; raw } ->
      let bigint = raw in
      let leading = Peek.comments env in
      Expect.token env (T_BIGINT_SINGLETON_TYPE { kind; approx_value; raw });
      let trailing = Eat.trailing_comments env in
      ( loc,
        Type.BigIntLiteral
          {
            Ast.BigIntLiteral.approx_value;
            bigint;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
      )
    | (T_TRUE | T_FALSE) as token ->
      let leading = Peek.comments env in
      Expect.token env token;
      let trailing = Eat.trailing_comments env in
      let value = token = T_TRUE in
      ( loc,
        Type.BooleanLiteral
          { BooleanLiteral.value; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
      )
    | _ ->
      (match primitive env with
      | Some t -> (loc, t)
      | None ->
        error_unexpected env;
        (loc, Type.Any None))

  and is_primitive = function
    | T_ANY_TYPE
    | T_MIXED_TYPE
    | T_EMPTY_TYPE
    | T_BOOLEAN_TYPE _
    | T_NUMBER_TYPE
    | T_BIGINT_TYPE
    | T_STRING_TYPE
    | T_SYMBOL_TYPE
    | T_VOID_TYPE
    | T_NULL ->
      true
    | _ -> false

  and primitive env =
    let leading = Peek.comments env in
    let token = Peek.token env in
    match token with
    | T_ANY_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Any (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_MIXED_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Mixed (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_EMPTY_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Empty (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_BOOLEAN_TYPE _ ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Boolean (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_NUMBER_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Number (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_BIGINT_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.BigInt (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_STRING_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.String (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_SYMBOL_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Symbol (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_VOID_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Void (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_NULL ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Null (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | _ -> None

  and tuple =
    let rec types env acc =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET ->
        List.rev acc
      | _ ->
        let acc = _type env :: acc in
        (* Trailing comma support (like [number, string,]) *)
        if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
        types env acc
    in
    fun env ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token env T_LBRACKET;
          let tl = types (with_no_anon_function_type false env) [] in
          Expect.token env T_RBRACKET;
          let trailing = Eat.trailing_comments env in
          Type.Tuple
            {
              Type.Tuple.types = tl;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
        env

  and anonymous_function_param _env annot =
    (fst annot, Type.Function.Param.{ name = None; annot; optional = false })

  and function_param_with_id env =
    with_loc
      (fun env ->
        Eat.push_lex_mode env Lex_mode.NORMAL;
        let name = Parse.identifier env in
        Eat.pop_lex_mode env;
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
        let optional = Eat.maybe env T_PLING in
        Expect.token env T_COLON;
        let annot = _type env in
        { Type.Function.Param.name = Some name; annot; optional })
      env

  and function_param_list_without_parens =
    let param env =
      match Peek.ith_token ~i:1 env with
      | T_COLON
      | T_PLING ->
        function_param_with_id env
      | _ ->
        let annot = _type env in
        anonymous_function_param env annot
    in
    let rec param_list env this_ acc =
      match Peek.token env with
      | (T_EOF | T_ELLIPSIS | T_RPAREN) as t ->
        let rest =
          if t = T_ELLIPSIS then
            let rest =
              with_loc
                (fun env ->
                  let leading = Peek.comments env in
                  Expect.token env T_ELLIPSIS;
                  {
                    Type.Function.RestParam.argument = param env;
                    comments = Flow_ast_utils.mk_comments_opt ~leading ();
                  })
                env
            in
            Some rest
          else
            None
        in
        { Ast.Type.Function.Params.params = List.rev acc; rest; this_; comments = None }
      | T_IDENTIFIER { raw = "this"; _ }
        when Peek.ith_token ~i:1 env == T_COLON || Peek.ith_token ~i:1 env == T_PLING ->
        if this_ <> None || acc <> [] then error env Parse_error.ThisParamMustBeFirst;
        let this_ =
          with_loc
            (fun env ->
              let leading = Peek.comments env in
              Eat.token env;
              if Peek.token env == T_PLING then error env Parse_error.ThisParamMayNotBeOptional;
              {
                Type.Function.ThisParam.annot = annotation env;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              })
            env
        in
        if Peek.token env <> T_RPAREN then Expect.token env T_COMMA;
        param_list env (Some this_) acc
      | _ ->
        let acc = param env :: acc in
        if Peek.token env <> T_RPAREN then Expect.token env T_COMMA;
        param_list env this_ acc
    in
    (fun env -> param_list env None)

  and function_param_list env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_LPAREN;
        let params = function_param_list_without_parens env [] in
        let internal = Peek.comments env in
        Expect.token env T_RPAREN;
        let trailing = Eat.trailing_comments env in
        {
          params with
          Ast.Type.Function.Params.comments =
            Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
        })
      env

  and param_list_or_type env =
    let leading = Peek.comments env in
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
        ParamList
          { Ast.Type.Function.Params.this_ = None; params = []; rest = None; comments = None }
      | T_IDENTIFIER _
      | T_STATIC (* `static` is reserved in strict mode, but still an identifier *) ->
        (* This could be a function parameter or a generic type *)
        function_param_or_generic_type env
      | token when is_primitive token ->
        (* Don't know if this is (number) or (number: number). The first
         * is a type, the second is a param. *)
        (match Peek.ith_token ~i:1 env with
        | T_PLING
        | T_COLON ->
          (* Ok this is definitely a parameter *)
          ParamList (function_param_list_without_parens env [])
        | _ -> Type (_type env))
      | _ ->
        (* All params start with an identifier or `...` *)
        Type (_type env)
    in
    (* Now that we allow anonymous parameters in function types, we need to
     * disambiguate a little bit more *)
    let ret =
      match ret with
      | ParamList _ -> ret
      | Type _ when no_anon_function_type env -> ret
      | Type t ->
        (match Peek.token env with
        | T_RPAREN ->
          (* Reinterpret `(type) =>` as a ParamList *)
          if Peek.ith_token ~i:1 env = T_ARROW then
            let param = anonymous_function_param env t in
            ParamList (function_param_list_without_parens env [param])
          else
            Type t
        | T_COMMA ->
          (* Reinterpret `(type,` as a ParamList *)
          Expect.token env T_COMMA;
          let param = anonymous_function_param env t in
          ParamList (function_param_list_without_parens env [param])
        | _ -> ret)
    in
    let internal = Peek.comments env in
    Expect.token env T_RPAREN;
    let trailing = Eat.trailing_comments env in
    let ret =
      match ret with
      | ParamList params ->
        ParamList
          {
            params with
            Ast.Type.Function.Params.comments =
              Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
          }
      | Type t -> Type (add_comments t leading trailing)
    in
    ret

  and function_param_or_generic_type env =
    match Peek.ith_token ~i:1 env with
    | T_PLING
    (* optional param *)
    | T_COLON ->
      ParamList (function_param_list_without_parens env [])
    | _ ->
      let id = type_identifier env in
      Type
        (generic_type_with_identifier env id
        |> postfix_with env
        |> anon_function_without_parens_with env
        |> intersection_with env
        |> union_with env
        )

  and function_or_group env =
    let start_loc = Peek.loc env in
    match with_loc param_list_or_type env with
    | (loc, ParamList params) -> function_with_params env start_loc None (loc, params)
    | (_, Type _type) -> _type

  and _function env =
    let start_loc = Peek.loc env in
    let tparams = type_params_remove_trailing env (type_params env) in
    let params = function_param_list env in
    function_with_params env start_loc tparams params

  and function_with_params env start_loc tparams (params : (Loc.t, Loc.t) Ast.Type.Function.Params.t)
      =
    with_loc
      ~start_loc
      (fun env ->
        Expect.token env T_ARROW;
        let return = _type env in
        Type.(Function { Function.params; return; tparams; comments = None }))
      env

  and _object =
    let methodish env start_loc tparams =
      with_loc
        ~start_loc
        (fun env ->
          let params = function_param_list env in
          Expect.token env T_COLON;
          let return = _type env in
          { Type.Function.params; return; tparams; comments = None })
        env
    in
    let method_property env start_loc static key ~leading =
      let key = object_key_remove_trailing env key in
      let tparams = type_params_remove_trailing env (type_params env) in
      let value = methodish env start_loc tparams in
      let value = (fst value, Type.Function (snd value)) in
      Type.Object.(
        Property
          ( fst value,
            {
              Property.key;
              value = Property.Init value;
              optional = false;
              static = static <> None;
              proto = false;
              _method = true;
              variance = None;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            }
          )
      )
    in
    let call_property env start_loc static ~leading =
      let prop =
        with_loc
          ~start_loc
          (fun env ->
            let start_loc = Peek.loc env in
            let tparams = type_params_remove_trailing env (type_params env) in
            let value = methodish env start_loc tparams in
            Type.Object.CallProperty.
              {
                value;
                static = static <> None;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              }
            )
          env
      in
      Type.Object.CallProperty prop
    in
    let init_property env start_loc ~variance ~static ~proto ~leading key =
      ignore proto;
      if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
      let prop =
        with_loc
          ~start_loc
          (fun env ->
            let optional = Eat.maybe env T_PLING in
            Expect.token env T_COLON;
            let value = _type env in
            Type.Object.Property.
              {
                key;
                value = Init value;
                optional;
                static = static <> None;
                proto = proto <> None;
                _method = false;
                variance;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              }
            )
          env
      in
      Type.Object.Property prop
    in
    let getter_or_setter ~is_getter ~leading env start_loc static key =
      let prop =
        with_loc
          ~start_loc
          (fun env ->
            let (key_loc, key) = key in
            let key = object_key_remove_trailing env key in
            let value = methodish env start_loc None in
            let (_, { Type.Function.params; _ }) = value in
            begin
              match (is_getter, params) with
              | (true, (_, { Type.Function.Params.this_ = Some _; _ })) ->
                error_at env (key_loc, Parse_error.GetterMayNotHaveThisParam)
              | (false, (_, { Type.Function.Params.this_ = Some _; _ })) ->
                error_at env (key_loc, Parse_error.SetterMayNotHaveThisParam)
              | ( true,
                  (_, { Type.Function.Params.params = []; rest = None; this_ = None; comments = _ })
                ) ->
                ()
              | (false, (_, { Type.Function.Params.rest = Some _; _ })) ->
                (* rest params don't make sense on a setter *)
                error_at env (key_loc, Parse_error.SetterArity)
              | (false, (_, { Type.Function.Params.params = [_]; _ })) -> ()
              | (true, _) -> error_at env (key_loc, Parse_error.GetterArity)
              | (false, _) -> error_at env (key_loc, Parse_error.SetterArity)
            end;
            Type.Object.Property.
              {
                key;
                value =
                  ( if is_getter then
                    Get value
                  else
                    Set value
                  );
                optional = false;
                static = static <> None;
                proto = false;
                _method = false;
                variance = None;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              }
            )
          env
      in
      Type.Object.Property prop
    in
    let indexer_property env start_loc static variance ~leading =
      let indexer =
        with_loc
          ~start_loc
          (fun env ->
            let leading = leading @ Peek.comments env in
            Expect.token env T_LBRACKET;
            let id =
              if Peek.ith_token ~i:1 env = T_COLON then (
                let id = identifier_name env in
                Expect.token env T_COLON;
                Some id
              ) else
                None
            in
            let key = _type env in
            Expect.token env T_RBRACKET;
            let trailing = Eat.trailing_comments env in
            Expect.token env T_COLON;
            let value = _type env in
            {
              Type.Object.Indexer.id;
              key;
              value;
              static = static <> None;
              variance;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
          env
      in
      Type.Object.Indexer indexer
    in
    let internal_slot env start_loc static ~leading =
      let islot =
        with_loc
          ~start_loc
          (fun env ->
            let leading = leading @ Peek.comments env in
            Expect.token env T_LBRACKET;
            Expect.token env T_LBRACKET;
            let id = identifier_name env in
            Expect.token env T_RBRACKET;
            Expect.token env T_RBRACKET;
            let (optional, _method, value, trailing) =
              match Peek.token env with
              | T_LESS_THAN
              | T_LPAREN ->
                let tparams = type_params_remove_trailing env (type_params env) in
                let value =
                  let (fn_loc, fn) = methodish env start_loc tparams in
                  (fn_loc, Type.Function fn)
                in
                (false, true, value, [])
              | _ ->
                let optional = Eat.maybe env T_PLING in
                let trailing = Eat.trailing_comments env in
                Expect.token env T_COLON;
                let value = _type env in
                (optional, false, value, trailing)
            in
            {
              Type.Object.InternalSlot.id;
              value;
              optional;
              static = static <> None;
              _method;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
          env
      in
      Type.Object.InternalSlot islot
      (* Expects the T_ELLIPSIS has already been eaten *)
    in
    let spread_property env start_loc ~leading =
      let spread =
        with_loc
          ~start_loc
          (fun env ->
            {
              Type.Object.SpreadProperty.argument = _type env;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
          env
      in
      Type.Object.SpreadProperty spread
    in
    let semicolon exact env =
      match Peek.token env with
      | T_COMMA
      | T_SEMICOLON ->
        Eat.token env
      | T_RCURLYBAR when exact -> ()
      | T_RCURLY when not exact -> ()
      | _ -> error_unexpected env
    in
    let error_unexpected_variance env = function
      | Some (loc, _) -> error_at env (loc, Parse_error.UnexpectedVariance)
      | None -> ()
    in
    let error_unexpected_proto env = function
      | Some loc -> error_at env (loc, Parse_error.UnexpectedProto)
      | None -> ()
    in
    let error_invalid_property_name env is_class static key =
      let is_static = static <> None in
      let is_constructor = String.equal "constructor" in
      let is_prototype = String.equal "prototype" in
      match key with
      | Expression.Object.Property.Identifier (loc, { Identifier.name; comments = _ })
        when is_class && (is_constructor name || (is_static && is_prototype name)) ->
        error_at
          env
          ( loc,
            Parse_error.InvalidClassMemberName
              { name; static = is_static; method_ = false; private_ = false }
          )
      | _ -> ()
    in
    let rec properties
        ~is_class ~allow_inexact ~allow_spread ~exact env ((props, inexact, internal) as acc) =
      (* no `static ...A` *)
      assert (not (is_class && allow_spread));

      (* allow_inexact implies allow_spread *)
      assert ((not allow_inexact) || allow_spread);

      let start_loc = Peek.loc env in
      match Peek.token env with
      | T_EOF -> (List.rev props, inexact, internal)
      | T_RCURLYBAR when exact -> (List.rev props, inexact, internal)
      | T_RCURLY when not exact -> (List.rev props, inexact, internal)
      | T_ELLIPSIS when allow_spread ->
        let leading = Peek.comments env in
        Eat.token env;
        begin
          match Peek.token env with
          | T_COMMA
          | T_SEMICOLON
          | T_RCURLY
          | T_RCURLYBAR ->
            semicolon exact env;
            begin
              match Peek.token env with
              | T_RCURLY when allow_inexact -> (List.rev props, true, leading)
              | T_RCURLYBAR ->
                error_at env (start_loc, Parse_error.InexactInsideExact);
                (List.rev props, inexact, internal)
              | _ ->
                error_at env (start_loc, Parse_error.UnexpectedExplicitInexactInObject);
                properties ~is_class ~allow_inexact ~allow_spread ~exact env acc
            end
          | _ ->
            let prop = spread_property env start_loc leading in
            semicolon exact env;
            properties
              ~is_class
              ~allow_inexact
              ~allow_spread
              ~exact
              env
              (prop :: props, inexact, internal)
        end
      (* In this case, allow_spread is false, so we may assume allow_inexact is false based on our
       * assertion at the top of this function. Thus, any T_ELLIPSIS here is not allowed.
       *)
      | T_ELLIPSIS ->
        Eat.token env;
        begin
          match Peek.token env with
          | T_COMMA
          | T_SEMICOLON
          | T_RCURLY
          | T_RCURLYBAR ->
            error_at env (start_loc, Parse_error.InexactInsideNonObject);
            semicolon exact env;
            properties ~is_class ~allow_inexact ~allow_spread ~exact env acc
          | _ ->
            error_list env (Peek.errors env);
            error_at env (start_loc, Parse_error.UnexpectedSpreadType);

            (* It's likely the user is trying to spread something here, so we can
             * eat what they try to spread to try to continue parsing the remaining
             * properties.
             *)
            Eat.token env;
            semicolon exact env;
            properties ~is_class ~allow_inexact ~allow_spread ~exact env acc
        end
      | _ ->
        let prop =
          property
            env
            start_loc
            ~is_class
            ~allow_static:is_class
            ~allow_proto:is_class
            ~variance:None
            ~static:None
            ~proto:None
            ~leading:[]
        in
        semicolon exact env;
        properties
          ~is_class
          ~allow_inexact
          ~allow_spread
          ~exact
          env
          (prop :: props, inexact, internal)
    and property
        env ~is_class ~allow_static ~allow_proto ~variance ~static ~proto ~leading start_loc =
      match Peek.token env with
      | T_PLUS
      | T_MINUS
        when variance = None ->
        let variance = maybe_variance env in
        property
          env
          ~is_class
          ~allow_static:false
          ~allow_proto:false
          ~variance
          ~static
          ~proto
          ~leading
          start_loc
      | T_STATIC when allow_static ->
        assert (variance = None);

        (* if we parsed variance, allow_static = false *)
        let static = Some (Peek.loc env) in
        let leading = leading @ Peek.comments env in
        Eat.token env;
        property
          env
          ~is_class
          ~allow_static:false
          ~allow_proto:false
          ~variance
          ~static
          ~proto
          ~leading
          start_loc
      | T_IDENTIFIER { raw = "proto"; _ } when allow_proto ->
        assert (variance = None);

        (* if we parsed variance, allow_proto = false *)
        let proto = Some (Peek.loc env) in
        let leading = leading @ Peek.comments env in
        Eat.token env;
        property
          env
          ~is_class
          ~allow_static:false
          ~allow_proto:false
          ~variance
          ~static
          ~proto
          ~leading
          start_loc
      | T_LBRACKET ->
        error_unexpected_proto env proto;
        (match Peek.ith_token ~i:1 env with
        | T_LBRACKET ->
          error_unexpected_variance env variance;
          internal_slot env start_loc static ~leading
        | _ -> indexer_property env start_loc static variance ~leading)
      | T_LESS_THAN
      | T_LPAREN ->
        (* Note that `static(): void` is a static callable property if we
           successfully parsed the static modifier above. *)
        error_unexpected_proto env proto;
        error_unexpected_variance env variance;
        call_property env start_loc static ~leading
      | token ->
        (match (static, proto, token) with
        | (Some _, Some _, _) -> failwith "Can not have both `static` and `proto`"
        | (Some static_loc, None, (T_PLING | T_COLON)) ->
          (* We speculatively parsed `static` as a static modifier, but now
             that we've parsed the next token, we changed our minds and want
             to parse `static` as the key of a named property. *)
          let key =
            Expression.Object.Property.Identifier
              (Flow_ast_utils.ident_of_source
                 (static_loc, "static")
                 ?comments:(Flow_ast_utils.mk_comments_opt ~leading ())
              )
          in
          let static = None in
          init_property env start_loc ~variance ~static ~proto ~leading:[] key
        | (None, Some proto_loc, (T_PLING | T_COLON)) ->
          (* We speculatively parsed `proto` as a proto modifier, but now
             that we've parsed the next token, we changed our minds and want
             to parse `proto` as the key of a named property. *)
          let key =
            Expression.Object.Property.Identifier
              (Flow_ast_utils.ident_of_source
                 (proto_loc, "proto")
                 ?comments:(Flow_ast_utils.mk_comments_opt ~leading ())
              )
          in
          let proto = None in
          init_property env start_loc ~variance ~static ~proto ~leading:[] key
        | _ ->
          let object_key env =
            Eat.push_lex_mode env Lex_mode.NORMAL;
            let result = Parse.object_key env in
            Eat.pop_lex_mode env;
            result
          in
          let leading_key = Peek.comments env in
          (match object_key env with
          | ( _,
              ( Expression.Object.Property.Identifier
                  (_, { Identifier.name = ("get" | "set") as name; comments = _ }) as key
              )
            ) ->
            begin
              match Peek.token env with
              | T_LESS_THAN
              | T_LPAREN ->
                error_unexpected_proto env proto;
                error_unexpected_variance env variance;
                method_property env start_loc static key leading
              | T_COLON
              | T_PLING ->
                init_property env start_loc ~variance ~static ~proto ~leading key
              | _ ->
                ignore (object_key_remove_trailing env key);
                let key = object_key env in
                let is_getter = name = "get" in
                let leading = leading @ leading_key in
                error_unexpected_proto env proto;
                error_unexpected_variance env variance;
                getter_or_setter ~is_getter ~leading env start_loc static key
            end
          | (_, key) ->
            begin
              match Peek.token env with
              | T_LESS_THAN
              | T_LPAREN ->
                error_unexpected_proto env proto;
                error_unexpected_variance env variance;
                method_property env start_loc static key leading
              | _ ->
                error_invalid_property_name env is_class static key;
                init_property env start_loc ~variance ~static ~proto ~leading key
            end))
    in
    fun ~is_class ~allow_exact ~allow_spread env ->
      let exact = allow_exact && Peek.token env = T_LCURLYBAR in
      let allow_inexact = allow_exact && not exact in
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token
            env
            ( if exact then
              T_LCURLYBAR
            else
              T_LCURLY
            );
          let (properties, inexact, internal) =
            let env = with_no_anon_function_type false env in
            properties ~is_class ~allow_inexact ~exact ~allow_spread env ([], false, [])
          in
          let internal = internal @ Peek.comments env in
          Expect.token
            env
            ( if exact then
              T_RCURLYBAR
            else
              T_RCURLY
            );
          let trailing = Eat.trailing_comments env in

          (* inexact = true iff `...` was used to indicate inexactnes *)
          {
            Type.Object.exact;
            properties;
            inexact;
            comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
          })
        env

  and interface_helper =
    let rec supers env acc =
      let super = generic env in
      let acc = super :: acc in
      match Peek.token env with
      | T_COMMA ->
        Expect.token env T_COMMA;
        supers env acc
      | _ -> List.rev acc
    in
    fun env ->
      let extends =
        if Peek.token env = T_EXTENDS then (
          Expect.token env T_EXTENDS;
          let extends = supers env [] in
          generic_type_list_remove_trailing env extends
        ) else
          []
      in
      let body = _object env ~allow_exact:false ~allow_spread:false ~is_class:false in
      (extends, body)

  and type_identifier env =
    let (loc, { Identifier.name; comments }) = identifier_name env in
    if is_reserved_type name then error_at env (loc, Parse_error.UnexpectedReservedType);
    (loc, { Identifier.name; comments })

  and bounded_type env =
    with_loc
      (fun env ->
        let name = type_identifier env in
        let bound =
          if Peek.token env = T_COLON then
            Ast.Type.Available (annotation env)
          else
            Ast.Type.Missing (Peek.loc_skip_lookahead env)
        in
        (name, bound))
      env

  and type_params =
    let rec params env ~require_default acc =
      Type.TypeParam.(
        let (loc, (variance, name, bound, default, require_default)) =
          with_loc
            (fun env ->
              let variance = maybe_variance env in
              let (loc, (name, bound)) = bounded_type env in
              let (default, require_default) =
                match Peek.token env with
                | T_ASSIGN ->
                  Eat.token env;
                  (Some (_type env), true)
                | _ ->
                  if require_default then error_at env (loc, Parse_error.MissingTypeParamDefault);
                  (None, require_default)
              in
              (variance, name, bound, default, require_default))
            env
        in
        let param = (loc, { name; bound; variance; default }) in
        let acc = param :: acc in
        match Peek.token env with
        | T_EOF
        | T_GREATER_THAN ->
          List.rev acc
        | _ ->
          Expect.token env T_COMMA;
          if Peek.token env = T_GREATER_THAN then
            List.rev acc
          else
            params env ~require_default acc
      )
    in
    fun env ->
      if Peek.token env = T_LESS_THAN then (
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
        Some
          (with_loc
             (fun env ->
               let leading = Peek.comments env in
               Expect.token env T_LESS_THAN;
               let params = params env ~require_default:false [] in
               let internal = Peek.comments env in
               Expect.token env T_GREATER_THAN;
               let trailing = Eat.trailing_comments env in
               {
                 Type.TypeParams.params;
                 comments =
                   Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
               })
             env
          )
      ) else
        None

  and type_args =
    let rec args env acc =
      match Peek.token env with
      | T_EOF
      | T_GREATER_THAN ->
        List.rev acc
      | _ ->
        let acc = _type env :: acc in
        if Peek.token env <> T_GREATER_THAN then Expect.token env T_COMMA;
        args env acc
    in
    fun env ->
      if Peek.token env = T_LESS_THAN then
        Some
          (with_loc
             (fun env ->
               let leading = Peek.comments env in
               Expect.token env T_LESS_THAN;
               let env = with_no_anon_function_type false env in
               let arguments = args env [] in
               let internal = Peek.comments env in
               Expect.token env T_GREATER_THAN;
               let trailing = Eat.trailing_comments env in
               {
                 Type.TypeArgs.arguments;
                 comments =
                   Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
               })
             env
          )
      else
        None

  and generic env = raw_generic_with_identifier env (type_identifier env)

  and raw_generic_with_identifier =
    let rec identifier env (q_loc, qualification) =
      if Peek.token env = T_PERIOD && Peek.ith_is_type_identifier ~i:1 env then
        let (loc, q) =
          with_loc
            ~start_loc:q_loc
            (fun env ->
              Expect.token env T_PERIOD;
              let id = type_identifier env in
              { Type.Generic.Identifier.qualification; id })
            env
        in
        let qualification = Type.Generic.Identifier.Qualified (loc, q) in
        identifier env (loc, qualification)
      else
        (q_loc, qualification)
    in
    fun env id ->
      with_loc
        ~start_loc:(fst id)
        (fun env ->
          let id = (fst id, Type.Generic.Identifier.Unqualified id) in
          let id =
            let (_id_loc, id) = identifier env id in
            if Peek.token env <> T_LESS_THAN then
              id
            else
              let { remove_trailing; _ } = trailing_and_remover env in
              remove_trailing id (fun remover id -> remover#generic_identifier_type id)
          in
          let targs = type_args env in
          { Type.Generic.id; targs; comments = None })
        env

  and generic_type_with_identifier env id =
    let (loc, generic) = raw_generic_with_identifier env id in
    (loc, Type.Generic generic)

  and annotation_opt env =
    match Peek.token env with
    | T_COLON -> Type.Available (annotation env)
    | _ -> Type.Missing (Peek.loc_skip_lookahead env)

  and add_comments (loc, t) leading trailing =
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
    let open Ast.Type in
    ( loc,
      match t with
      | Any comments -> Any (merge_comments comments)
      | Mixed comments -> Mixed (merge_comments comments)
      | Empty comments -> Empty (merge_comments comments)
      | Void comments -> Void (merge_comments comments)
      | Null comments -> Null (merge_comments comments)
      | Number comments -> Number (merge_comments comments)
      | BigInt comments -> BigInt (merge_comments comments)
      | String comments -> String (merge_comments comments)
      | Boolean comments -> Boolean (merge_comments comments)
      | Symbol comments -> Symbol (merge_comments comments)
      | Exists comments -> Exists (merge_comments comments)
      | Nullable ({ Nullable.comments; _ } as t) ->
        Nullable { t with Nullable.comments = merge_comments comments }
      | Function ({ Function.comments; _ } as t) ->
        Function { t with Function.comments = merge_comments comments }
      | Object ({ Object.comments; _ } as t) ->
        Object { t with Object.comments = merge_comments_with_internal comments }
      | Interface ({ Interface.comments; _ } as t) ->
        Interface { t with Interface.comments = merge_comments comments }
      | Array ({ Array.comments; _ } as t) ->
        Array { t with Array.comments = merge_comments comments }
      | Generic ({ Generic.comments; _ } as t) ->
        Generic { t with Generic.comments = merge_comments comments }
      | IndexedAccess ({ IndexedAccess.comments; _ } as t) ->
        IndexedAccess { t with IndexedAccess.comments = merge_comments comments }
      | OptionalIndexedAccess
          {
            OptionalIndexedAccess.indexed_access = { IndexedAccess.comments; _ } as indexed_access;
            optional;
          } ->
        OptionalIndexedAccess
          {
            OptionalIndexedAccess.indexed_access =
              { indexed_access with IndexedAccess.comments = merge_comments comments };
            optional;
          }
      | Union ({ Union.comments; _ } as t) ->
        Union { t with Union.comments = merge_comments comments }
      | Intersection ({ Intersection.comments; _ } as t) ->
        Intersection { t with Intersection.comments = merge_comments comments }
      | Typeof ({ Typeof.comments; _ } as t) ->
        Typeof { t with Typeof.comments = merge_comments comments }
      | Tuple ({ Tuple.comments; _ } as t) ->
        Tuple { t with Tuple.comments = merge_comments comments }
      | StringLiteral ({ StringLiteral.comments; _ } as t) ->
        StringLiteral { t with StringLiteral.comments = merge_comments comments }
      | NumberLiteral ({ NumberLiteral.comments; _ } as t) ->
        NumberLiteral { t with NumberLiteral.comments = merge_comments comments }
      | BigIntLiteral ({ BigIntLiteral.comments; _ } as t) ->
        BigIntLiteral { t with BigIntLiteral.comments = merge_comments comments }
      | BooleanLiteral ({ BooleanLiteral.comments; _ } as t) ->
        BooleanLiteral { t with BooleanLiteral.comments = merge_comments comments }
    )

  let predicate =
    with_loc (fun env ->
        let open Ast.Type.Predicate in
        let leading = Peek.comments env in
        Expect.token env T_CHECKS;
        if Peek.token env = T_LPAREN then (
          let leading = leading @ Peek.comments env in
          Expect.token env T_LPAREN;
          Eat.push_lex_mode env Lex_mode.NORMAL;
          let exp = Parse.conditional env in
          Eat.pop_lex_mode env;
          Expect.token env T_RPAREN;
          let trailing = Eat.trailing_comments env in
          { kind = Declared exp; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
        ) else
          let trailing = Eat.trailing_comments env in
          {
            kind = Ast.Type.Predicate.Inferred;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
    )

  let predicate_opt env =
    let env = with_no_anon_function_type false env in
    match Peek.token env with
    | T_CHECKS -> Some (predicate env)
    | _ -> None

  let annotation_and_predicate_opt env =
    let open Ast.Type in
    match (Peek.token env, Peek.ith_token ~i:1 env) with
    | (T_COLON, T_CHECKS) ->
      Expect.token env T_COLON;
      (Missing (Peek.loc_skip_lookahead env), predicate_opt env)
    | (T_COLON, _) ->
      let annotation =
        let annotation = annotation_opt env in
        if Peek.token env = T_CHECKS then
          type_annotation_hint_remove_trailing env annotation
        else
          annotation
      in
      let predicate = predicate_opt env in
      (annotation, predicate)
    | _ -> (Missing (Peek.loc_skip_lookahead env), None)

  let wrap f env =
    let env = env |> with_strict true in
    Eat.push_lex_mode env Lex_mode.TYPE;
    let ret = f env in
    Eat.pop_lex_mode env;
    ret

  let _type = wrap _type

  let type_identifier = wrap type_identifier

  let type_params = wrap type_params

  let type_args = wrap type_args

  let _object ~is_class env = wrap (_object ~is_class ~allow_exact:false ~allow_spread:false) env

  let interface_helper = wrap interface_helper

  let function_param_list = wrap function_param_list

  let annotation = wrap annotation

  let annotation_opt = wrap annotation_opt

  let predicate_opt = wrap predicate_opt

  let annotation_and_predicate_opt = wrap annotation_and_predicate_opt

  let generic = wrap generic
end
