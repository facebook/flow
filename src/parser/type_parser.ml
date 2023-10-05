(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

  val component_param_list : env -> (Loc.t, Loc.t) Ast.Type.Component.Params.t

  val annotation : env -> (Loc.t, Loc.t) Ast.Type.annotation

  val annotation_opt : env -> (Loc.t, Loc.t) Ast.Type.annotation_or_hint

  val renders_annotation_opt : env -> (Loc.t, Loc.t) Ast.Type.component_renders_annotation

  val function_return_annotation_opt : env -> (Loc.t, Loc.t) Ast.Function.ReturnAnnot.t

  val predicate_opt : env -> (Loc.t, Loc.t) Ast.Type.Predicate.t option

  val function_return_annotation_and_predicate_opt :
    env -> (Loc.t, Loc.t) Ast.Function.ReturnAnnot.t * (Loc.t, Loc.t) Ast.Type.Predicate.t option

  val type_guard : env -> (Loc.t, Loc.t) Ast.Type.TypeGuard.t
end

module Type (Parse : Parser_common.PARSER) : TYPE = struct
  type param_list_or_type =
    | ParamList of (Loc.t, Loc.t) Type.Function.Params.t'
    | Type of (Loc.t, Loc.t) Type.t

  let maybe_variance ?(parse_readonly = false) ?(parse_in_out = false) env =
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
    | T_READONLY when parse_readonly ->
      let leading = Peek.comments env in
      Eat.token env;
      Some
        ( loc,
          {
            Variance.kind = Variance.Readonly;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          }
        )
    | T_IDENTIFIER { raw = "in"; _ } when parse_in_out && Peek.ith_is_type_identifier ~i:1 env ->
      let leading = Peek.comments env in
      Eat.token env;
      let (kind, loc) =
        match Peek.token env with
        | T_IDENTIFIER { raw = "out"; _ } ->
          let end_loc = Peek.loc env in
          Eat.token env;
          (Variance.InOut, Loc.btwn loc end_loc)
        | _ -> (Variance.In, loc)
      in
      Some (loc, { Variance.kind; comments = Flow_ast_utils.mk_comments_opt ~leading () })
    | T_IDENTIFIER { raw = "out"; _ } when parse_in_out && Peek.ith_is_type_identifier ~i:1 env ->
      let leading = Peek.comments env in
      Eat.token env;
      Some
        ( loc,
          { Variance.kind = Variance.Out; comments = Flow_ast_utils.mk_comments_opt ~leading () }
        )
    | _ -> None

  let number_singleton ~neg kind value raw env =
    if kind = LEGACY_OCTAL then strict_error env Parse_error.StrictOctalLiteral;
    let leading = Peek.comments env in
    Eat.token env;
    let trailing = Eat.trailing_comments env in
    let (value, raw, comments) =
      match neg with
      | None ->
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        (value, raw, comments)
      | Some leading_neg ->
        let leading = leading_neg @ leading in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        (~-.value, "-" ^ raw, comments)
    in
    Type.NumberLiteral { Ast.NumberLiteral.value; raw; comments }

  let bigint_singleton ~neg value raw env =
    let leading = Peek.comments env in
    Eat.token env;
    let trailing = Eat.trailing_comments env in
    let (value, raw, comments) =
      match neg with
      | None ->
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        (value, raw, comments)
      | Some leading_neg ->
        let leading = leading_neg @ leading in
        let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
        (Option.map Int64.neg value, "-" ^ raw, comments)
    in
    Type.BigIntLiteral { Ast.BigIntLiteral.value; raw; comments }

  let rec _type env = conditional env

  and annotation env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
    with_loc
      (fun env ->
        Expect.token env T_COLON;
        _type env)
      env

  and function_return_annotation env =
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
    let start_loc = Peek.loc env in
    Expect.token env T_COLON;
    if is_start_of_type_guard env then
      Function.ReturnAnnot.TypeGuard (type_guard_annotation env ~start_loc)
    else
      Function.ReturnAnnot.Available (with_loc ~start_loc _type env)

  and conditional env =
    let start_loc = Peek.loc env in
    let env = Parser_env.with_no_conditional_type false env in
    let check_type = union env in
    conditional_with env ~start_loc check_type

  and conditional_with env ~start_loc check_type =
    match Peek.token env with
    | T_EXTENDS ->
      with_loc
        ~start_loc
        (fun env ->
          Expect.token env T_EXTENDS;
          let extends_type = union (Parser_env.with_no_conditional_type true env) in
          Expect.token_opt env T_PLING;
          let true_type = _type env in
          Expect.token_opt env T_COLON;
          let false_type = _type env in
          let trailing = Eat.trailing_comments env in
          Type.Conditional
            {
              Type.Conditional.check_type;
              extends_type;
              true_type;
              false_type;
              comments = Flow_ast_utils.mk_comments_opt ~trailing ();
            })
        env
    | _ -> check_type

  and union env =
    let start_loc = Peek.loc env in
    let leading =
      if Peek.token env = T_BIT_OR then (
        let leading = Peek.comments env in
        Eat.token env;
        leading
      ) else
        []
    in
    let left = intersection env in
    union_with env ~leading ~start_loc left

  and union_with =
    let rec unions leading acc env =
      if Eat.maybe env T_BIT_OR then
        unions leading (intersection env :: acc) env
      else
        match List.rev acc with
        | t0 :: t1 :: ts ->
          Type.Union
            {
              Type.Union.types = (t0, t1, ts);
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            }
        | _ -> assert false
    in
    fun env ?(leading = []) ~start_loc left ->
      if Peek.token env = T_BIT_OR then
        with_loc ~start_loc (unions leading [left]) env
      else
        left

  and intersection env =
    let start_loc = Peek.loc env in
    let leading =
      if Peek.token env = T_BIT_AND then (
        let leading = Peek.comments env in
        Eat.token env;
        leading
      ) else
        []
    in
    let left = anon_function_without_parens env in
    intersection_with env ~leading ~start_loc left

  and intersection_with =
    let rec intersections leading acc env =
      if Eat.maybe env T_BIT_AND then
        intersections leading (anon_function_without_parens env :: acc) env
      else
        match List.rev acc with
        | t0 :: t1 :: ts ->
          Type.Intersection
            {
              Type.Intersection.types = (t0, t1, ts);
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            }
        | _ -> assert false
    in
    fun env ?(leading = []) ~start_loc left ->
      if Peek.token env = T_BIT_AND then
        with_loc ~start_loc (intersections leading [left]) env
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
    let start_loc = Peek.loc env in
    let t = primary env in
    postfix_with env ~start_loc t

  and postfix_with ?(in_optional_indexed_access = false) env ~start_loc t =
    if Peek.is_line_terminator env then
      t
    else
      match Peek.token env with
      | T_PLING_PERIOD ->
        Eat.token env;
        if Peek.token env <> T_LBRACKET then error env Parse_error.InvalidOptionalIndexedAccess;
        Expect.token env T_LBRACKET;
        postfix_brackets
          ~in_optional_indexed_access:true
          ~optional_indexed_access:true
          env
          start_loc
          t
      | T_LBRACKET ->
        Eat.token env;
        postfix_brackets ~in_optional_indexed_access ~optional_indexed_access:false env start_loc t
      | T_PERIOD ->
        (match Peek.ith_token ~i:1 env with
        | T_LBRACKET ->
          error env (Parse_error.InvalidIndexedAccess { has_bracket = true });
          Expect.token env T_PERIOD;
          Expect.token env T_LBRACKET;
          postfix_brackets
            ~in_optional_indexed_access
            ~optional_indexed_access:false
            env
            start_loc
            t
        | _ ->
          error env (Parse_error.InvalidIndexedAccess { has_bracket = false });
          t)
      | _ -> t

  and postfix_brackets ~in_optional_indexed_access ~optional_indexed_access env start_loc t =
    let t =
      with_loc
        ~start_loc
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
    postfix_with env ~in_optional_indexed_access ~start_loc t

  and typeof_expr env = raw_typeof_expr_with_identifier env (Parse.identifier env)

  and raw_typeof_expr_with_identifier =
    let rec identifier env (q_loc, qualification) =
      if Peek.token env = T_PERIOD && Peek.ith_is_identifier_name ~i:1 env then
        let (loc, q) =
          with_loc
            ~start_loc:q_loc
            (fun env ->
              Expect.token env T_PERIOD;
              let id = identifier_name env in
              { Type.Typeof.Target.qualification; id })
            env
        in
        let qualification = Type.Typeof.Target.Qualified (loc, q) in
        identifier env (loc, qualification)
      else
        qualification
    in
    fun env ((loc, _) as id) ->
      let id = Type.Typeof.Target.Unqualified id in
      identifier env (loc, id)

  and typeof_arg env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let result =
      if Peek.token env = T_LPAREN then (
        Eat.token env;
        let typeof = typeof_arg env in
        Expect.token env T_RPAREN;
        typeof
      ) else if Peek.is_identifier env then
        Some (typeof_expr env)
      else (
        error env Parse_error.InvalidTypeof;
        None
      )
    in
    Eat.pop_lex_mode env;
    result

  and typeof env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_TYPEOF;
        match typeof_arg env with
        | None -> Type.Any None
        | Some argument ->
          let targs =
            if Peek.is_line_terminator env then
              None
            else
              type_args env
          in
          Type.Typeof
            { Type.Typeof.argument; targs; comments = Flow_ast_utils.mk_comments_opt ~leading () })
      env

  and primary env =
    let loc = Peek.loc env in
    match Peek.token env with
    | T_MULT ->
      let leading = Peek.comments env in
      Eat.token env;
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
          Eat.token env;
          let (extends, body) = interface_helper env in
          Type.Interface
            { Type.Interface.extends; body; comments = Flow_ast_utils.mk_comments_opt ~leading () })
        env
    | T_TYPEOF -> typeof env
    | T_LBRACKET -> tuple env
    | T_IDENTIFIER { raw = "component"; _ } when (parse_options env).components ->
      with_loc
        (fun env ->
          (* This logic is very similar to the statement parser but omits the component name *)
          let leading = Peek.comments env in
          Expect.identifier env "component";
          let tparams = type_params_remove_trailing env (type_params env) in
          let params = component_param_list env in
          let (params, renders) =
            if Peek.is_renders_ident env then
              let renders = renders_annotation_opt env in
              let renders = component_renders_annotation_remove_trailing env renders in
              (params, renders)
            else
              let missing_annotation = renders_annotation_opt env in
              (component_type_params_remove_trailing env params, missing_annotation)
          in
          Type.Component
            {
              Type.Component.tparams;
              params;
              renders;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            })
        env
    | T_IDENTIFIER { raw = "renders"; _ }
    | T_RENDERS_QUESTION
    | T_RENDERS_STAR ->
      with_loc (fun env -> Type.Renders (render_type env)) env
    | T_IDENTIFIER _
    | T_EXTENDS (* `extends` is reserved, but recover by treating it as an identifier *)
    | T_STATIC (* `static` is reserved, but recover by treating it as an identifier *) ->
      let (loc, g) = generic env in
      (loc, Type.Generic g)
    | T_STRING (loc, value, raw, octal) ->
      if octal then strict_error env Parse_error.StrictOctalLiteral;
      let leading = Peek.comments env in
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      ( loc,
        Type.StringLiteral
          {
            Ast.StringLiteral.value;
            raw;
            comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
          }
      )
    | T_MINUS -> with_loc negate env
    | T_NUMBER_SINGLETON_TYPE { kind; value; raw } ->
      with_loc (number_singleton ~neg:None kind value raw) env
    | T_BIGINT_SINGLETON_TYPE { kind = _; value; raw } ->
      with_loc (bigint_singleton ~neg:None value raw) env
    | (T_TRUE | T_FALSE) as token ->
      let leading = Peek.comments env in
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      let value = token = T_TRUE in
      ( loc,
        Type.BooleanLiteral
          { BooleanLiteral.value; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
      )
    | T_KEYOF ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Eat.token env;
          let trailing = Eat.trailing_comments env in
          let argument = _type env in
          Type.Keyof
            { Type.Keyof.argument; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () })
        env
    | T_READONLY ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Eat.token env;
          let trailing = Eat.trailing_comments env in
          let argument = _type env in
          Type.ReadOnly
            {
              Type.ReadOnly.argument;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
        env
    | T_INFER ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Eat.token env;
          let trailing = Eat.trailing_comments env in
          let tparam =
            with_loc
              (fun env ->
                let name = type_identifier env in
                let bound =
                  Try.or_else
                    env
                    ~fallback:(Type.Missing (Peek.loc env))
                    (fun env ->
                      if not @@ Eat.maybe env T_EXTENDS then raise Try.Rollback;
                      let bound = union env in
                      if Parser_env.no_conditional_type env || Peek.token env <> T_PLING then
                        Type.Available (fst bound, bound)
                      else
                        raise Try.Rollback)
                in
                {
                  Type.TypeParam.name;
                  bound;
                  bound_kind = Type.TypeParam.Extends;
                  variance = None;
                  default = None;
                })
              env
          in
          Type.Infer
            { Type.Infer.tparam; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () })
        env
    | T_ERROR "`" ->
      error env Parse_error.TSTemplateLiteralType;
      (loc, Type.Any None)
    | _ ->
      (match primitive env with
      | Some t -> (loc, t)
      | None ->
        error_unexpected ~expected:"a type" env;
        (loc, Type.Any None))

  and negate env =
    let leading = Peek.comments env in
    Eat.token env;
    match Peek.token env with
    | T_NUMBER_SINGLETON_TYPE { kind; value; raw } ->
      number_singleton ~neg:(Some leading) kind value raw env
    | T_BIGINT_SINGLETON_TYPE { kind = _; value; raw } ->
      bigint_singleton ~neg:(Some leading) value raw env
    | _ ->
      error_unexpected ~expected:"a number literal type" env;
      Type.Any None

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
    | T_NULL
    | T_UNKNOWN_TYPE
    | T_NEVER_TYPE
    | T_UNDEFINED_TYPE ->
      true
    | _ -> false

  and generic_of_primitive env name =
    let leading = Peek.comments env in
    let (loc, _) = with_loc Eat.token env in
    let trailing = Eat.trailing_comments env in
    let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
    Some
      (Ast.Type.Generic
         {
           Ast.Type.Generic.id =
             Ast.Type.Generic.Identifier.Unqualified (Flow_ast_utils.ident_of_source (loc, name));
           targs = None;
           comments;
         }
      )

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
    | T_BOOLEAN_TYPE kind ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      let raw =
        match kind with
        | BOOL -> `Bool
        | BOOLEAN -> `Boolean
      in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      Some (Type.Boolean { raw; comments })
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
    | T_UNKNOWN_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Unknown (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_NEVER_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Never (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_UNDEFINED_TYPE ->
      Eat.token env;
      let trailing = Eat.trailing_comments env in
      Some (Type.Undefined (Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
    | T_ASSERTS -> generic_of_primitive env "asserts"
    | T_IS -> generic_of_primitive env "is"
    | _ -> None

  and tuple =
    let element env =
      with_loc
        (fun env ->
          if Eat.maybe env T_ELLIPSIS then
            let name =
              match (Peek.is_identifier env, Peek.ith_token ~i:1 env) with
              | (true, T_PLING)
              | (true, T_COLON) ->
                let name = identifier_name env in
                if Peek.token env = T_PLING then (
                  error env Parse_error.InvalidTupleOptionalSpread;
                  Eat.token env
                );
                Expect.token env T_COLON;
                Some name
              | _ -> None
            in
            let annot = _type env in
            Type.Tuple.SpreadElement { Type.Tuple.SpreadElement.name; annot }
          else
            let variance =
              match Peek.token env with
              | T_PLUS -> maybe_variance env
              | T_MINUS when Peek.ith_is_identifier ~i:1 env ->
                (* `-1` is a valid type but not a valid tuple label.
                   But `-foo` is only valid as a tuple label. *)
                maybe_variance env
              | _ -> None
            in
            match (Peek.is_identifier env, Peek.ith_token ~i:1 env) with
            | (true, T_PLING)
            | (true, T_COLON) ->
              let name = identifier_name env in
              let optional = Eat.maybe env T_PLING in
              Expect.token env T_COLON;
              let annot = _type env in
              Type.Tuple.LabeledElement
                { Type.Tuple.LabeledElement.name; annot; variance; optional }
            | _ ->
              if Option.is_some variance then error env Parse_error.InvalidTupleVariance;
              Type.Tuple.UnlabeledElement (_type env))
        env
    in
    let rec elements env acc =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET ->
        List.rev acc
      | _ ->
        let acc = element env :: acc in
        (* Trailing comma support (like [number, string,]) *)
        if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
        elements env acc
    in
    fun env ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          Expect.token env T_LBRACKET;
          let els = elements (with_no_anon_function_type false env) [] in
          Expect.token env T_RBRACKET;
          let trailing = Eat.trailing_comments env in
          Type.Tuple
            {
              Type.Tuple.elements = els;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
        env

  and render_type env =
    let leading = Peek.comments env in
    let variant =
      match Peek.token env with
      | T_IDENTIFIER { raw = "renders"; _ } -> Type.Renders.Normal
      | T_RENDERS_QUESTION -> Type.Renders.Maybe
      | T_RENDERS_STAR -> Type.Renders.Star
      | _ ->
        failwith
          "You should only call render_type after making sure the next token is a renders variant"
    in
    let operator_loc = Peek.loc env in
    Eat.token env;
    let trailing = Eat.trailing_comments env in
    let argument = prefix env in
    {
      Type.Renders.operator_loc;
      argument;
      comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
      variant;
    }

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

  and component_param_list_without_parens =
    let param_name env =
      match Peek.token env with
      | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Parse_error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let trailing = Eat.trailing_comments env in
        Statement.ComponentDeclaration.Param.StringLiteral
          (loc, { StringLiteral.value; raw; comments = Flow_ast_utils.mk_comments_opt ~trailing () })
      (* If not a string, must be an identifier *)
      | _ ->
        Eat.push_lex_mode env Lex_mode.NORMAL;
        let ident = Parse.identifier env in
        Eat.pop_lex_mode env;
        Statement.ComponentDeclaration.Param.Identifier ident
    in

    let param env =
      with_loc
        (fun env ->
          let name = param_name env in
          let optional = Eat.maybe env T_PLING in
          let annot = annotation env in
          { Ast.Type.Component.Param.name; annot; optional })
        env
    in
    let rec param_list env acc =
      match Peek.token env with
      | (T_EOF | T_ELLIPSIS | T_RPAREN) as t ->
        let rest =
          if t = T_ELLIPSIS then
            let rest =
              with_loc
                (fun env ->
                  let leading = Peek.comments env in
                  Expect.token env T_ELLIPSIS;
                  let (argument, optional) =
                    match Peek.ith_token ~i:1 env with
                    | T_COLON ->
                      Eat.push_lex_mode env Lex_mode.NORMAL;
                      let ident = Parse.identifier env in
                      Eat.pop_lex_mode env;
                      Expect.token env T_COLON;
                      (Some ident, false)
                    | T_PLING ->
                      Eat.push_lex_mode env Lex_mode.NORMAL;
                      let ident = Parse.identifier env in
                      Eat.pop_lex_mode env;
                      Expect.token env T_PLING;
                      Expect.token env T_COLON;
                      (Some ident, true)
                    | _ -> (None, false)
                  in
                  let annot = _type env in
                  {
                    Ast.Type.Component.RestParam.argument;
                    annot;
                    optional;
                    comments = Flow_ast_utils.mk_comments_opt ~leading ();
                  })
                env
            in
            Some rest
          else
            None
        in
        { Ast.Type.Component.Params.params = List.rev acc; rest; comments = None }
      | _ ->
        let acc = param env :: acc in
        if Peek.token env <> T_RPAREN then Expect.token env T_COMMA;
        param_list env acc
    in
    (* Need this wrapper function due to a compilation issue with js_of_ocaml.
       Directly returning param_list causes an error. *)
    (fun env acc -> param_list env acc)

  and component_param_list env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_LPAREN;
        let params = component_param_list_without_parens env [] in
        let internal = Peek.comments env in
        Expect.token env T_RPAREN;
        let trailing = Eat.trailing_comments env in
        {
          params with
          Ast.Type.Component.Params.comments =
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
      | T_RENDERS_QUESTION ->
        (match Peek.ith_token ~i:1 env with
        | T_COLON ->
          (* Ok this is definitely a parameter *)
          ParamList (function_param_list_without_parens env [])
        | _ -> Type (_type env))
      | T_IDENTIFIER { raw = "renders"; _ } ->
        (match Peek.ith_token ~i:1 env with
        | T_PLING
        | T_COLON ->
          (* Ok this is definitely a parameter *)
          ParamList (function_param_list_without_parens env [])
        | _ -> Type (_type env))
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
      let start_loc = Peek.loc env in
      let id = type_identifier env in
      Type
        (generic_type_with_identifier env id
        |> postfix_with env ~start_loc
        |> anon_function_without_parens_with env
        |> intersection_with ~start_loc env
        |> union_with ~start_loc env
        |> conditional_with (Parser_env.with_no_conditional_type false env) ~start_loc
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
        let return = function_return_type env in
        Type.(Function { Function.params; return; tparams; comments = None }))
      env

  and function_return_type env =
    if is_start_of_type_guard env then
      Type.Function.TypeGuard (type_guard env)
    else
      Type.Function.TypeAnnotation (_type env)

  and type_guard env =
    with_loc
      (fun env ->
        let leading = Peek.comments env in
        let asserts = Eat.maybe env T_ASSERTS in
        (* Parse the identifier part as normal code, since this can be any name that
         * a parameter can be. *)
        Eat.push_lex_mode env Lex_mode.NORMAL;
        let param = identifier_name env in
        Eat.pop_lex_mode env;
        let (t, internal) =
          match Peek.token env with
          | T_IS ->
            let internal = Peek.comments env in
            Expect.token env T_IS;
            let internal = internal @ Peek.comments env in
            (Some (_type env), internal)
          | _ -> (None, [])
        in
        let guard = (param, t) in
        let comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~internal () in
        { Ast.Type.TypeGuard.asserts; guard; comments })
      env

  and type_guard_annotation env ~start_loc = with_loc ~start_loc type_guard env

  and _object =
    let methodish env start_loc tparams =
      with_loc
        ~start_loc
        (fun env ->
          let params = function_param_list env in
          Expect.token env T_COLON;
          let return = function_return_type env in
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
              })
          env
      in
      Type.Object.CallProperty prop
    in
    let init_property env start_loc ~variance ~static ~proto ~leading (key_loc, key) =
      ignore proto;
      if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
      let prop =
        with_loc
          ~start_loc
          (fun env ->
            let optional = Eat.maybe env T_PLING in
            let value =
              if Expect.token_maybe env T_COLON then
                _type env
              else
                (key_loc, Type.Any None)
            in
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
              })
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
              })
          env
      in
      Type.Object.Property prop
    in
    let indexer_property env start_loc static variance ~leading =
      let indexer =
        with_loc
          ~start_loc
          (fun env ->
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

    let mapped_type env start_loc variance ~leading =
      let mapped_type =
        with_loc
          ~start_loc
          (fun env ->
            let ((key_name_loc, _) as key_id) = type_identifier env in
            let key_tparam =
              {
                Type.TypeParam.name = key_id;
                bound = Ast.Type.Missing key_name_loc;
                variance = None;
                default = None;
                bound_kind = Type.TypeParam.Colon;
              }
            in
            (* We already checked in mapped_type_or_indexer that the next token was an
             * "in" identifier. Now we eat it. *)
            Eat.token env;
            let source_type = _type env in
            Expect.token env T_RBRACKET;
            let optional =
              Type.Object.MappedType.(
                match Peek.token env with
                | T_PLING ->
                  Eat.token env;
                  Optional
                | T_PLUS ->
                  Eat.token env;
                  Expect.token env T_PLING;
                  PlusOptional
                | T_MINUS ->
                  Eat.token env;
                  Expect.token env T_PLING;
                  MinusOptional
                | _ -> NoOptionalFlag
              )
            in
            Expect.token env T_COLON;
            let prop_type = _type env in
            let trailing = Eat.trailing_comments env in
            {
              Type.Object.MappedType.key_tparam = (key_name_loc, key_tparam);
              source_type;
              prop_type;
              variance;
              optional;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
          env
      in
      Type.Object.MappedType mapped_type
    in

    let mapped_type_or_indexer env start_loc static variance ~leading =
      let leading = leading @ Peek.comments env in
      Expect.token env T_LBRACKET;
      match Peek.ith_token ~i:1 env with
      | T_IDENTIFIER { raw = "in"; _ } when static = None ->
        mapped_type env start_loc variance ~leading
      | _ -> indexer_property env start_loc static variance ~leading
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
      | _ -> Expect.error env T_COMMA
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
            let prop = spread_property env start_loc ~leading in
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
      | T_READONLY
        when variance = None
             && (Peek.ith_is_identifier ~i:1 env || Peek.ith_token ~i:1 env = T_LBRACKET) ->
        let variance = maybe_variance ~parse_readonly:true env in
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
        | _ -> mapped_type_or_indexer env start_loc static variance ~leading)
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
          init_property env start_loc ~variance ~static ~proto ~leading:[] (static_loc, key)
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
          init_property env start_loc ~variance ~static ~proto ~leading:[] (proto_loc, key)
        | _ ->
          let object_key env =
            Eat.push_lex_mode env Lex_mode.NORMAL;
            let result = Parse.object_key env in
            Eat.pop_lex_mode env;
            result
          in
          let leading_key = Peek.comments env in
          (match object_key env with
          | ( key_loc,
              ( Expression.Object.Property.Identifier
                  (_, { Identifier.name = ("get" | "set") as name; comments = _ }) as key
              )
            ) -> begin
            match Peek.token env with
            | T_LESS_THAN
            | T_LPAREN ->
              error_unexpected_proto env proto;
              error_unexpected_variance env variance;
              method_property env start_loc static key ~leading
            | T_COLON
            | T_PLING ->
              init_property env start_loc ~variance ~static ~proto ~leading (key_loc, key)
            | _ ->
              ignore (object_key_remove_trailing env key);
              let key = object_key env in
              let is_getter = name = "get" in
              let leading = leading @ leading_key in
              error_unexpected_proto env proto;
              error_unexpected_variance env variance;
              getter_or_setter ~is_getter ~leading env start_loc static key
          end
          | (key_loc, key) -> begin
            match Peek.token env with
            | T_LESS_THAN
            | T_LPAREN ->
              error_unexpected_proto env proto;
              error_unexpected_variance env variance;
              method_property env start_loc static key ~leading
            | _ ->
              error_invalid_property_name env is_class static key;
              init_property env start_loc ~variance ~static ~proto ~leading (key_loc, key)
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
        if Eat.maybe env T_EXTENDS then
          let extends = supers env [] in
          generic_type_list_remove_trailing env extends
        else
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
        let (bound, bound_kind) =
          match Peek.token env with
          | T_COLON -> (Ast.Type.Available (annotation env), Ast.Type.TypeParam.Colon)
          | T_EXTENDS ->
            ( Ast.Type.Available
                (with_loc
                   (fun env ->
                     Eat.token env;
                     _type env)
                   env
                ),
              Ast.Type.TypeParam.Extends
            )
          | _ -> (Ast.Type.Missing (Peek.loc_skip_lookahead env), Ast.Type.TypeParam.Colon)
        in
        (name, bound, bound_kind))
      env

  and type_params =
    (* whether we should consume [token] as a type param. a type param can
       either start with an identifier or a variance sigil; we'll also parse
       types like `number` to improve error recovery. *)
    let token_is_maybe_param env token =
      token_is_type_identifier env token || token_is_variance token || token_is_reserved_type token
    in
    (* whether an unexpected [token] should signal the end of the param list.
       these are tokens that are likely to follow a param list, if the closing
       > is missing. This improves error recovery when you add type params
       to an existing node.

       Note that we're in Lex_mode.TYPE here, so the tokens are those produced
       by [Flow_lexer.type_token]. *)
    let token_is_maybe_end_of_list env token =
      match token with
      (* Reserved words are lexed as identifiers in Lex_env.TYPE mode (if
         they're not also reserved types). e.g. `switch` is a T_IDENTIFIER.
         we're not expecting a type identifier, so let's assume it's a
         NORMAL-mode keyword and end the list. *)
      | T_IDENTIFIER { raw; _ } when is_reserved raw || is_contextually_reserved raw -> true
      (* adding a type above an enum: `type T<U\nenum ....` (`enum` is not an ES keyword) *)
      | T_IDENTIFIER { raw = "enum"; _ } when (parse_options env).enums -> true
      (* adding a type above another: `type T<U\ntype V ...` (`type` is not an ES keyword) *)
      | T_IDENTIFIER { raw = "type"; _ }
      (* RHS: `type U<T = default = ...` (this only helps if there's a default!) *)
      | T_ASSIGN
      (* start of function params: `function f<T|(...)` *)
      | T_LPAREN
      (* class heritage: `class C<T| implements ...` *)
      | T_IDENTIFIER { raw = "implements"; _ } ->
        true
      | _ -> false
    in
    let rec params env ~require_default acc =
      let (acc, require_default) =
        if token_is_maybe_param env (Peek.token env) then
          let (param, require_default) =
            with_loc_extra
              (fun env ->
                let variance = maybe_variance ~parse_in_out:true env in
                let (loc, (name, bound, bound_kind)) = bounded_type env in
                let (default, require_default) =
                  match Peek.token env with
                  | T_ASSIGN ->
                    Eat.token env;
                    (Some (_type env), true)
                  | _ ->
                    if require_default then error_at env (loc, Parse_error.MissingTypeParamDefault);
                    (None, require_default)
                in
                ({ Type.TypeParam.name; bound; bound_kind; variance; default }, require_default))
              env
          in
          (param :: acc, require_default)
        else
          (acc, require_default)
      in
      match Peek.token env with
      | T_EOF
      | T_GREATER_THAN ->
        (* end of list *)
        List.rev acc
      | T_COMMA ->
        (* handle multiple params *)
        Eat.token env;
        params env ~require_default acc
      | token when token_is_maybe_end_of_list env token ->
        (* error recovery: tokens likely to follow a param list *)
        Expect.error env T_GREATER_THAN;
        List.rev acc
      | token when token_is_maybe_param env token ->
        (* recover from a missing comma between items by not consuming the token *)
        Expect.error env T_COMMA;
        params env ~require_default acc
      | _ ->
        (* unexpected token. consume it until we hit the end of the list *)
        Expect.token env T_COMMA;
        params env ~require_default acc
    in
    fun env ->
      if Peek.token env = T_LESS_THAN then (
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
        let ((loc, { Type.TypeParams.params; _ }) as result) =
          with_loc
            (fun env ->
              let leading = Peek.comments env in
              Expect.token env T_LESS_THAN;
              let params = params env ~require_default:false [] in
              let internal = Peek.comments env in
              Expect.token_opt env T_GREATER_THAN;
              let trailing = Eat.trailing_comments env in
              let comments =
                Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ()
              in
              { Type.TypeParams.params; comments })
            env
        in
        (match params with
        | [] -> error_at env (loc, Parse_error.MissingTypeParam)
        | _ -> ());
        Some result
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

  and function_return_annotation_opt env =
    match Peek.token env with
    | T_COLON -> function_return_annotation env
    | _ -> Function.ReturnAnnot.Missing (Peek.loc_skip_lookahead env)

  and annotation_opt env =
    match Peek.token env with
    | T_COLON -> Type.Available (annotation env)
    | _ -> Type.Missing (Peek.loc_skip_lookahead env)

  and renders_annotation_opt env =
    match Peek.token env with
    | T_COLON ->
      let operator_loc = Peek.loc env in
      if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
      error env Parse_error.InvalidComponentRenderAnnotation;
      Eat.token env;
      let (loc, argument) = with_loc _type env in
      Type.AvailableRenders
        ( loc,
          {
            Ast.Type.Renders.operator_loc;
            argument;
            variant = Ast.Type.Renders.Normal;
            comments = None;
          }
        )
    | T_IDENTIFIER { raw = "renders"; _ }
    | T_RENDERS_QUESTION
    | T_RENDERS_STAR ->
      if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
      let (loc, renders) = with_loc ~start_loc:(Peek.loc env) render_type env in
      Type.AvailableRenders (loc, renders)
    | _ -> Type.MissingRenders (Peek.loc_skip_lookahead env)

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
      | Boolean ({ comments; _ } as t) -> Boolean { t with comments = merge_comments comments }
      | Symbol comments -> Symbol (merge_comments comments)
      | Exists comments -> Exists (merge_comments comments)
      | Unknown comments -> Unknown (merge_comments comments)
      | Never comments -> Never (merge_comments comments)
      | Undefined comments -> Undefined (merge_comments comments)
      | Nullable ({ Nullable.comments; _ } as t) ->
        Nullable { t with Nullable.comments = merge_comments comments }
      | Function ({ Function.comments; _ } as t) ->
        Function { t with Function.comments = merge_comments comments }
      | Component ({ Component.comments; _ } as t) ->
        Component { t with Component.comments = merge_comments comments }
      | Object ({ Object.comments; _ } as t) ->
        Object { t with Object.comments = merge_comments_with_internal comments }
      | Interface ({ Interface.comments; _ } as t) ->
        Interface { t with Interface.comments = merge_comments comments }
      | Array ({ Array.comments; _ } as t) ->
        Array { t with Array.comments = merge_comments comments }
      | Conditional ({ Conditional.comments; _ } as t) ->
        Conditional { t with Conditional.comments = merge_comments comments }
      | Infer ({ Infer.comments; _ } as t) ->
        Infer { t with Infer.comments = merge_comments comments }
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
      | Keyof ({ Keyof.comments; _ } as t) ->
        Keyof { t with Keyof.comments = merge_comments comments }
      | Renders ({ Renders.comments; _ } as t) ->
        Renders { t with Renders.comments = merge_comments comments }
      | ReadOnly ({ ReadOnly.comments; _ } as t) ->
        ReadOnly { t with ReadOnly.comments = merge_comments comments }
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

  let predicate_checks_contents env ~leading =
    let open Ast.Type.Predicate in
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

  let predicate =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_CHECKS;
        predicate_checks_contents env ~leading
    )

  let predicate_opt env =
    let env = with_no_anon_function_type false env in
    match Peek.token env with
    | T_CHECKS -> Some (predicate env)
    | _ -> None

  let no_annot_predicate env ~start_loc =
    let env = with_no_anon_function_type false env in
    with_loc
      ~start_loc
      (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_CHECKS;
        predicate_checks_contents env ~leading)
      env

  let function_return_annotation_and_predicate env =
    let open Ast.Function.ReturnAnnot in
    if not (should_parse_types env) then error env Parse_error.UnexpectedTypeAnnotation;
    let missing_loc = Peek.loc_skip_lookahead env in
    let start_loc = Peek.loc env in
    Expect.token env T_COLON;
    match Peek.token env with
    | T_CHECKS ->
      let predicate = no_annot_predicate env ~start_loc in
      (Missing missing_loc, Some predicate)
    | _ ->
      if is_start_of_type_guard env then
        (TypeGuard (type_guard_annotation env ~start_loc), None)
      else
        let annotation =
          let annotation = Available (with_loc ~start_loc _type env) in
          if Peek.token env = T_CHECKS then
            return_annotation_remove_trailing env annotation
          else
            annotation
        in
        let predicate = predicate_opt env in
        (annotation, predicate)

  let function_return_annotation_and_predicate_opt env =
    let open Ast.Function.ReturnAnnot in
    match Peek.token env with
    | T_COLON -> function_return_annotation_and_predicate env
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

  let function_return_annotation_opt = wrap function_return_annotation_opt

  let predicate_opt = wrap predicate_opt

  let function_return_annotation_and_predicate_opt =
    wrap function_return_annotation_and_predicate_opt

  let component_param_list = wrap component_param_list

  let generic = wrap generic

  let renders_annotation_opt = wrap renders_annotation_opt
end
