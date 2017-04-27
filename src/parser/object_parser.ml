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

open Parser_common

(* A module for parsing various object related things, like object literals
 * and classes *)

module type OBJECT = sig
  val key : env -> Loc.t * Ast.Expression.Object.Property.key
  val _initializer : env -> Loc.t * Ast.Expression.Object.t
  val class_declaration : env -> Ast.Expression.t list -> Ast.Statement.t
  val class_expression : env -> Ast.Expression.t
  val decorator_list : env -> Ast.Expression.t list
end

module Object
  (Parse: Parser_common.PARSER)
  (Type: Type_parser.TYPE)
  (Declaration: Declaration_parser.DECLARATION)
  (Expression: Expression_parser.EXPRESSION)
: OBJECT = struct
  let decorator_list =
    let rec decorator_list_helper env decorators =
      match Peek.token env with
      | T_AT ->
          Eat.token env;
          decorator_list_helper env ((Expression.left_hand_side env)::decorators)
      | _ ->
          decorators

    in fun env ->
      if (parse_options env).esproposal_decorators
      then List.rev (decorator_list_helper env [])
      else []

  let key env =
    Ast.Expression.Object.Property.(match Peek.token env with
    | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let value = Literal.String value in
        loc, Literal (loc, { Literal.value; raw; })
    | T_NUMBER number_type ->
        let raw = Peek.value env in
        let loc = Peek.loc env in
        let value = Expression.number env number_type in
        let value = Literal.Number value in
        loc,  Literal (loc, { Literal.value; raw; })
    | T_LBRACKET ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let expr = Parse.assignment (env |> with_no_in false) in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.Computed expr
    | _ ->
        let id, _ = Expression.identifier_or_reserved_keyword env in
        fst id, Identifier id)

  let getter_or_setter env is_getter =
    (* this is a getter or setter, it cannot be async *)
    let async = false in
    let generator = Declaration.generator env in
    let key_loc, key = key env in
    let start_loc = Peek.loc env in
    (* It's not clear how type params on getters & setters would make sense
     * in Flow's type system. Since this is a Flow syntax extension, we might
     * as well disallow it until we need it *)
    let typeParameters = None in
    let params = Declaration.function_params env in
    begin match is_getter, params with
    | true, ([], None) -> ()
    | false, (_, Some _rest) ->
        (* rest params don't make sense on a setter *)
        error_at env (key_loc, Error.SetterArity)
    | false, ([_], _) -> ()
    | true, _ -> error_at env (key_loc, Error.GetterArity)
    | false, _ -> error_at env (key_loc, Error.SetterArity)
    end;
    let returnType = Type.annotation_opt env in
    let _, body, strict = Declaration.function_body env ~async ~generator in
    let simple = Declaration.is_simple_function_params params in
    Declaration.strict_post_check env ~strict ~simple None params;
    let end_loc, expression = Function.(
      match body with
      | BodyBlock (loc, _) -> loc, false
      | BodyExpression (loc, _) -> loc, true) in
    let loc = Loc.btwn start_loc end_loc in
    let value = loc, Function.({
      id = None;
      params;
      body;
      generator;
      async;
      predicate = None; (* setters/getter are not predicates *)
      expression;
      returnType;
      typeParameters;
    }) in
    key, value

  let _initializer =
    let rec property env = Ast.Expression.Object.(
      let start_loc = Peek.loc env in
      if Peek.token env = T_ELLIPSIS
      then begin
        (* Spread property *)
        Expect.token env T_ELLIPSIS;
        let argument = Parse.assignment env in
        SpreadProperty (Loc.btwn start_loc (fst argument), SpreadProperty.({
          argument;
        }))
      end else begin
        (* look for a following identifier to tell whether to parse a function
         * or not *)
        let async =
          Peek.is_literal_property_name ~i:1 env && Declaration.async env in
        Property (match async , Declaration.generator env, key env with
        | false, false, (_, (Property.Identifier (_, "get") as key)) ->
            (match Peek.token env with
            | T_COLON
            | T_LESS_THAN
            | T_LPAREN
            | T_COMMA
            | T_RCURLY -> init env start_loc key false false
            | _ -> get env start_loc)
        | false, false, (_, (Property.Identifier (_, "set") as key)) ->
            (match Peek.token env with
            | T_COLON
            | T_LESS_THAN
            | T_LPAREN
            | T_COMMA
            | T_RCURLY -> init env start_loc key false false
            | _ -> set env start_loc)
        | async, generator, (_, key) ->
            init env start_loc key async generator
        )
      end
    )

    and get env start_loc =
      let key, (end_loc, fn) = getter_or_setter env true in
      Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.({
        key;
        value = Get (end_loc, fn);
        _method = false;
        shorthand = false;
      })

    and set env start_loc =
      let key, (end_loc, fn) = getter_or_setter env false in
      Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.({
        key;
        value = Set (end_loc, fn);
        _method = false;
        shorthand = false;
      })

    and init =
      let open Ast.Expression.Object.Property in
      let parse_shorthand key = match key with
        | Literal lit -> fst lit, Ast.Expression.Literal (snd lit)
        | Identifier id -> fst id, Ast.Expression.Identifier id
        | Computed expr -> expr
      in
      let parse_method env ~async ~generator =
        let start_loc = Peek.loc env in
        let typeParameters = Type.type_parameter_declaration env in
        let params = Declaration.function_params env in
        let returnType = Type.annotation_opt env in
        let _, body, strict =
          Declaration.function_body env ~async ~generator in
        let simple = Declaration.is_simple_function_params params in
        Declaration.strict_post_check env ~strict ~simple None params;
        let end_loc, expression = match body with
          | Function.BodyBlock (loc, _) -> loc, false
          | Function.BodyExpression (loc, _) -> loc, true
        in
        let loc = Loc.btwn start_loc end_loc in
        let value = loc, Ast.Expression.(Function Function.({
          id = None;
          params;
          body;
          generator;
          async;
          (* TODO: add support for object method predicates *)
          predicate = None;
          expression;
          returnType;
          typeParameters;
        })) in
        value
      in
      let parse_value env =
        Expect.token env T_COLON;
        Parse.assignment env
      in
      let parse_init ~key ~async ~generator env = match Peek.token env with
        | T_RCURLY
        | T_COMMA ->
          parse_shorthand key, true, false
        | T_LESS_THAN
        | T_LPAREN ->
          parse_method env ~async ~generator, false, true
        | _ ->
          parse_value env, false, false
      in
      fun env start_loc key async generator ->
        let end_loc, (value, shorthand, _method) = with_loc (
          parse_init ~key ~async ~generator
        ) env in
        Loc.btwn start_loc end_loc, {
          key;
          value = Init value;
          _method;
          shorthand;
        }

    and properties env acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev acc
      | _ ->
          let prop = property env in
          if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
          properties env (prop::acc)

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      let props = properties env [] in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Loc.btwn start_loc end_loc, Ast.Expression.Object.({
        properties = props;
      })

  let rec _class env =
    let superClass, superTypeParameters =
      if Peek.token env = T_EXTENDS
      then begin
        Expect.token env T_EXTENDS;
        let superClass =
          Expression.left_hand_side (env |> with_allow_yield false) in
        let superTypeParameters = Type.type_parameter_instantiation env in
        Some superClass, superTypeParameters
      end else None, None in
    let implements =
      if Peek.token env = T_IMPLEMENTS
      then begin
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeInterface;
        Expect.token env T_IMPLEMENTS;
        class_implements env []
      end else [] in
    let body = class_body env in
    body, superClass, superTypeParameters, implements

  and class_implements env acc =
    let id = Parse.identifier env in
    let typeParameters = Type.type_parameter_instantiation env in
    let loc = match typeParameters with
    | None -> fst id
    | Some (loc, _) -> Loc.btwn (fst id) loc in
    let implement = loc, Ast.Class.Implements.({
      id;
      typeParameters;
    }) in
    let acc = implement::acc in
    match Peek.token env with
    | T_COMMA ->
        Expect.token env T_COMMA;
        class_implements env acc
    | _ -> List.rev acc

  and class_body =
    let rec elements env acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev acc
      | T_SEMICOLON ->
          (* Skip empty elements *)
          Expect.token env T_SEMICOLON;
          elements env acc
      | _ -> elements env ((class_element env)::acc)

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      let body = elements env [] in
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Loc.btwn start_loc end_loc, Ast.Class.Body.({
        body;
      })

  (* In the ES6 draft, all elements are methods. No properties (though there
   * are getter and setters allowed *)
  and class_element =
    let get env start_loc decorators static =
      let key, (end_loc, _ as value) =
        getter_or_setter env true in
      Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
        key;
        value;
        kind = Get;
        static;
        decorators;
      })))

    in let set env start_loc decorators static =
      let key, (end_loc, _ as value) =
        getter_or_setter env false in
      Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
        key;
        value;
        kind = Set;
        static;
        decorators;
      })))

    in let error_unsupported_variance env = function
    | Some (loc, _) -> error_at env (loc, Error.UnexpectedVariance)
    | None -> ()

    in let rec init env start_loc decorators key async generator static variance =
      match Peek.token env with
      | T_COLON
      | T_ASSIGN
      | T_SEMICOLON when not async && not generator ->
        (* Class property with annotation *)
        let end_loc, (typeAnnotation, value) = with_loc (fun env ->
          let typeAnnotation = Type.annotation_opt env in
          let options = parse_options env in
          let value =
            if Peek.token env = T_ASSIGN then (
              if static && options.esproposal_class_static_fields
                 || (not static) && options.esproposal_class_instance_fields
              then begin
                Expect.token env T_ASSIGN;
                Some (Parse.expression env)
              end else None
            ) else None
          in
          begin if Expect.maybe env T_SEMICOLON then
            ()
          else if Peek.token env == T_LBRACKET || Peek.token env == T_LPAREN then
            error_unexpected env
          end;
          typeAnnotation, value
        ) env in
        let loc = Loc.btwn start_loc end_loc in
        Ast.Class.(Body.Property (loc, Property.({
          key;
          value;
          typeAnnotation;
          static;
          variance;
        })))
      | T_PLING ->
        (* TODO: add support for optional class properties *)
        error_unexpected env;
        Eat.token env;
        init env start_loc decorators key async generator static variance
      | _ ->
        error_unsupported_variance env variance;
        let func_loc = Peek.loc env in
        let typeParameters = Type.type_parameter_declaration env in
        let params = Declaration.function_params env in
        let returnType = Type.annotation_opt env in
        let _, body, strict =
          Declaration.function_body env ~async ~generator in
        let simple = Declaration.is_simple_function_params params in
        Declaration.strict_post_check env ~strict ~simple None params;
        let end_loc, expression = Function.(
          match body with
          | BodyBlock (loc, _) -> loc, false
          | BodyExpression (loc, _) -> loc, true) in
        let loc = Loc.btwn func_loc end_loc in
        let value = loc, Function.({
          id = None;
          params;
          body;
          generator;
          async;
          (* TODO: add support for method predicates *)
          predicate = None;
          expression;
          returnType;
          typeParameters;
        }) in
        let kind = Ast.(match static, key with
          | false, Expression.Object.Property.Identifier (_, "constructor")
          | false, Expression.Object.Property.Literal (_, {
              Literal.value = Literal.String "constructor";
              _;
            }) ->
            Class.Method.Constructor
          | _ ->
            Class.Method.Method) in
        Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
          key;
          value;
          kind;
          static;
          decorators;
        })))

    in fun env -> Ast.Expression.Object.Property.(
      let start_loc = Peek.loc env in
      let decorators = decorator_list env in
      let static =
        Peek.token ~i:1 env <> T_LPAREN &&
        Peek.token ~i:1 env <> T_LESS_THAN &&
        Expect.maybe env T_STATIC in
      let async =
        Peek.token ~i:1 env <> T_LPAREN &&
        Peek.token ~i:1 env <> T_COLON &&
        Declaration.async env in
      let generator = Declaration.generator env in
      let variance = Declaration.variance env async generator in
      let generator = match generator, variance with
      | false, Some _ -> Declaration.generator env
      | _ -> generator
      in
      match (async, generator, key env) with
      | false, false,
        (_, (Identifier (_, "get") as key)) ->
          (match Peek.token env with
          | T_LESS_THAN
          | T_COLON
          | T_ASSIGN
          | T_SEMICOLON
          | T_LPAREN ->
            init env start_loc decorators key async generator static variance
          | _ ->
            error_unsupported_variance env variance;
            get env start_loc decorators static)
      | false, false,
        (_, (Identifier (_, "set") as key)) ->
          (match Peek.token env with
          | T_LESS_THAN
          | T_COLON
          | T_ASSIGN
          | T_SEMICOLON
          | T_LPAREN ->
            init env start_loc decorators key async generator static variance
          | _ ->
            error_unsupported_variance env variance;
            set env start_loc decorators static)
      | _, _, (_, key) ->
          init env start_loc decorators key async generator static variance
    )

  let class_declaration env decorators =
    (* 10.2.1 says all parts of a class definition are strict *)
    let env = env |> with_strict true in
    let start_loc = Peek.loc env in
    let decorators = decorators @ (decorator_list env) in
    Expect.token env T_CLASS;
    let tmp_env = env |> with_no_let true in
    let id = (
      match in_export env, Peek.is_identifier tmp_env with
      | true, false -> None
      | _ -> Some(Parse.identifier tmp_env)
    ) in
    let typeParameters = Type.type_parameter_declaration_with_defaults env in
    let body, superClass, superTypeParameters, implements = _class env in
    let loc = Loc.btwn start_loc (fst body) in
    loc, Ast.Statement.(ClassDeclaration Class.({
      id;
      body;
      superClass;
      typeParameters;
      superTypeParameters;
      implements;
      classDecorators=decorators;
    }))

  let class_expression env =
    let start_loc = Peek.loc env in
    let decorators = decorator_list env in
    Expect.token env T_CLASS;
    let id, typeParameters = match Peek.token env with
      | T_EXTENDS
      | T_LESS_THAN
      | T_LCURLY -> None, None
      | _ ->
          let id = Some (Parse.identifier env) in
          let typeParameters = Type.type_parameter_declaration_with_defaults env in
          id, typeParameters in
    let body, superClass, superTypeParameters, implements = _class env in
    let loc = Loc.btwn start_loc (fst body) in
    loc, Ast.Expression.(Class Class.({
      id;
      body;
      superClass;
      typeParameters;
      superTypeParameters;
      implements;
      classDecorators=decorators;
    }))
end
