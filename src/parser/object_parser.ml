(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_env
open Ast
module Error = Parse_error
module SSet = Set.Make(String)

open Parser_common

(* A module for parsing various object related things, like object literals
 * and classes *)

module type OBJECT = sig
  val key : ?class_body: bool -> env -> Loc.t * Loc.t Ast.Expression.Object.Property.key
  val _initializer : env -> Loc.t * Loc.t Ast.Expression.Object.t * pattern_errors
  val class_declaration : env -> Loc.t Ast.Expression.t list -> Loc.t Ast.Statement.t
  val class_expression : env -> Loc.t Ast.Expression.t
  val decorator_list : env -> Loc.t Ast.Expression.t list
end

module Object
  (Parse: Parser_common.PARSER)
  (Type: Type_parser.TYPE)
  (Declaration: Declaration_parser.DECLARATION)
  (Expression: Expression_parser.EXPRESSION)
  (Pattern_cover: Pattern_cover.COVER)
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

  let key ?(class_body=false) env =
    let open Ast.Expression.Object.Property in
    match Peek.token env with
    | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let value = Literal.String value in
        loc, Literal (loc, { Literal.value; raw; })
    | T_NUMBER { kind; raw } ->
        let loc = Peek.loc env in
        let value = Expression.number env kind raw in
        let value = Literal.Number value in
        loc,  Literal (loc, { Literal.value; raw; })
    | T_LBRACKET ->
        let start_loc = Peek.loc env in
        Expect.token env T_LBRACKET;
        let expr = Parse.assignment (env |> with_no_in false) in
        let end_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        Loc.btwn start_loc end_loc, Ast.Expression.Object.Property.Computed expr
    | T_POUND when class_body ->
        let loc, id, _is_private = Expression.property_name_include_private env in
        add_declared_private env (snd id);
        loc, PrivateName (loc, id)
    | _ ->
        let loc, id, is_private = Expression.property_name_include_private env in
        if is_private then error_at env (loc, Parse_error.PrivateNotInClass);
        loc, Identifier id

  let getter_or_setter env is_getter =
    (* this is a getter or setter, it cannot be async *)
    let async = false in
    let generator = Declaration.generator env in
    let key_loc, key = key env in
    let start_loc = Peek.loc env in

    (* #sec-function-definitions-static-semantics-early-errors *)
    let env = env |> with_allow_super Super_prop in

    (* It's not clear how type params on getters & setters would make sense
     * in Flow's type system. Since this is a Flow syntax extension, we might
     * as well disallow it until we need it *)
    let typeParameters = None in
    let params = Declaration.function_params ~await:false ~yield:false env in
    begin match is_getter, params with
    | true, (_, { Ast.Function.Params.params = []; rest = None }) -> ()
    | false, (_, { Ast.Function.Params.rest = Some _; _ }) ->
        (* rest params don't make sense on a setter *)
        error_at env (key_loc, Error.SetterArity)
    | false, (_, { Ast.Function.Params.params = [_]; _ }) -> ()
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
    let parse_assignment_cover env =
      match Expression.assignment_cover env with
      | Cover_expr expr -> expr, Pattern_cover.empty_errors
      | Cover_patt (expr, errs) -> expr, errs
    in
    let rec property env =
      let open Ast.Expression.Object in
      let start_loc = Peek.loc env in
      if Peek.token env = T_ELLIPSIS
      then begin
        (* Spread property *)
        Expect.token env T_ELLIPSIS;
        let argument, errs = parse_assignment_cover env in
        SpreadProperty (Loc.btwn start_loc (fst argument), SpreadProperty.({
          argument;
        })), errs
      end else begin
        let async = match Peek.token ~i:1 env with
          | T_ASSIGN (* { async = true } (destructuring) *)
          | T_COLON (* { async: true } *)
          | T_LESS_THAN (* { async<T>() {} } *)
          | T_LPAREN (* { async() {} } *)
          | T_COMMA (* { async, other, shorthand } *)
          | T_RCURLY (* { async } *)
            -> false
          | _
            -> Declaration.async env
        in
        let generator = Declaration.generator env in
        match async, generator, Peek.token env with
        | false, false, T_IDENTIFIER { raw = "get"; _ } ->
            let _, key = key env in
            begin match Peek.token env with
            | T_ASSIGN
            | T_COLON
            | T_LESS_THAN
            | T_LPAREN
            | T_COMMA
            | T_RCURLY -> init env start_loc key false false
            | _ -> get env start_loc, Pattern_cover.empty_errors
            end
        | false, false, T_IDENTIFIER { raw = "set"; _ } ->
            let _, key = key env in
            begin match Peek.token env with
            | T_ASSIGN
            | T_COLON
            | T_LESS_THAN
            | T_LPAREN
            | T_COMMA
            | T_RCURLY -> init env start_loc key false false
            | _ -> set env start_loc, Pattern_cover.empty_errors
            end
        | async, generator, _ ->
            let _, key = key env in
            init env start_loc key async generator
      end

    and get env start_loc =
      let key, (end_loc, fn) = getter_or_setter env true in
      let loc = Loc.btwn start_loc end_loc in
      Ast.Expression.Object.(Property (loc, { Property.
        key;
        value = Property.Get (end_loc, fn);
        _method = false;
        shorthand = false;
      }))

    and set env start_loc =
      let key, (end_loc, fn) = getter_or_setter env false in
      let loc = Loc.btwn start_loc end_loc in
      Ast.Expression.Object.(Property (loc, { Property.
        key;
        value = Property.Set (end_loc, fn);
        _method = false;
        shorthand = false;
      }))

    (* #prod-PropertyDefinition *)
    and init =
      let open Ast.Expression.Object.Property in

      (* #prod-IdentifierReference *)
      let parse_shorthand env key = match key with
        | Literal (loc, lit) ->
            error_at env (loc, Parse_error.LiteralShorthandProperty);
            loc, Ast.Expression.Literal lit
        | Identifier ((loc, name) as id) ->
            (* #sec-identifiers-static-semantics-early-errors *)
            begin
              if is_reserved name && name <> "yield" && name <> "await" then
                (* it is a syntax error if `name` is a reserved word other than await or yield *)
                error_at env (loc, Parse_error.UnexpectedReserved)
              else if is_strict_reserved name then
                (* it is a syntax error if `name` is a strict reserved word, in strict mode *)
                strict_error_at env (loc, Parse_error.StrictReservedWord)
            end;
            loc, Ast.Expression.Identifier id
        | PrivateName _ -> failwith "Internal Error: private name found in object props"
        | Computed expr ->
            error_at env (fst expr, Parse_error.ComputedShorthandProperty);
            expr
      in

      (* #prod-MethodDefinition *)
      let parse_method env ~async ~generator =
        let start_loc = Peek.loc env in

        (* #sec-function-definitions-static-semantics-early-errors *)
        let env = env |> with_allow_super Super_prop in

        let typeParameters = Type.type_parameter_declaration env in
        let params =
          let yield, await = match async, generator with
          | true, true -> true, true (* proposal-async-iteration/#prod-AsyncGeneratorMethod *)
          | true, false -> false, allow_await env (* #prod-AsyncMethod *)
          | false, true -> true, false (* #prod-GeneratorMethod *)
          | false, false -> false, false (* #prod-MethodDefinition *)
          in
          Declaration.function_params ~await ~yield env
        in
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

      (* PropertyName `:` AssignmentExpression *)
      let parse_value env =
        Expect.token env T_COLON;
        parse_assignment_cover env
      in

      (* #prod-CoverInitializedName *)
      let parse_assignment_pattern ~key env =
        let open Ast.Expression.Object in

        let left = match key with
        | Property.Literal (loc, lit) -> Some (loc, Ast.Expression.Literal lit)
        | Property.Identifier id -> Some (fst id, Ast.Expression.Identifier id)
        | Property.PrivateName _ -> None
        | Property.Computed _ -> None
        in

        match left with
        | Some left ->
          let assignment_loc = Peek.loc env in
          Expect.token env T_ASSIGN;
          let left = Parse.pattern_from_expr env left in
          let right = Parse.assignment env in
          let loc = Loc.btwn (fst left) (fst right) in
          (loc, Ast.Expression.(Assignment Assignment.({
            operator = Assign;
            left;
            right;
          }))), {
            if_expr = [assignment_loc, Parse_error.UnexpectedToken "="];
            if_patt = [];
          }
        | None ->
          parse_value env
      in

      let parse_init ~key ~async ~generator env =
        if async || generator then
          (* the `async` and `*` modifiers are only valid on methods *)
          parse_method env ~async ~generator, false, true, Pattern_cover.empty_errors
        else match Peek.token env with
        | T_RCURLY
        | T_COMMA ->
          parse_shorthand env key, true, false, Pattern_cover.empty_errors
        | T_LESS_THAN
        | T_LPAREN ->
          parse_method env ~async ~generator, false, true, Pattern_cover.empty_errors
        | T_ASSIGN ->
          let value, errs = parse_assignment_pattern ~key env in
          value, true, false, errs
        | _ ->
          let value, errs = parse_value env in
          value, false, false, errs
      in
      fun env start_loc key async generator ->
        let end_loc, (value, shorthand, _method, errs) = with_loc (
          parse_init ~key ~async ~generator
        ) env in
        Ast.Expression.Object.Property (Loc.btwn start_loc end_loc, {
          key;
          value = Init value;
          _method;
          shorthand;
        }), errs

    and properties env (props, errs) =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev props, Pattern_cover.rev_errors errs
      | _ ->
          let prop, new_errs = property env in
          if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
          let errs = Pattern_cover.rev_append_errors new_errs errs in
          properties env (prop::props, errs)

    in fun env ->
      let loc, (expr, errs) = with_loc (fun env ->
        Expect.token env T_LCURLY;
        let props, errs = properties env ([], Pattern_cover.empty_errors) in
        Expect.token env T_RCURLY;
        { Ast.Expression.Object.properties = props; }, errs
      ) env in
      loc, expr, errs

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
    let rec elements env seen_constructor private_names acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY -> List.rev acc
      | T_SEMICOLON ->
          (* Skip empty elements *)
          Expect.token env T_SEMICOLON;
          elements env seen_constructor private_names acc
      | _ ->
          let element = class_element env in
          let seen_constructor', private_names' = begin match element with
          | Ast.Class.Body.Method (loc, m) ->
              let open Ast.Class.Method in
              begin match m.kind with
              | Constructor when not m.static ->
                  if seen_constructor then
                    error_at env (loc, Error.DuplicateConstructor);
                  (true, private_names)
              | Method ->
                (seen_constructor, begin match m.key with
                | Ast.Expression.Object.Property.PrivateName _ ->
                    error_at env (loc, Error.PrivateMethod);
                    private_names
                | _ -> private_names
                end)
              | _ -> (seen_constructor, private_names)
              end
          | Ast.Class.Body.Property (loc, p) ->
              let open Ast.Expression.Object.Property in
              (seen_constructor, begin match p.Ast.Class.Property.key with
              | Identifier (_, x) when String.equal x "constructor" ||
                (String.equal x "prototype" && p.Ast.Class.Property.static) ->
                  error_at env (loc, Error.InvalidFieldName (x, String.equal x "prototype", false));
                  private_names
              | _ -> private_names
              end)
            | Ast.Class.Body.PrivateField (_, {Ast.Class.PrivateField.key = (loc, (_, name)); _})
              when String.equal name "#constructor" ->
                error_at env (loc, Error.InvalidFieldName (name, false, true));
                (seen_constructor, private_names)
            | Ast.Class.Body.PrivateField (_, {Ast.Class.PrivateField.key = (loc, (_, name)); _}) ->
                if SSet.mem name private_names then
                  error_at env (loc, Error.DuplicatePrivateFields name);
                (seen_constructor, SSet.add name private_names)
          end in
          elements env seen_constructor' private_names' (element::acc)

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      enter_class env;
      let body = elements env false SSet.empty [] in
      exit_class env;
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
                Some (Parse.expression (env |> with_allow_super Super_prop))
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
        begin match key with
        | Ast.Expression.Object.Property.PrivateName private_name ->
          Ast.Class.(Body.PrivateField (loc, PrivateField.({
            key = private_name;
            value;
            typeAnnotation;
            static;
            variance;
          })))
        | _ -> Ast.Class.(Body.Property (loc, Property.({
            key;
            value;
            typeAnnotation;
            static;
            variance;
          }))) end
      | T_PLING ->
        (* TODO: add support for optional class properties *)
        error_unexpected env;
        Eat.token env;
        init env start_loc decorators key async generator static variance
      | _ ->
        error_unsupported_variance env variance;
        let kind, env = match static, key with
          | false, Ast.Expression.Object.Property.Identifier (_, "constructor")
          | false, Ast.Expression.Object.Property.Literal (_, {
              Literal.value = Literal.String "constructor";
              _;
            }) ->
            Ast.Class.Method.Constructor,
            env |> with_allow_super Super_prop_or_call
          | _ ->
            Ast.Class.Method.Method,
            env |> with_allow_super Super_prop
        in
        let func_loc = Peek.loc env in
        let typeParameters = Type.type_parameter_declaration env in
        let params =
          let yield, await = match async, generator with
          | true, true -> true, true (* proposal-async-iteration/#prod-AsyncGeneratorMethod *)
          | true, false -> false, allow_await env (* #prod-AsyncMethod *)
          | false, true -> true, false (* #prod-GeneratorMethod *)
          | false, false -> false, false (* #prod-MethodDefinition *)
          in
          Declaration.function_params ~await ~yield env
        in
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
        Ast.Class.(Body.Method (Loc.btwn start_loc end_loc, Method.({
          key;
          value;
          kind;
          static;
          decorators;
        })))

    in fun env ->
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
      match async, generator, Peek.token env with
      | false, false, T_IDENTIFIER { raw = "get"; _ } ->
          let _, key = key ~class_body:true env in
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
      | false, false, T_IDENTIFIER { raw = "set"; _ } ->
          let _, key = key ~class_body:true env in
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
      | _, _, _ ->
          let _, key = key ~class_body:true env in
          init env start_loc decorators key async generator static variance

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

  let class_expression = with_loc (fun env ->
    (* 10.2.1 says all parts of a class expression are strict *)
    let env = env |> with_strict true in
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
    Ast.Expression.Class { Class.
      id;
      body;
      superClass;
      typeParameters;
      superTypeParameters;
      implements;
      classDecorators=decorators;
    }
  )
end
