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
        let async = match Peek.ith_token ~i:1 env with
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
      Ast.Expression.Object.(Property (loc, Property.Get {
        key;
        value = (end_loc, fn);
      }))

    and set env start_loc =
      let key, (end_loc, fn) = getter_or_setter env false in
      let loc = Loc.btwn start_loc end_loc in
      Ast.Expression.Object.(Property (loc, Property.Set {
        key;
        value = (end_loc, fn);
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
        loc, Function.({
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
        })
      in

      (* PropertyName `:` AssignmentExpression *)
      let parse_value env =
        Expect.token env T_COLON;
        parse_assignment_cover env
      in

      (* #prod-CoverInitializedName *)
      let parse_assignment_pattern ~key env =
        let open Ast.Expression.Object in
        match key with
        | Property.Identifier id ->
          let assignment_loc = Peek.loc env in
          Expect.token env T_ASSIGN;
          let left = Parse.pattern_from_expr env (fst id, Ast.Expression.Identifier id) in
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

        | Property.Literal _
        | Property.PrivateName _
        | Property.Computed _ ->
          parse_value env
      in

      let parse_init ~key ~async ~generator env =
        if async || generator then
          (* the `async` and `*` modifiers are only valid on methods *)
          let value = parse_method env ~async ~generator in
          let prop = Method { key; value } in
          prop, Pattern_cover.empty_errors
        else match Peek.token env with
        | T_RCURLY
        | T_COMMA ->
          let value = parse_shorthand env key in
          let prop = Init { key; value; shorthand = true } in
          prop, Pattern_cover.empty_errors
        | T_LESS_THAN
        | T_LPAREN ->
          let value = parse_method env ~async ~generator in
          let prop = Method { key; value } in
          prop, Pattern_cover.empty_errors
        | T_ASSIGN ->
          let value, errs = parse_assignment_pattern ~key env in
          let prop = Init { key; value; shorthand = true } in
          prop, errs
        | _ ->
          let value, errs = parse_value env in
          let prop = Init { key; value; shorthand = false } in
          prop, errs
      in
      fun env start_loc key async generator ->
        let end_loc, (prop, errs) = with_loc (
          parse_init ~key ~async ~generator
        ) env in
        Ast.Expression.Object.Property (Loc.btwn start_loc end_loc, prop), errs

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
    let id = Type.type_identifier env in
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
      | T_RCURLY -> acc
      | T_SEMICOLON ->
          (* Skip empty elements *)
          Expect.token env T_SEMICOLON;
          elements env acc
      | _ ->
          elements env (class_element env acc)

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      enter_class env;
      let body = Object_members.members (elements env Object_members.empty) in
      exit_class env;
      let end_loc = Peek.loc env in
      Expect.token env T_RCURLY;
      Loc.btwn start_loc end_loc, Ast.Class.Body.({
        body;
      })

  (* In the ES6 draft, all elements are methods. No properties (though there
   * are getter and setters allowed *)
  and class_element =
    let module Members = struct
      include Object_members.Make(struct
        type internal_t = Loc.t Class.Body.element

        module AbstractMethod = Object_members.AbstractMethod(struct
          type t = Loc.t Ast.Class.AbstractMethod.t
          type internal_t = Loc.t Ast.Class.Body.element
          let loc (loc, _) = loc
          let name (_, m) = Some (snd m.Ast.Class.AbstractMethod.key)
          let is_static (_, m) = m.Ast.Class.AbstractMethod.static
          let intern t = Ast.Class.Body.AbstractMethod t
        end)
        module Method = struct
          include Object_members.Method(struct
            type t = Loc.t Ast.Class.Method.t
            type internal_t = Loc.t Ast.Class.Body.element
            let loc (loc, _) = loc
            let name (_, m) = Object_members.property_name m.Ast.Class.Method.key
            let is_static (_, m) = m.Ast.Class.Method.static
            let intern t = Ast.Class.Body.Method t
          end)
          let add env variance t marks =
            let open Ast.Expression.Object.Property in
            match t with
            | loc, {Class.Method.key = PrivateName _; _} ->
                error_at env (loc, Error.PrivateMethod);
                marks
            | loc, {Class.Method.kind = Class.Method.Constructor; _} ->
                let open Object_members in
                let name = "constructor" in
                begin match SMap.get name marks with
                | Some Native ->
                    error_at env (loc, Error.DuplicateConstructor);
                    marks
                | _ ->
                   SMap.add name Native marks
                end
            | _ -> add env variance t marks
        end
        module Property = struct
          include Object_members.Property(struct
            type t = Loc.t Class.Property.t
            type internal_t = Loc.t Ast.Class.Body.element
            let loc (loc, _) = loc
            let name (_, p) = Object_members.property_name p.Class.Property.key
            let is_static (_, p) = p.Class.Property.static
            let intern t = Ast.Class.Body.Property t
          end)
          let add env abstract t marks =
            let open Ast.Expression.Object.Property in
            let (loc, p) = t in
            Class.(match p with
            | {Property.key = Identifier (_, "constructor"); static; _} ->
                let e = Error.InvalidFieldName ("constructor", static, false) in
                error_at env (loc, e);
                marks
            | {Property.key = Identifier (_, "prototype"); static = true; _} ->
                let e = Error.InvalidFieldName ("prototype", true, false) in
                error_at env (loc, e);
                marks
            | _ -> add env abstract t marks)
        end
        module Getter = struct
          include Object_members.Getter(struct
            type t = Loc.t Ast.Class.Method.t
            type internal_t = Loc.t Ast.Class.Body.element
            let loc (loc, _) = loc
            let name (_, m) = Object_members.property_name m.Ast.Class.Method.key
            let is_static (_, m) = m.Ast.Class.Method.static
            let intern t = Ast.Class.Body.Method t
          end)
          let add env abstract variance t marks =
            let open Ast.Expression.Object.Property in
            match t with
            | loc, {Class.Method.key = PrivateName _; _} ->
                error_at env (loc, Error.PrivateGetter);
                marks
            | _ -> add env abstract variance t marks
        end
        module Setter = struct
          include Object_members.Setter(struct
            type t = Loc.t Ast.Class.Method.t
            type internal_t = Loc.t Ast.Class.Body.element
            let loc (loc, _) = loc
            let name (_, m) = Object_members.property_name m.Ast.Class.Method.key
            let is_static (_, m) = m.Ast.Class.Method.static
            let intern t = Ast.Class.Body.Method t
          end)
          let add env abstract variance t marks =
            let open Ast.Expression.Object.Property in
            match t with
            | loc, {Class.Method.key = PrivateName _; _} ->
                error_at env (loc, Error.PrivateSetter);
                marks
            | _ -> add env abstract variance t marks
        end
      end)

      let add_private_property env abstract (p: Object_members.PrivateField.t) t =
        if abstract <> None then error_at env (fst p, Error.AbstractPrivate);
        let open Object_members.PrivateField in
        let f = add env abstract p in
        t |> update_privates f |> add_member (intern p)
    end

    in let class_get env start decorators static =
      let key, (end_loc, _ as value) = getter_or_setter env true in
      Loc.btwn start end_loc, Ast.Class.Method.({
        key;
        value;
        kind = Get;
        static = static <> None;
        decorators;
      })

    in let class_set env start decorators static =
      let key, (end_loc, _ as value) = getter_or_setter env false in
      Loc.btwn start end_loc, Ast.Class.Method.({
        key;
        value;
        kind = Set;
        static = static <> None;
        decorators;
      })

    in let class_abstract_method env start keywords generator key =
      let (abstract, static, async) = keywords in
      assert(abstract <> None);
      let value = with_loc (fun env ->
        let typeParameters = Type.type_parameter_declaration env in
        let params = Type.function_param_list env in
        Expect.token env T_COLON;
        let returnType = Type._type env in
        ignore (Expect.maybe env T_SEMICOLON);
        Ast.Type.Function.({
          params;
          async = async <> None;
          generator;
          returnType;
          typeParameters;
        })
      ) env in
      Loc.btwn start (fst value), Class.AbstractMethod.({
        key;
        value;
        static = static <> None;
      })

    in let class_method env start decors keywords generator key =
      let (abstract, static, async) = keywords in
      assert(abstract = None);
      let kind, env = match static, key with
        | None, Ast.Expression.Object.Property.Identifier (_, "constructor")
        | None, Ast.Expression.Object.Property.Literal (_, {
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
        | Some _, true -> true, true (* proposal-async-iteration/#prod-AsyncGeneratorMethod *)
        | Some _, false -> false, allow_await env (* #prod-AsyncMethod *)
        | None, true -> true, false (* #prod-GeneratorMethod *)
        | None, false -> false, false (* #prod-MethodDefinition *)
        in
        Declaration.function_params ~await ~yield env
      in
      let returnType = Type.annotation_opt env in
      let _, body, strict =
        Declaration.function_body env ~async:(async <> None) ~generator in
      let simple = Declaration.is_simple_function_params params in
      Declaration.strict_post_check env ~strict ~simple None params;
      let end_loc, expression = Function.(
        match body with
        | BodyBlock (loc, _) -> loc, false
        | BodyExpression (loc, _) -> loc, true) in
      let value = Loc.btwn func_loc end_loc, Function.({
        id = None;
        params;
        body;
        generator;
        async = async <> None;
        (* TODO: add support for method predicates *)
        predicate = None;
        expression;
        returnType;
        typeParameters;
      }) in
      Loc.btwn start end_loc, Ast.Class.Method.({
        key;
        value;
        kind;
        static = static <> None;
        decorators = decors;
      })

    in let class_property_value env static = with_loc (fun env ->
      let typeAnnotation = Type.annotation_opt env in
      let options = parse_options env in
      let value =
        if Peek.token env = T_ASSIGN then (
          if (static <> None) && options.esproposal_class_static_fields
             || (static = None) && options.esproposal_class_instance_fields
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
      typeAnnotation, value) env

    in let init env start decors keywords generator variance k acc =
      let (abstract, static, async) = keywords in
      let key = match k with
        | Some id ->
            let _, name = id in
            assert (List.mem name ["abstract";"static";"async";"get";"set"]);
            Ast.Expression.Object.Property.Identifier id
        | None ->
            let id_loc = Peek.loc env in
            Ast.Expression.Object.Property.(match key ~class_body:true env with
            | _, (Identifier _ as key)
            | _, (PrivateName _ as key) ->
                key
            | _, key ->
                if abstract <> None then
                  error_at env (id_loc, Error.ExpectedIdentifier);
                key)
      in
      if Peek.token env == T_PLING then (
        (* TODO: add support for optional class properties *)
        error_unexpected env;
        Eat.token env
      );
      match Peek.token env with
      | T_COLON
      | T_ASSIGN
      | T_SEMICOLON ->
          if async <> None then error_unexpected env;
          if generator then error_unexpected env;
          let end_loc, (annotation, value) = class_property_value env static in
          let loc = Loc.btwn start end_loc in
          begin match key with
          | Ast.Expression.Object.Property.PrivateName private_name ->
              let p = loc, Ast.Class.PrivateField.({
                key = private_name;
                value;
                typeAnnotation = annotation;
                static = static <> None;
                variance;
              }) in
              Members.add_private_property env abstract p acc
          | _ ->
              let p = loc, Ast.Class.Property.({
                key;
                value;
                typeAnnotation = annotation;
                static = static <> None;
                variance;
              }) in
              Members.add_property env abstract p acc
          end
      | T_LESS_THAN
      | T_LPAREN ->
          begin match abstract, key with
          | Some _, Ast.Expression.Object.Property.Identifier key ->
              let m = class_abstract_method env start keywords generator key in
              Members.add_abstract_method env variance m acc
          | Some _, Ast.Expression.Object.Property.PrivateName (_, key) ->
              let m = class_abstract_method env start keywords generator key in
              error_at env (fst m, Error.AbstractPrivate);
              Members.add_abstract_method env variance m acc
          | _ ->
              (* Given an abstract method with some non-identifier key, the
                 `AbstractNonidentifier` error has already been handled. Parse
                 such an abstract method as a non-abstract method alongside
                 non-abstract methods. *)
              let m = class_method env start decors keywords generator key in
              Members.add_method env variance m acc
          end
      | _ ->
          error_unexpected env;
          Eat.token env;
          acc

    in let is_prior_init_key env =
      match Peek.token env with
      | T_LESS_THAN | T_LPAREN
      | T_PLING | T_COLON
      | T_ASSIGN
      | T_SEMICOLON ->
          true
      | _ -> false

    in let prelude_keywords env decors =
      let id_opt name = function
        | Some loc -> Some (loc, name)
        | None -> None
      in
      let loc = Peek.loc env in
      let abstract = if Expect.maybe env T_ABSTRACT then Some loc else None in
      if is_prior_init_key env then
        ((None, None, None), id_opt "abstract" abstract)
      else begin
        begin match decors, abstract with
        | _::_, Some loc ->
            error_at env (loc, Error.DecoratorOnAbstract)
        | _ -> ()
        end;
        let loc = Peek.loc env in
        let static = if Expect.maybe env T_STATIC then Some loc else None in
        if is_prior_init_key env then
          ((abstract, None, None), id_opt "static" static)
        else
          let loc = Peek.loc env in
          let async = if Expect.maybe env T_ASYNC then Some loc else None in
          if is_prior_init_key env then
            ((abstract, static, None), id_opt "async" async)
          else
            ((abstract, static, async), None)
      end

    in fun env acc ->
      let start = Peek.loc env in
      let decors = decorator_list env in
      let (keywords, key) = prelude_keywords env decors in
      match key with
      | Some _ ->
          let generator = false in
          let variance = None in
          init env start decors keywords generator variance key acc
      | None ->
          let (abstract, static, async) = keywords in
          let generator = Declaration.generator env in
          let variance = Declaration.variance env (async <> None) generator in
          let generator = match generator, variance with
            | false, Some _ -> Declaration.generator env
            | _ -> generator
          in
          match Peek.token env with
          | T_IDENTIFIER { raw = "get" | "set" as name; _ } ->
              let name_loc = Peek.loc env in
              Eat.token env;
              begin match name, is_prior_init_key env with
              | "get", false ->
                  if (async <> None) || generator then error_unexpected env;
                  let m = class_get env start decors static in
                  Members.add_getter env abstract variance m acc
              | "set", false ->
                  if (async <> None) || generator then error_unexpected env;
                  let m = class_set env start decors static in
                  Members.add_setter env abstract variance m acc
              | _ ->
                  let key = Some (name_loc, name) in
                  init env start decors keywords generator variance key acc
              end
          | _ ->
              init env start decors keywords generator variance key acc

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
