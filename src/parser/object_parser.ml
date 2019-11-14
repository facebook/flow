(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Token
open Parser_env
open Flow_ast
module SMap = Map.Make (String)
open Parser_common

(* A module for parsing various object related things, like object literals
 * and classes *)

module type OBJECT = sig
  val key : ?class_body:bool -> env -> Loc.t * (Loc.t, Loc.t) Ast.Expression.Object.Property.key

  val _initializer : env -> Loc.t * (Loc.t, Loc.t) Ast.Expression.Object.t * pattern_errors

  val class_declaration :
    env -> (Loc.t, Loc.t) Ast.Class.Decorator.t list -> (Loc.t, Loc.t) Ast.Statement.t

  val class_expression : env -> (Loc.t, Loc.t) Ast.Expression.t

  val class_implements :
    env -> (Loc.t, Loc.t) Ast.Class.Implements.t list -> (Loc.t, Loc.t) Ast.Class.Implements.t list

  val decorator_list : env -> (Loc.t, Loc.t) Ast.Class.Decorator.t list
end

module Object
    (Parse : Parser_common.PARSER)
    (Type : Type_parser.TYPE)
    (Declaration : Declaration_parser.DECLARATION)
    (Expression : Expression_parser.EXPRESSION)
    (Pattern_cover : Pattern_cover.COVER) : OBJECT = struct
  let decorator_list =
    let decorator env =
      Eat.token env;
      { Ast.Class.Decorator.expression = Expression.left_hand_side env }
    in
    let rec decorator_list_helper env decorators =
      match Peek.token env with
      | T_AT -> decorator_list_helper env (with_loc decorator env :: decorators)
      | _ -> decorators
    in
    fun env ->
      if (parse_options env).esproposal_decorators then
        List.rev (decorator_list_helper env [])
      else
        []

  let key ?(class_body = false) env =
    Ast.Expression.Object.Property.(
      let leading = Peek.comments env in
      let tkn = Peek.token env in
      let trailing = Peek.comments env in
      match tkn with
      | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Parse_error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let value = Literal.String value in
        ( loc,
          Literal
            ( loc,
              {
                Literal.value;
                raw;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              } ) )
      | T_NUMBER { kind; raw } ->
        let loc = Peek.loc env in
        let value = Expression.number env kind raw in
        let value = Literal.Number value in
        ( loc,
          Literal
            ( loc,
              {
                Literal.value;
                raw;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              } ) )
      | T_LBRACKET ->
        with_loc
          (fun env ->
            Expect.token env T_LBRACKET;
            let expr = Parse.assignment (env |> with_no_in false) in
            Expect.token env T_RBRACKET;
            Ast.Expression.Object.Property.Computed expr)
          env
      | T_POUND when class_body ->
        let (loc, id, _is_private) = Expression.property_name_include_private env in
        add_declared_private env (Flow_ast_utils.name_of_ident id);
        (loc, PrivateName (loc, id))
      | _ ->
        let (loc, id, is_private) = Expression.property_name_include_private env in
        if is_private then error_at env (loc, Parse_error.PrivateNotInClass);
        (loc, Identifier id))

  let getter_or_setter env ~in_class_body is_getter =
    (* this is a getter or setter, it cannot be async *)
    let async = false in
    let generator = Declaration.generator env in
    let (key_loc, key) = key ~class_body:in_class_body env in
    let value =
      with_loc
        (fun env ->
          (* #sec-function-definitions-static-semantics-early-errors *)
          let env = env |> with_allow_super Super_prop in
          let (sig_loc, (tparams, params, return)) =
            with_loc
              (fun env ->
                (* It's not clear how type params on getters & setters would make sense
                 * in Flow's type system. Since this is a Flow syntax extension, we might
                 * as well disallow it until we need it *)
                let tparams = None in
                let params = Declaration.function_params ~await:false ~yield:false env in
                begin
                  match (is_getter, params) with
                  | (true, (_, { Ast.Function.Params.params = []; rest = None })) -> ()
                  | (false, (_, { Ast.Function.Params.rest = Some _; _ })) ->
                    (* rest params don't make sense on a setter *)
                    error_at env (key_loc, Parse_error.SetterArity)
                  | (false, (_, { Ast.Function.Params.params = [_]; _ })) -> ()
                  | (true, _) -> error_at env (key_loc, Parse_error.GetterArity)
                  | (false, _) -> error_at env (key_loc, Parse_error.SetterArity)
                end;
                let return = Type.annotation_opt env in
                (tparams, params, return))
              env
          in
          let (body, strict) = Declaration.function_body env ~async ~generator in
          let simple = Declaration.is_simple_function_params params in
          Declaration.strict_post_check env ~strict ~simple None params;
          {
            Function.id = None;
            params;
            body;
            generator;
            async;
            predicate = None;
            (* setters/getter are not predicates *)
            return;
            tparams;
            sig_loc;
          })
        env
    in
    (key, value)

  let _initializer =
    let parse_assignment_cover env =
      match Expression.assignment_cover env with
      | Cover_expr expr -> (expr, Pattern_cover.empty_errors)
      | Cover_patt (expr, errs) -> (expr, errs)
    in
    let get env start_loc =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:false true) env
      in
      Ast.Expression.Object.(Property (loc, Property.Get { key; value }))
    in
    let set env start_loc =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:false false) env
      in
      Ast.Expression.Object.(Property (loc, Property.Set { key; value }))
    in
    (* #prod-PropertyDefinition *)
    let init =
      Ast.Expression.Object.Property.(
        (* #prod-IdentifierReference *)
        let parse_shorthand env key =
          match key with
          | Literal (loc, lit) ->
            error_at env (loc, Parse_error.LiteralShorthandProperty);
            (loc, Ast.Expression.Literal lit)
          | Identifier ((loc, { Identifier.name; comments = _ }) as id) ->
            (* #sec-identifiers-static-semantics-early-errors *)
            if is_reserved name && name <> "yield" && name <> "await" then
              (* it is a syntax error if `name` is a reserved word other than await or yield *)
              error_at env (loc, Parse_error.UnexpectedReserved)
            else if is_strict_reserved name then
              (* it is a syntax error if `name` is a strict reserved word, in strict mode *)
              strict_error_at env (loc, Parse_error.StrictReservedWord);
            (loc, Ast.Expression.Identifier id)
          | PrivateName _ -> failwith "Internal Error: private name found in object props"
          | Computed expr ->
            error_at env (fst expr, Parse_error.ComputedShorthandProperty);
            expr
        in
        (* #prod-MethodDefinition *)
        let parse_method ~async ~generator =
          with_loc (fun env ->
              (* #sec-function-definitions-static-semantics-early-errors *)
              let env = env |> with_allow_super Super_prop in
              let (sig_loc, (tparams, params, return)) =
                with_loc
                  (fun env ->
                    let tparams = Type.type_params env in
                    let params =
                      let (yield, await) =
                        match (async, generator) with
                        | (true, true) ->
                          (true, true) (* proposal-async-iteration/#prod-AsyncGeneratorMethod *)
                        | (true, false) -> (false, allow_await env) (* #prod-AsyncMethod *)
                        | (false, true) -> (true, false) (* #prod-GeneratorMethod *)
                        | (false, false) -> (false, false)
                        (* #prod-MethodDefinition *)
                      in
                      Declaration.function_params ~await ~yield env
                    in
                    let return = Type.annotation_opt env in
                    (tparams, params, return))
                  env
              in
              let (body, strict) = Declaration.function_body env ~async ~generator in
              let simple = Declaration.is_simple_function_params params in
              Declaration.strict_post_check env ~strict ~simple None params;
              {
                Function.id = None;
                params;
                body;
                generator;
                async;
                (* TODO: add support for object method predicates *)
                predicate = None;
                return;
                tparams;
                sig_loc;
              })
        in
        (* PropertyName `:` AssignmentExpression *)
        let parse_value env =
          Expect.token env T_COLON;
          parse_assignment_cover env
        in
        (* #prod-CoverInitializedName *)
        let parse_assignment_pattern ~key env =
          Ast.Expression.Object.(
            match key with
            | Property.Identifier id ->
              let assignment_loc = Peek.loc env in
              let ast =
                with_loc
                  ~start_loc:(fst id)
                  (fun env ->
                    Expect.token env T_ASSIGN;
                    let left = Parse.pattern_from_expr env (fst id, Ast.Expression.Identifier id) in
                    let right = Parse.assignment env in
                    Ast.Expression.Assignment
                      { Ast.Expression.Assignment.operator = None; left; right })
                  env
              in
              let errs =
                {
                  if_expr = [(assignment_loc, Parse_error.Unexpected (Token.quote_token_value "="))];
                  if_patt = [];
                }
              in
              (ast, errs)
            | Property.Literal _
            | Property.PrivateName _
            | Property.Computed _ ->
              parse_value env)
        in
        let parse_init ~key ~async ~generator env =
          if async || generator then
            (* the `async` and `*` modifiers are only valid on methods *)
            let value = parse_method env ~async ~generator in
            let prop = Method { key; value } in
            (prop, Pattern_cover.empty_errors)
          else
            match Peek.token env with
            | T_RCURLY
            | T_COMMA ->
              let value = parse_shorthand env key in
              let prop = Init { key; value; shorthand = true } in
              (prop, Pattern_cover.empty_errors)
            | T_LESS_THAN
            | T_LPAREN ->
              let value = parse_method env ~async ~generator in
              let prop = Method { key; value } in
              (prop, Pattern_cover.empty_errors)
            | T_ASSIGN ->
              let (value, errs) = parse_assignment_pattern ~key env in
              let prop = Init { key; value; shorthand = true } in
              (prop, errs)
            | _ ->
              let (value, errs) = parse_value env in
              let prop = Init { key; value; shorthand = false } in
              (prop, errs)
        in
        fun env start_loc key async generator ->
          let (loc, (prop, errs)) = with_loc ~start_loc (parse_init ~key ~async ~generator) env in
          (Ast.Expression.Object.Property (loc, prop), errs))
    in
    let property env =
      Ast.Expression.Object.(
        if Peek.token env = T_ELLIPSIS then
          (* Spread property *)
          let (loc, (argument, errs)) =
            with_loc
              (fun env ->
                Expect.token env T_ELLIPSIS;
                parse_assignment_cover env)
              env
          in
          (SpreadProperty (loc, { SpreadProperty.argument }), errs)
        else
          let start_loc = Peek.loc env in
          let async =
            match Peek.ith_token ~i:1 env with
            | T_ASSIGN
            (* { async = true } (destructuring) *)
            | T_COLON
            (* { async: true } *)
            | T_LESS_THAN
            (* { async<T>() {} } *)
            | T_LPAREN
            (* { async() {} } *)
            | T_COMMA
            (* { async, other, shorthand } *)
            | T_RCURLY (* { async } *) ->
              false
            | _ -> Declaration.async env
          in
          let generator = Declaration.generator env in
          match (async, generator, Peek.token env) with
          | (false, false, T_IDENTIFIER { raw = "get"; _ }) ->
            let (_, key) = key env in
            begin
              match Peek.token env with
              | T_ASSIGN
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN
              | T_COMMA
              | T_RCURLY ->
                init env start_loc key false false
              | _ -> (get env start_loc, Pattern_cover.empty_errors)
            end
          | (false, false, T_IDENTIFIER { raw = "set"; _ }) ->
            let (_, key) = key env in
            begin
              match Peek.token env with
              | T_ASSIGN
              | T_COLON
              | T_LESS_THAN
              | T_LPAREN
              | T_COMMA
              | T_RCURLY ->
                init env start_loc key false false
              | _ -> (set env start_loc, Pattern_cover.empty_errors)
            end
          | (async, generator, _) ->
            let (_, key) = key env in
            init env start_loc key async generator)
    in
    let rec properties env ~rest_trailing_comma (props, errs) =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        let errs =
          match rest_trailing_comma with
          | Some loc ->
            { errs with if_patt = (loc, Parse_error.TrailingCommaAfterRestElement) :: errs.if_patt }
          | None -> errs
        in
        (List.rev props, Pattern_cover.rev_errors errs)
      | _ ->
        let (prop, new_errs) = property env in
        let rest_trailing_comma =
          match prop with
          | Ast.Expression.Object.SpreadProperty _ when Peek.token env = T_COMMA ->
            Some (Peek.loc env)
          | _ -> None
        in
        if Peek.token env <> T_RCURLY then Expect.token env T_COMMA;
        let errs = Pattern_cover.rev_append_errors new_errs errs in
        properties env ~rest_trailing_comma (prop :: props, errs)
    in
    fun env ->
      let (loc, (expr, errs)) =
        with_loc
          (fun env ->
            let leading = Peek.comments env in
            Expect.token env T_LCURLY;
            let (props, errs) =
              properties env ~rest_trailing_comma:None ([], Pattern_cover.empty_errors)
            in
            Expect.token env T_RCURLY;
            let trailing = Peek.comments env in
            ( {
                Ast.Expression.Object.properties = props;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              },
              errs ))
          env
      in
      (loc, expr, errs)

  let check_property_name env loc name static =
    if String.equal name "constructor" || (String.equal name "prototype" && static) then
      error_at env (loc, Parse_error.InvalidFieldName { name; static; private_ = false })

  let check_private_names env seen_names private_name (kind : [ `Field | `Getter | `Setter ]) =
    let (loc, (_, { Identifier.name; comments = _ })) = private_name in
    if String.equal name "constructor" then
      let () =
        error_at env (loc, Parse_error.InvalidFieldName { name; static = false; private_ = true })
      in
      seen_names
    else
      match SMap.find_opt name seen_names with
      | Some seen ->
        begin
          match (kind, seen) with
          | (`Getter, `Setter)
          | (`Setter, `Getter) ->
            (* one getter and one setter are allowed as long as it's not used as a field *)
            ()
          | _ -> error_at env (loc, Parse_error.DuplicatePrivateFields name)
        end;
        SMap.add name `Field seen_names
      | None -> SMap.add name kind seen_names

  let rec class_implements env acc =
    let implement =
      with_loc
        (fun env ->
          let id = Type.type_identifier env in
          let targs = Type.type_args env in
          { Ast.Class.Implements.id; targs })
        env
    in
    let acc = implement :: acc in
    match Peek.token env with
    | T_COMMA ->
      Expect.token env T_COMMA;
      class_implements env acc
    | _ -> List.rev acc

  let class_extends =
    with_loc (fun env ->
        let expr = Expression.left_hand_side (env |> with_allow_yield false) in
        let targs = Type.type_args env in
        { Class.Extends.expr; targs })

  (* https://tc39.es/ecma262/#prod-ClassHeritage *)
  let class_heritage env =
    let extends =
      if Expect.maybe env T_EXTENDS then
        Some (class_extends env)
      else
        None
    in
    let implements =
      if Peek.token env = T_IMPLEMENTS then (
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeInterface;
        Expect.token env T_IMPLEMENTS;
        class_implements env []
      ) else
        []
    in
    (extends, implements)

  (* In the ES6 draft, all elements are methods. No properties (though there
   * are getter and setters allowed *)
  let class_element =
    let get env start_loc decorators static =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:true true) env
      in
      Ast.Class.(Body.Method (loc, { Method.key; value; kind = Method.Get; static; decorators }))
    in
    let set env start_loc decorators static =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:true false) env
      in
      Ast.Class.(Body.Method (loc, { Method.key; value; kind = Method.Set; static; decorators }))
    in
    let error_unsupported_variance env = function
      | Some (loc, _) -> error_at env (loc, Parse_error.UnexpectedVariance)
      | None -> ()
      (* Class property with annotation *)
    in
    let property env start_loc key static variance =
      let (loc, (annot, value)) =
        with_loc
          ~start_loc
          (fun env ->
            let annot = Type.annotation_opt env in
            let options = parse_options env in
            let value =
              if Peek.token env = T_ASSIGN then
                if
                  (static && options.esproposal_class_static_fields)
                  || ((not static) && options.esproposal_class_instance_fields)
                then (
                  Expect.token env T_ASSIGN;
                  Some (Parse.expression (env |> with_allow_super Super_prop))
                ) else
                  None
              else
                None
            in
            if Expect.maybe env T_SEMICOLON then
              ()
            else if Peek.token env == T_LBRACKET || Peek.token env == T_LPAREN then
              error_unexpected env;
            (annot, value))
          env
      in
      match key with
      | Ast.Expression.Object.Property.PrivateName private_name ->
        Ast.Class.(
          Body.PrivateField
            (loc, { PrivateField.key = private_name; value; annot; static; variance }))
      | _ -> Ast.Class.(Body.Property (loc, { Property.key; value; annot; static; variance }))
    in
    let rec init env start_loc decorators key async generator static variance =
      match Peek.token env with
      | T_COLON
      | T_ASSIGN
      | T_SEMICOLON
      | T_RCURLY
        when (not async) && not generator ->
        property env start_loc key static variance
      | T_PLING ->
        (* TODO: add support for optional class properties *)
        error_unexpected env;
        Eat.token env;
        init env start_loc decorators key async generator static variance
      | _ when Peek.is_implicit_semicolon env ->
        (* an uninitialized, unannotated property *)
        property env start_loc key static variance
      | _ ->
        error_unsupported_variance env variance;
        let (kind, env) =
          match (static, key) with
          | ( false,
              Ast.Expression.Object.Property.Identifier
                (_, { Identifier.name = "constructor"; comments = _ }) )
          | ( false,
              Ast.Expression.Object.Property.Literal
                (_, { Literal.value = Literal.String "constructor"; _ }) ) ->
            (Ast.Class.Method.Constructor, env |> with_allow_super Super_prop_or_call)
          | _ -> (Ast.Class.Method.Method, env |> with_allow_super Super_prop)
        in
        let value =
          with_loc
            (fun env ->
              let (sig_loc, (tparams, params, return)) =
                with_loc
                  (fun env ->
                    let tparams = Type.type_params env in
                    let params =
                      let (yield, await) =
                        match (async, generator) with
                        | (true, true) ->
                          (true, true) (* proposal-async-iteration/#prod-AsyncGeneratorMethod *)
                        | (true, false) -> (false, allow_await env) (* #prod-AsyncMethod *)
                        | (false, true) -> (true, false) (* #prod-GeneratorMethod *)
                        | (false, false) -> (false, false)
                        (* #prod-MethodDefinition *)
                      in
                      Declaration.function_params ~await ~yield env
                    in
                    let return = Type.annotation_opt env in
                    (tparams, params, return))
                  env
              in
              let (body, strict) = Declaration.function_body env ~async ~generator in
              let simple = Declaration.is_simple_function_params params in
              Declaration.strict_post_check env ~strict ~simple None params;
              {
                Function.id = None;
                params;
                body;
                generator;
                async;
                (* TODO: add support for method predicates *)
                predicate = None;
                return;
                tparams;
                sig_loc;
              })
            env
        in
        Ast.Class.(
          Body.Method
            (Loc.btwn start_loc (fst value), { Method.key; value; kind; static; decorators }))
    in
    let ith_implies_identifier ~i env =
      match Peek.ith_token ~i env with
      | T_LESS_THAN
      | T_COLON
      | T_ASSIGN
      | T_SEMICOLON
      | T_LPAREN
      | T_RCURLY ->
        true
      | _ -> false
    in
    let implies_identifier = ith_implies_identifier ~i:0 in
    fun env ->
      let start_loc = Peek.loc env in
      let decorators = decorator_list env in
      let static =
        Peek.ith_token ~i:1 env <> T_LPAREN
        && Peek.ith_token ~i:1 env <> T_LESS_THAN
        && Expect.maybe env T_STATIC
      in
      let async =
        Peek.token env = T_ASYNC
        && (not (ith_implies_identifier ~i:1 env))
        && not (Peek.ith_is_line_terminator ~i:1 env)
      in
      if async then Eat.token env;

      (* consume `async` *)
      let generator = Declaration.generator env in
      let variance = Declaration.variance env async generator in
      let generator =
        match (generator, variance) with
        | (false, Some _) -> Declaration.generator env
        | _ -> generator
      in
      match (async, generator, Peek.token env) with
      | (false, false, T_IDENTIFIER { raw = "get"; _ }) ->
        let (_, key) = key ~class_body:true env in
        if implies_identifier env then
          init env start_loc decorators key async generator static variance
        else (
          error_unsupported_variance env variance;
          get env start_loc decorators static
        )
      | (false, false, T_IDENTIFIER { raw = "set"; _ }) ->
        let (_, key) = key ~class_body:true env in
        if implies_identifier env then
          init env start_loc decorators key async generator static variance
        else (
          error_unsupported_variance env variance;
          set env start_loc decorators static
        )
      | (_, _, _) ->
        let (_, key) = key ~class_body:true env in
        init env start_loc decorators key async generator static variance

  let class_body =
    let rec elements env seen_constructor private_names acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        List.rev acc
      | T_SEMICOLON ->
        (* Skip empty elements *)
        Expect.token env T_SEMICOLON;
        elements env seen_constructor private_names acc
      | _ ->
        let element = class_element env in
        let (seen_constructor', private_names') =
          match element with
          | Ast.Class.Body.Method (loc, m) ->
            Ast.Class.Method.(
              begin
                match m.kind with
                | Constructor ->
                  if m.static then
                    (seen_constructor, private_names)
                  else (
                    if seen_constructor then error_at env (loc, Parse_error.DuplicateConstructor);
                    (true, private_names)
                  )
                | Method ->
                  ( seen_constructor,
                    begin
                      match m.key with
                      | Ast.Expression.Object.Property.PrivateName _ ->
                        error_at env (loc, Parse_error.PrivateMethod);
                        private_names
                      | _ -> private_names
                    end )
                | Get ->
                  let private_names =
                    match m.key with
                    | Ast.Expression.Object.Property.PrivateName name ->
                      check_private_names env private_names name `Getter
                    | _ -> private_names
                  in
                  (seen_constructor, private_names)
                | Set ->
                  let private_names =
                    match m.key with
                    | Ast.Expression.Object.Property.PrivateName name ->
                      check_private_names env private_names name `Setter
                    | _ -> private_names
                  in
                  (seen_constructor, private_names)
              end)
          | Ast.Class.Body.Property (_, { Ast.Class.Property.key; static; _ }) ->
            Ast.Expression.Object.Property.(
              begin
                match key with
                | Identifier (loc, { Identifier.name; comments = _ })
                | Literal (loc, { Literal.value = Literal.String name; _ }) ->
                  check_property_name env loc name static
                | Literal _
                | Computed _ ->
                  ()
                | PrivateName _ ->
                  failwith "unexpected PrivateName in Property, expected a PrivateField"
              end;
              (seen_constructor, private_names))
          | Ast.Class.Body.PrivateField (_, { Ast.Class.PrivateField.key; _ }) ->
            let private_names = check_private_names env private_names key `Field in
            (seen_constructor, private_names)
        in
        elements env seen_constructor' private_names' (element :: acc)
    in
    fun env ->
      with_loc
        (fun env ->
          if Expect.maybe env T_LCURLY then (
            enter_class env;
            let body = elements env false SMap.empty [] in
            exit_class env;
            Expect.token env T_RCURLY;
            { Ast.Class.Body.body }
          ) else (
            Expect.error env T_LCURLY;
            { Ast.Class.Body.body = [] }
          ))
        env

  let _class ?(decorators = []) env ~optional_id =
    (* 10.2.1 says all parts of a class definition are strict *)
    let env = env |> with_strict true in
    let decorators = decorators @ decorator_list env in
    let leading = Peek.comments env in
    Expect.token env T_CLASS;
    let id =
      let tmp_env = env |> with_no_let true in
      match (optional_id, Peek.token tmp_env) with
      | (true, (T_EXTENDS | T_IMPLEMENTS | T_LESS_THAN | T_LCURLY)) -> None
      | _ -> Some (Parse.identifier tmp_env)
    in
    let tparams = Type.type_params env in
    let (extends, implements) = class_heritage env in
    let body = class_body env in
    let trailing = Peek.comments env in
    let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
    (id, body, extends, implements, tparams, decorators, comments)

  let class_declaration env decorators =
    with_loc
      (fun env ->
        let optional_id = in_export env in
        let (id, body, extends, implements, tparams, decorators, comments) =
          _class env ~decorators ~optional_id
        in
        Ast.Statement.ClassDeclaration
          { Class.id; body; tparams; extends; implements; classDecorators = decorators; comments })
      env

  let class_expression =
    with_loc (fun env ->
        let (id, body, extends, implements, tparams, decorators, comments) =
          _class env ~optional_id:true
        in
        Ast.Expression.Class
          { Class.id; body; tparams; extends; implements; classDecorators = decorators; comments })
end
