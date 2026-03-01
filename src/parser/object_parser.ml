(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_env
open Flow_ast
module SMap = Flow_map.Make (String)
open Parser_common
open Comment_attachment

(* A module for parsing various object related things, like object literals
 * and classes *)

module Object
    (Parse : Parser_common.PARSER)
    (Type : Parser_common.TYPE)
    (Declaration : Parser_common.DECLARATION)
    (Expression : Parser_common.EXPRESSION)
    (Pattern_cover : Parser_common.COVER) : Parser_common.OBJECT = struct
  let decorator_list =
    let expression env =
      let expression = Expression.left_hand_side env in
      let { remove_trailing; _ } =
        if Peek.is_line_terminator env then
          trailing_and_remover_after_last_line env
        else
          trailing_and_remover_after_last_loc env
      in
      remove_trailing expression (fun remover expression -> remover#expression expression)
    in
    let decorator env =
      let leading = Peek.comments env in
      Eat.token env;
      {
        Ast.Class.Decorator.expression = expression env;
        comments = Flow_ast_utils.mk_comments_opt ~leading ();
      }
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
    let open Ast.Expression.Object.Property in
    let leading = Peek.comments env in
    let tkn = Peek.token env in
    match tkn with
    | T_STRING (loc, value, raw, octal) ->
      if octal then strict_error env Parse_error.StrictOctalLiteral;
      Expect.token env (T_STRING (loc, value, raw, octal));
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, StringLiteral (loc, { Ast.StringLiteral.value; raw; comments }))
    | T_NUMBER { kind; raw } ->
      let loc = Peek.loc env in
      let value = Expression.number env kind raw in
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, NumberLiteral (loc, { Ast.NumberLiteral.value; raw; comments }))
    | T_BIGINT { kind; raw } ->
      let loc = Peek.loc env in
      let value = Expression.bigint env kind raw in
      let trailing = Eat.trailing_comments env in
      let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
      (loc, BigIntLiteral (loc, { Ast.BigIntLiteral.value; raw; comments }))
    | T_LBRACKET ->
      let (loc, key) =
        with_loc
          (fun env ->
            let leading = Peek.comments env in
            Expect.token env T_LBRACKET;
            let expr = Parse.assignment (env |> with_no_in false) in
            Expect.token env T_RBRACKET;
            let trailing = Eat.trailing_comments env in
            {
              ComputedKey.expression = expr;
              comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
            })
          env
      in
      (loc, Ast.Expression.Object.Property.Computed (loc, key))
    | T_POUND when class_body ->
      let ((loc, { PrivateName.name; _ }) as id) = private_identifier env in
      add_declared_private env name;
      (loc, PrivateName id)
    | T_POUND ->
      let (loc, id) =
        with_loc
          (fun env ->
            Eat.token env;
            Identifier (identifier_name env))
          env
      in
      error_at env (loc, Parse_error.PrivateNotInClass);
      (loc, id)
    | _ ->
      let ((loc, _) as id) = identifier_name env in
      (loc, Identifier id)

  let getter_or_setter env ~in_class_body is_getter =
    (* this is a getter or setter, it cannot be async *)
    let async = false in
    let (generator, leading) = Declaration.generator env in
    let (key_loc, key) = key ~class_body:in_class_body env in
    let key = object_key_remove_trailing env key in
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
                let params =
                  let params = Declaration.function_params ~await:false ~yield:false env in
                  if Peek.token env = T_COLON then
                    params
                  else
                    function_params_remove_trailing env params
                in
                begin
                  match (is_getter, params) with
                  | (true, (_, { Ast.Function.Params.this_ = Some _; _ })) ->
                    error_at env (key_loc, Parse_error.GetterMayNotHaveThisParam)
                  | (false, (_, { Ast.Function.Params.this_ = Some _; _ })) ->
                    error_at env (key_loc, Parse_error.SetterMayNotHaveThisParam)
                  | ( true,
                      ( _,
                        { Ast.Function.Params.params = []; rest = None; this_ = None; comments = _ }
                      )
                    ) ->
                    ()
                  | (false, (_, { Ast.Function.Params.rest = Some _; _ })) ->
                    (* rest params don't make sense on a setter *)
                    error_at env (key_loc, Parse_error.SetterArity)
                  | ( false,
                      ( _,
                        {
                          Ast.Function.Params.params = [_];
                          rest = None;
                          this_ = None;
                          comments = _;
                        }
                      )
                    ) ->
                    ()
                  | (true, _) -> error_at env (key_loc, Parse_error.GetterArity)
                  | (false, _) -> error_at env (key_loc, Parse_error.SetterArity)
                end;
                let return =
                  return_annotation_remove_trailing env (Type.function_return_annotation_opt env)
                in
                (tparams, params, return))
              env
          in
          let simple_params = is_simple_parameter_list params in
          let (body, contains_use_strict) =
            Declaration.function_body env ~async ~generator ~expression:false ~simple_params
          in
          Declaration.strict_function_post_check env ~contains_use_strict None params;
          {
            Function.id = None;
            params;
            body;
            generator;
            async;
            effect_ = Function.Arbitrary;
            predicate = None;
            (* setters/getter are not predicates *)
            return;
            tparams;
            sig_loc;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          })
        env
    in
    (key, value)

  (* #prod-MethodDefinition *)
  let parse_method ~async ~generator ~leading =
    with_loc (fun env ->
        (* #sec-function-definitions-static-semantics-early-errors *)
        let env = env |> with_allow_super Super_prop in
        let (sig_loc, (tparams, params, return)) =
          with_loc
            (fun env ->
              let tparams =
                type_params_remove_trailing
                  env
                  ~kind:Flow_ast_mapper.FunctionTP
                  (Type.type_params env)
              in
              let params =
                let params = Declaration.function_params ~await:async ~yield:generator env in
                if Peek.token env = T_COLON then
                  params
                else
                  function_params_remove_trailing env params
              in
              let return =
                return_annotation_remove_trailing env (Type.function_return_annotation_opt env)
              in
              (tparams, params, return))
            env
        in
        let simple_params = is_simple_parameter_list params in
        let (body, contains_use_strict) =
          Declaration.function_body env ~async ~generator ~expression:false ~simple_params
        in
        Declaration.strict_function_post_check env ~contains_use_strict None params;
        {
          Function.id = None;
          params;
          body;
          generator;
          effect_ = Function.Arbitrary;
          async;
          (* TODO: add support for object method predicates *)
          predicate = None;
          return;
          tparams;
          sig_loc;
          comments = Flow_ast_utils.mk_comments_opt ~leading ();
        }
    )

  let _initializer =
    let parse_assignment_cover env =
      match Expression.assignment_cover env with
      | Cover_expr expr -> (expr, Pattern_cover.empty_errors)
      | Cover_patt (expr, errs) -> (expr, errs)
    in
    let get env start_loc leading =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:false true) env
      in
      let open Ast.Expression.Object in
      Property
        (loc, Property.Get { key; value; comments = Flow_ast_utils.mk_comments_opt ~leading () })
    in
    let set env start_loc leading =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:false false) env
      in
      let open Ast.Expression.Object in
      Property
        (loc, Property.Set { key; value; comments = Flow_ast_utils.mk_comments_opt ~leading () })
    in
    (* #prod-PropertyDefinition *)
    let init =
      let open Ast.Expression.Object.Property in
      (* #prod-IdentifierReference *)
      let parse_shorthand env key =
        match key with
        | StringLiteral (loc, lit) ->
          error_at env (loc, Parse_error.LiteralShorthandProperty);
          (loc, Ast.Expression.StringLiteral lit)
        | NumberLiteral (loc, lit) ->
          error_at env (loc, Parse_error.LiteralShorthandProperty);
          (loc, Ast.Expression.NumberLiteral lit)
        | BigIntLiteral (loc, lit) ->
          error_at env (loc, Parse_error.LiteralShorthandProperty);
          (loc, Ast.Expression.BigIntLiteral lit)
        | Identifier ((loc, { Identifier.name; comments = _ }) as id) ->
          (* #sec-identifiers-static-semantics-early-errors *)
          if is_reserved name then
            (* it is a syntax error if `name` is a reserved word other than await or yield *)
            error_at env (loc, Parse_error.UnexpectedReserved)
          else if is_strict_reserved name then
            (* it is a syntax error if `name` is a strict reserved word, in strict mode *)
            strict_error_at env (loc, Parse_error.StrictReservedWord);
          (loc, Ast.Expression.Identifier id)
        | PrivateName _ -> failwith "Internal Error: private name found in object props"
        | Computed (_, { ComputedKey.expression = expr; comments = _ }) ->
          error_at env (fst expr, Parse_error.ComputedShorthandProperty);
          expr
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
          let ast =
            with_loc
              ~start_loc:(fst id)
              (fun env ->
                let leading = Peek.comments env in
                Expect.token env T_ASSIGN;
                let trailing = Eat.trailing_comments env in
                let left = Parse.pattern_from_expr env (fst id, Ast.Expression.Identifier id) in
                let right = Parse.assignment env in
                let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
                Ast.Expression.Assignment
                  { Ast.Expression.Assignment.operator = None; left; right; comments })
              env
          in
          let errs =
            {
              if_expr = [(assignment_loc, Parse_error.Unexpected (Token.quote_token_value "="))];
              if_patt = [];
            }
          in
          (ast, errs)
        | Property.StringLiteral _
        | Property.NumberLiteral _
        | Property.BigIntLiteral _
        | Property.PrivateName _
        | Property.Computed _ ->
          parse_value env
      in
      let parse_init ~key ~async ~generator ~leading env =
        if async || generator then
          let key = object_key_remove_trailing env key in
          (* the `async` and `*` modifiers are only valid on methods *)
          let value = parse_method env ~async ~generator ~leading in
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
            let key = object_key_remove_trailing env key in
            let value = parse_method env ~async ~generator ~leading in
            let prop = Method { key; value } in
            (prop, Pattern_cover.empty_errors)
          | T_ASSIGN ->
            let (value, errs) = parse_assignment_pattern ~key env in
            let prop = Init { key; value; shorthand = true } in
            (prop, errs)
          | T_COLON ->
            let (value, errs) = parse_value env in
            let prop = Init { key; value; shorthand = false } in
            (prop, errs)
          | _ ->
            (* error. we recover by treating it as a shorthand property so as to not
               consume any more tokens and make the error worse. we don't error here
               because we'll expect a comma before the next token. *)
            let value = parse_shorthand env key in
            let prop = Init { key; value; shorthand = true } in
            (prop, Pattern_cover.empty_errors)
      in
      fun env start_loc key async generator leading ->
        let (loc, (prop, errs)) =
          with_loc ~start_loc (parse_init ~key ~async ~generator ~leading) env
        in
        (Ast.Expression.Object.Property (loc, prop), errs)
    in
    let property env =
      let open Ast.Expression.Object in
      if Peek.token env = T_ELLIPSIS then
        (* Spread property *)
        let leading = Peek.comments env in
        let (loc, (argument, errs)) =
          with_loc
            (fun env ->
              Expect.token env T_ELLIPSIS;
              parse_assignment_cover env)
            env
        in
        ( SpreadProperty
            (loc, { SpreadProperty.argument; comments = Flow_ast_utils.mk_comments_opt ~leading () }),
          errs
        )
      else
        let start_loc = Peek.loc env in
        let (async, leading_async) =
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
            (false, [])
          | _ -> Declaration.async env
        in
        let (generator, leading_generator) = Declaration.generator env in
        let leading = leading_async @ leading_generator in
        match (async, generator, Peek.token env) with
        | (false, false, T_IDENTIFIER { raw = "get"; _ }) ->
          let leading = Peek.comments env in
          let (_, key) = key env in
          begin
            match Peek.token env with
            | T_ASSIGN
            | T_COLON
            | T_LESS_THAN
            | T_LPAREN
            | T_COMMA
            | T_RCURLY ->
              init env start_loc key false false []
            | _ ->
              ignore (Comment_attachment.object_key_remove_trailing env key);
              (get env start_loc leading, Pattern_cover.empty_errors)
          end
        | (false, false, T_IDENTIFIER { raw = "set"; _ }) ->
          let leading = Peek.comments env in
          let (_, key) = key env in
          begin
            match Peek.token env with
            | T_ASSIGN
            | T_COLON
            | T_LESS_THAN
            | T_LPAREN
            | T_COMMA
            | T_RCURLY ->
              init env start_loc key false false []
            | _ ->
              ignore (Comment_attachment.object_key_remove_trailing env key);
              (set env start_loc leading, Pattern_cover.empty_errors)
          end
        | (async, generator, _) ->
          let (_, key) = key env in
          init env start_loc key async generator leading
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
        let errs = Pattern_cover.rev_append_errors new_errs errs in
        let errs =
          match Peek.token env with
          | T_RCURLY
          | T_EOF ->
            errs
          | T_COMMA ->
            Eat.token env;
            errs
          | _ ->
            (* we could use [Expect.error env T_COMMA], but we're in a weird
               cover grammar situation where we're storing errors in
               [Pattern_cover]. if we used [Expect.error], the errors would
               end up out of order. *)
            let err = Expect.get_error env T_COMMA in
            (* if the unexpected token is a semicolon, consume it to aid
               recovery. using a semicolon instead of a comma is a common
               mistake. *)
            let _ = Eat.maybe env T_SEMICOLON in
            Pattern_cover.cons_error err errs
        in
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
            let internal = Peek.comments env in
            Expect.token env T_RCURLY;
            let trailing = Eat.trailing_comments env in
            ( {
                Ast.Expression.Object.properties = props;
                comments =
                  Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
              },
              errs
            ))
          env
      in
      (loc, expr, errs)

  let check_property_name env loc name static =
    if String.equal name "constructor" || (String.equal name "prototype" && static) then
      error_at
        env
        (loc, Parse_error.InvalidClassMemberName { name; static; method_ = false; private_ = false })

  let check_private_names
      env seen_names private_name (kind : [ `Method | `Field | `Getter | `Setter ]) =
    let (loc, { PrivateName.name; comments = _ }) = private_name in
    if String.equal name "constructor" then
      let () =
        error_at
          env
          ( loc,
            Parse_error.InvalidClassMemberName
              { name; static = false; method_ = kind = `Method; private_ = true }
          )
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

  let class_implements env ~attach_leading =
    let rec interfaces env acc =
      let interface =
        with_loc
          (fun env ->
            let (_loc, { Ast.Type.Generic.id; targs; comments = _ }) = Type.generic env in
            { Ast.Class.Implements.Interface.id; targs })
          env
      in
      let acc = interface :: acc in
      match Peek.token env with
      | T_COMMA ->
        Expect.token env T_COMMA;
        interfaces env acc
      | _ -> List.rev acc
    in
    with_loc
      (fun env ->
        let leading =
          if attach_leading then
            Peek.comments env
          else
            []
        in
        Expect.token env T_IMPLEMENTS;
        let interfaces = interfaces env [] in
        { Ast.Class.Implements.interfaces; comments = Flow_ast_utils.mk_comments_opt ~leading () })
      env

  let class_extends ~leading =
    with_loc (fun env ->
        let expr =
          let expr =
            Expression.left_hand_side (env |> with_allow_yield false |> with_no_record true)
          in
          if Peek.token env <> T_LESS_THAN then
            expr
          else
            let { remove_trailing; _ } = trailing_and_remover env in
            remove_trailing expr (fun remover expr -> remover#expression expr)
        in
        let targs = Type.type_args env in
        { Class.Extends.expr; targs; comments = Flow_ast_utils.mk_comments_opt ~leading () }
    )

  (* https://tc39.es/ecma262/#prod-ClassHeritage *)
  let class_heritage env =
    let extends =
      let leading = Peek.comments env in
      if Eat.maybe env T_EXTENDS then
        let (loc, extends) = class_extends ~leading env in
        let { remove_trailing; _ } = trailing_and_remover env in
        Some
          (loc, remove_trailing extends (fun remover extends -> remover#class_extends loc extends))
      else
        None
    in
    let implements =
      if Peek.token env = T_IMPLEMENTS then (
        if not (should_parse_types env) then error env Parse_error.UnexpectedTypeInterface;
        Some (class_implements_remove_trailing env (class_implements env ~attach_leading:true))
      ) else
        None
    in
    (extends, implements)

  let string_value_of_key key =
    match key with
    | Ast.Expression.Object.Property.Identifier (key_loc, { Identifier.name; comments = _ })
    | Ast.Expression.Object.Property.StringLiteral (key_loc, { StringLiteral.value = name; _ }) ->
      Some (key_loc, name)
    | _ -> None

  (* In the ES6 draft, all elements are methods. No properties (though there
   * are getter and setters allowed *)
  let class_element =
    let get env start_loc decorators static ts_accessibility leading =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:true true) env
      in
      (match (static, string_value_of_key key) with
      | (false, Some (key_loc, "constructor")) ->
        error_at env (key_loc, Parse_error.ConstructorCannotBeAccessor)
      | (true, Some (key_loc, "prototype")) ->
        error_at
          env
          ( key_loc,
            Parse_error.InvalidClassMemberName
              { name = "prototype"; static; method_ = false; private_ = false }
          )
      | _ -> ());
      let open Ast.Class in
      Body.Method
        ( loc,
          {
            Method.key;
            value;
            kind = Method.Get;
            static;
            ts_accessibility;
            decorators;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          }
        )
    in
    let set env start_loc decorators static ts_accessibility leading =
      let (loc, (key, value)) =
        with_loc ~start_loc (fun env -> getter_or_setter env ~in_class_body:true false) env
      in
      (match (static, string_value_of_key key) with
      | (false, Some (key_loc, "constructor")) ->
        error_at env (key_loc, Parse_error.ConstructorCannotBeAccessor)
      | (true, Some (key_loc, "prototype")) ->
        error_at
          env
          ( key_loc,
            Parse_error.InvalidClassMemberName
              { name = "prototype"; static; method_ = false; private_ = false }
          )
      | _ -> ());
      let open Ast.Class in
      Body.Method
        ( loc,
          {
            Method.key;
            value;
            kind = Method.Set;
            static;
            ts_accessibility;
            decorators;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          }
        )
    in
    let error_unsupported_variance env = function
      | Some (loc, _) -> error_at env (loc, Parse_error.UnexpectedVariance)
      | None -> ()
      (* Class property with annotation *)
    in
    let error_unsupported_declare env = function
      | Some loc -> error_at env (loc, Parse_error.DeclareClassElement)
      | None -> ()
    in
    let property_end_and_semicolon env key annot value =
      match Peek.token env with
      | T_LBRACKET
      | T_LPAREN ->
        error_unexpected env;
        (key, annot, value, [])
      | T_SEMICOLON ->
        Eat.token env;
        let trailing =
          match Peek.token env with
          | T_EOF
          | T_RCURLY ->
            Eat.trailing_comments env
          | _ when Peek.is_line_terminator env -> Eat.comments_until_next_line env
          | _ -> []
        in
        (key, annot, value, trailing)
      | _ ->
        let remover =
          match Peek.token env with
          | T_EOF
          | T_RCURLY ->
            { trailing = []; remove_trailing = (fun x _ -> x) }
          | _ when Peek.is_line_terminator env ->
            Comment_attachment.trailing_and_remover_after_last_line env
          | _ -> Comment_attachment.trailing_and_remover_after_last_loc env
        in
        (* Remove trailing comments from the last node in this property *)
        let (key, annot, value) =
          match (annot, value) with
          (* prop = init *)
          | (_, Class.Property.Initialized expr) ->
            ( key,
              annot,
              Class.Property.Initialized
                (remover.remove_trailing expr (fun remover expr -> remover#expression expr))
            )
          (* prop: annot *)
          | (Ast.Type.Available annot, _) ->
            ( key,
              Ast.Type.Available
                (remover.remove_trailing annot (fun remover annot -> remover#type_annotation annot)),
              value
            )
          (* prop *)
          | _ ->
            (remover.remove_trailing key (fun remover key -> remover#object_key key), annot, value)
        in
        (key, annot, value, [])
    in
    let property
        env start_loc decorators key static declare variance ts_accessibility ~abstract leading =
      let (loc, (key, annot, value, comments)) =
        with_loc
          ~start_loc
          (fun env ->
            let annot = Type.annotation_opt env in
            let value =
              match (declare, Peek.token env) with
              | (None, T_ASSIGN) ->
                Eat.token env;
                Ast.Class.Property.Initialized
                  (Parse.expression (env |> with_allow_super Super_prop))
              | (Some _, T_ASSIGN) ->
                error env Parse_error.DeclareClassFieldInitializer;
                Eat.token env;
                Ast.Class.Property.Declared
              | (None, _) -> Ast.Class.Property.Uninitialized
              | (Some _, _) -> Ast.Class.Property.Declared
            in
            let (key, annot, value, trailing) = property_end_and_semicolon env key annot value in
            (key, annot, value, Flow_ast_utils.mk_comments_opt ~leading ~trailing ()))
          env
      in
      if abstract then begin
        (match value with
        | Ast.Class.Property.Initialized _ ->
          error_at env (loc, Parse_error.AbstractPropertyWithInitializer)
        | _ -> ());
        let open Ast.Class in
        match key with
        | Ast.Expression.Object.Property.PrivateName _ ->
          (* Private abstract properties fall back to PrivateField *)
          Body.PrivateField
            ( loc,
              {
                PrivateField.key =
                  (match key with
                  | Ast.Expression.Object.Property.PrivateName k -> k
                  | _ -> failwith "unreachable");
                value;
                annot;
                static;
                variance;
                ts_accessibility;
                decorators;
                comments;
              }
            )
        | _ ->
          Body.AbstractProperty
            (loc, { AbstractProperty.key; annot; ts_accessibility; variance; comments })
      end else begin
        let open Ast.Class in
        match key with
        | Ast.Expression.Object.Property.PrivateName key ->
          Body.PrivateField
            ( loc,
              {
                PrivateField.key;
                value;
                annot;
                static;
                variance;
                ts_accessibility;
                decorators;
                comments;
              }
            )
        | _ ->
          Body.Property
            ( loc,
              {
                Property.key;
                value;
                annot;
                static;
                variance;
                ts_accessibility;
                decorators;
                comments;
              }
            )
      end
    in
    let is_asi env =
      match Peek.token env with
      | T_LESS_THAN -> false
      | T_LPAREN -> false
      | _ when Peek.is_implicit_semicolon env -> true
      | _ -> false
    in
    let rec init
        env
        start_loc
        decorators
        key
        ~async
        ~generator
        ~static
        ~abstract
        ~declare
        variance
        ts_accessibility
        leading =
      match Peek.token env with
      | T_COLON
      | T_ASSIGN
      | T_SEMICOLON
      | T_RCURLY
        when (not async) && not generator ->
        property
          env
          start_loc
          decorators
          key
          static
          declare
          variance
          ts_accessibility
          ~abstract
          leading
      | T_PLING ->
        (* TODO: add support for optional class properties *)
        error_unexpected env;
        Eat.token env;
        init
          env
          start_loc
          decorators
          key
          ~async
          ~generator
          ~static
          ~abstract
          ~declare
          variance
          ts_accessibility
          leading
      | _ when is_asi env ->
        (* an uninitialized, unannotated property *)
        property
          env
          start_loc
          decorators
          key
          static
          declare
          variance
          ts_accessibility
          ~abstract
          leading
      | _ ->
        error_unsupported_declare env declare;
        error_unsupported_variance env variance;
        let (kind, env) =
          match (static, string_value_of_key key) with
          | (false, Some (key_loc, "constructor")) ->
            if async then error_at env (key_loc, Parse_error.ConstructorCannotBeAsync);
            if generator then error_at env (key_loc, Parse_error.ConstructorCannotBeGenerator);
            (Ast.Class.Method.Constructor, env |> with_allow_super Super_prop_or_call)
          | (true, Some (key_loc, "prototype")) ->
            error_at
              env
              ( key_loc,
                Parse_error.InvalidClassMemberName
                  { name = "prototype"; static; method_ = true; private_ = false }
              );
            (Ast.Class.Method.Method, env |> with_allow_super Super_prop)
          | _ -> (Ast.Class.Method.Method, env |> with_allow_super Super_prop)
        in
        let key = object_key_remove_trailing env key in
        (* Parse method signature: type params, params, return annotation *)
        let (sig_loc, (tparams, params, return)) =
          with_loc
            (fun env ->
              let tparams =
                type_params_remove_trailing
                  env
                  ~kind:Flow_ast_mapper.FunctionTP
                  (Type.type_params env)
              in
              let params =
                let params = Declaration.function_params ~await:async ~yield:generator env in
                let params =
                  if Peek.token env = T_COLON then
                    params
                  else
                    function_params_remove_trailing env params
                in
                Ast.Function.Params.(
                  match params with
                  | (loc, ({ this_ = Some (this_loc, _); _ } as params))
                    when kind = Ast.Class.Method.Constructor ->
                    (* Disallow this param annotations for constructors *)
                    error_at env (this_loc, Parse_error.ThisParamBannedInConstructor);
                    (loc, { params with this_ = None })
                  | params -> params
                )
              in
              let return =
                return_annotation_remove_trailing env (Type.function_return_annotation_opt env)
              in
              (tparams, params, return))
            env
        in
        (* Helper to build method function type *)
        let make_method_func_type type_params =
          let return_annot =
            match return with
            | Function.ReturnAnnot.Available (_, (ret_loc, t)) ->
              Ast.Type.Function.Available (ret_loc, t)
            | Function.ReturnAnnot.TypeGuard (_, tg) -> Ast.Type.Function.TypeGuard tg
            | Function.ReturnAnnot.Missing loc ->
              if kind = Ast.Class.Method.Constructor then
                Ast.Type.Function.Missing loc
              else
                Ast.Type.Function.Available (Loc.none, Ast.Type.Any None)
          in
          let func =
            {
              Ast.Type.Function.tparams;
              params = type_params;
              return = return_annot;
              comments = None;
              effect_ = Function.Arbitrary;
            }
          in
          let annot_loc =
            match tparams with
            | Some (tparams_loc, _) -> tparams_loc
            | None -> fst params
          in
          (annot_loc, func)
        in
        (* Wrap function type as Type.annotation for DeclareMethod *)
        let make_method_annot type_params =
          let (annot_loc, func) = make_method_func_type type_params in
          (annot_loc, (annot_loc, Ast.Type.Function func))
        in
        (* Check for bodyless method: semicolon instead of body *)
        let is_bodyless_method =
          Peek.token env = T_SEMICOLON
          && (not async)
          && (not generator)
          && (kind <> Ast.Class.Method.Constructor || in_ambient_context env)
        in
        let make_method_value env =
          with_loc
            ~start_loc:sig_loc
            (fun env ->
              let simple_params = is_simple_parameter_list params in
              let (body, contains_use_strict) =
                Declaration.function_body env ~async ~generator ~expression:false ~simple_params
              in
              Declaration.strict_function_post_check env ~contains_use_strict None params;
              {
                Function.id = None;
                params;
                body;
                generator;
                async;
                effect_ = Function.Arbitrary;
                predicate = None;
                return;
                tparams;
                sig_loc;
                comments = None;
              })
            env
        in
        let make_method env =
          let value = make_method_value env in
          Class.Body.Method
            ( Loc.btwn start_loc (fst value),
              {
                Class.Method.key;
                value;
                kind;
                static;
                ts_accessibility;
                decorators;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              }
            )
        in
        if abstract && is_bodyless_method then
          (* Abstract method - try to convert params to type params *)
          match Declaration.convert_function_params_to_type_params params with
          | Ok type_params ->
            Expect.token env T_SEMICOLON;
            Class.Body.AbstractMethod
              ( Loc.btwn start_loc sig_loc,
                {
                  Class.AbstractMethod.key;
                  annot = make_method_func_type type_params;
                  ts_accessibility;
                  comments = Flow_ast_utils.mk_comments_opt ~leading ();
                }
              )
          | Error _ ->
            (* Params couldn't be converted - fall back to normal method with body *)
            make_method env
        else if abstract then (
          (* Abstract method with a body - error *)
          error env Parse_error.AbstractMethodWithBody;
          make_method env
        ) else if
            is_bodyless_method
            && kind <> Class.Method.Get
            && kind <> Class.Method.Set
            && List.length decorators = 0
            &&
            match return with
            | Function.ReturnAnnot.Missing _ -> kind = Ast.Class.Method.Constructor
            | _ -> true
          then
          (* Implicit declare method *)
          match Declaration.convert_function_params_to_type_params params with
          | Ok type_params ->
            Expect.token env T_SEMICOLON;
            Class.Body.DeclareMethod
              ( Loc.btwn start_loc sig_loc,
                {
                  Class.DeclareMethod.key;
                  annot = make_method_annot type_params;
                  static;
                  comments = Flow_ast_utils.mk_comments_opt ~leading ();
                }
              )
          | Error _ ->
            (* Normal method with body *)
            make_method env
        else
          (* Normal method with body *)
          make_method env
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
      let (declare, leading_declare) =
        match Peek.token env with
        | T_DECLARE when not (ith_implies_identifier ~i:1 env) ->
          let ret = Some (Peek.loc env) in
          let leading = Peek.comments env in
          Eat.token env;
          (ret, leading)
        | _ -> (None, [])
      in
      (* Parse TS class visibility modifiers. *)
      let (ts_accessibility, leading_accessibility) =
        match Peek.token env with
        | (T_PUBLIC as t)
        | (T_PRIVATE as t)
        | (T_PROTECTED as t)
          when Peek.ith_is_identifier ~i:1 env ->
          let kind =
            match t with
            | T_PUBLIC -> Ast.Class.TSAccessibility.Public
            | T_PRIVATE -> Ast.Class.TSAccessibility.Private
            | T_PROTECTED -> Ast.Class.TSAccessibility.Protected
            | _ -> failwith "Must be one of the above"
          in
          let leading = Peek.comments env in
          let start = Peek.loc env in
          Eat.token env;
          let trailing = Eat.trailing_comments env in
          let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
          (Some (start, { Ast.Class.TSAccessibility.kind; comments }), leading)
        | _ -> (None, [])
      in
      let static =
        Peek.token env = T_STATIC
        &&
        match Peek.ith_token ~i:1 env with
        | T_ASSIGN (* static = 123 *)
        | T_COLON (* static: T *)
        | T_EOF (* incomplete property *)
        | T_LESS_THAN (* static<T>() {} *)
        | T_LPAREN (* static() {} *)
        | T_RCURLY (* end of class *)
        | T_SEMICOLON (* explicit semicolon *) ->
          false
        | _ -> true
      in
      let leading_static =
        if static then (
          let leading = Peek.comments env in
          Eat.token env;
          leading
        ) else
          []
      in
      (* Parse abstract modifier *)
      let abstract =
        match Peek.token env with
        | T_IDENTIFIER { raw = "abstract"; _ } when Peek.ith_is_identifier ~i:1 env -> true
        | _ -> false
      in
      let leading_abstract =
        if abstract then (
          let leading = Peek.comments env in
          Eat.token env;
          leading
        ) else
          []
      in
      (* Error if both static and abstract *)
      if static && abstract then error env Parse_error.StaticAbstractMethod;
      if static && Option.is_none declare && Eat.maybe env T_LCURLY then
        Class.Body.StaticBlock
          (with_loc
             ~start_loc
             (fun env ->
               let internal = Peek.comments env in
               let term_fn = function
                 | T_RCURLY -> true
                 | _ -> false
               in
               let body = Parse.statement_list ~term_fn env in
               Expect.token env T_RCURLY;
               let trailing = Eat.trailing_comments env in
               let comments =
                 Flow_ast_utils.mk_comments_with_internal_opt
                   ~leading:leading_static
                   ~trailing
                   ~internal
                   ()
               in
               { Class.StaticBlock.body; comments })
             env
          )
      else
        let async =
          Peek.token env = T_ASYNC
          && (not (ith_implies_identifier ~i:1 env))
          && not (Peek.ith_is_line_terminator ~i:1 env)
        in
        (* consume `async` *)
        let leading_async =
          if async then (
            let leading = Peek.comments env in
            Eat.token env;
            leading
          ) else
            []
        in
        let (generator, leading_generator) = Declaration.generator env in
        let parse_readonly =
          Peek.ith_is_identifier ~i:1 env || Peek.ith_token ~i:1 env = T_LBRACKET
        in
        let variance = Declaration.variance env ~parse_readonly async generator in
        let (generator, leading_generator) =
          match (generator, variance) with
          | (false, Some _) -> Declaration.generator env
          | _ -> (generator, leading_generator)
        in
        let leading =
          List.concat
            [
              leading_declare;
              leading_accessibility;
              leading_static;
              leading_abstract;
              leading_async;
              leading_generator;
            ]
        in
        match (async, generator, Peek.token env) with
        | (false, false, T_IDENTIFIER { raw = "get"; _ }) ->
          let leading_get = Peek.comments env in
          let (_, key) = key ~class_body:true env in
          if implies_identifier env then
            init
              env
              start_loc
              decorators
              key
              ~async
              ~generator
              ~static
              ~abstract
              ~declare
              variance
              ts_accessibility
              leading
          else (
            error_unsupported_declare env declare;
            error_unsupported_variance env variance;
            ignore (object_key_remove_trailing env key);
            get env start_loc decorators static ts_accessibility (leading @ leading_get)
          )
        | (false, false, T_IDENTIFIER { raw = "set"; _ }) ->
          let leading_set = Peek.comments env in
          let (_, key) = key ~class_body:true env in
          if implies_identifier env then
            init
              env
              start_loc
              decorators
              key
              ~async
              ~generator
              ~static
              ~abstract
              ~declare
              variance
              ts_accessibility
              leading
          else (
            error_unsupported_declare env declare;
            error_unsupported_variance env variance;
            ignore (object_key_remove_trailing env key);
            set env start_loc decorators static ts_accessibility (leading @ leading_set)
          )
        | (_, _, _) ->
          let (_, key) = key ~class_body:true env in
          init
            env
            start_loc
            decorators
            key
            ~async
            ~generator
            ~static
            ~abstract
            ~declare
            variance
            ts_accessibility
            leading

  let class_body =
    let rec elements env ~abstract seen_constructor private_names acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        List.rev acc
      | T_SEMICOLON ->
        (* Skip empty elements *)
        Expect.token env T_SEMICOLON;
        elements env ~abstract seen_constructor private_names acc
      | _ ->
        let element = class_element env in
        let (seen_constructor', private_names') =
          match element with
          | Ast.Class.Body.Method (loc, m) ->
            let open Ast.Class.Method in
            (match m.kind with
            | Constructor ->
              if m.static then
                (seen_constructor, private_names)
              else (
                if seen_constructor then error_at env (loc, Parse_error.DuplicateConstructor);
                (true, private_names)
              )
            | Method ->
              let private_names =
                match m.key with
                | Ast.Expression.Object.Property.PrivateName name ->
                  check_private_names env private_names name `Method
                | _ -> private_names
              in
              (seen_constructor, private_names)
            | Get ->
              let open Ast.Expression.Object.Property in
              let private_names =
                match m.key with
                | PrivateName name -> check_private_names env private_names name `Getter
                | _ -> private_names
              in
              (seen_constructor, private_names)
            | Set ->
              let open Ast.Expression.Object.Property in
              let private_names =
                match m.key with
                | PrivateName name -> check_private_names env private_names name `Setter
                | _ -> private_names
              in
              (seen_constructor, private_names))
          | Ast.Class.Body.Property (_, { Ast.Class.Property.key; static; _ }) ->
            let open Ast.Expression.Object.Property in
            begin
              match key with
              | Identifier (loc, { Identifier.name; comments = _ })
              | StringLiteral (loc, { StringLiteral.value = name; _ }) ->
                check_property_name env loc name static
              | NumberLiteral _
              | BigIntLiteral _
              | Computed _ ->
                ()
              | PrivateName _ ->
                failwith "unexpected PrivateName in Property, expected a PrivateField"
            end;
            (seen_constructor, private_names)
          | Ast.Class.Body.PrivateField (_, { Ast.Class.PrivateField.key; _ }) ->
            let private_names = check_private_names env private_names key `Field in
            (seen_constructor, private_names)
          | Ast.Class.Body.StaticBlock _ -> (seen_constructor, private_names)
          | Ast.Class.Body.DeclareMethod _ ->
            (* DeclareMethod is a bodyless method signature, no private name checking needed *)
            (seen_constructor, private_names)
          | Ast.Class.Body.AbstractMethod (loc, _) ->
            if not abstract then error_at env (loc, Parse_error.AbstractMethodInNonAbstractClass);
            (* AbstractMethod is a bodyless method signature, no private name checking needed *)
            (seen_constructor, private_names)
          | Ast.Class.Body.AbstractProperty (loc, _) ->
            if not abstract then error_at env (loc, Parse_error.AbstractPropertyInNonAbstractClass);
            (seen_constructor, private_names)
        in
        elements env ~abstract seen_constructor' private_names' (element :: acc)
    in
    fun ~expression ~abstract env ->
      with_loc
        (fun env ->
          let leading = Peek.comments env in
          if Eat.maybe env T_LCURLY then (
            enter_class env;
            let body = elements env ~abstract false SMap.empty [] in
            exit_class env;
            Expect.token env T_RCURLY;
            let trailing =
              match (expression, Peek.token env) with
              | (true, _)
              | (_, (T_RCURLY | T_EOF)) ->
                Eat.trailing_comments env
              | _ when Peek.is_line_terminator env -> Eat.comments_until_next_line env
              | _ -> []
            in
            { Ast.Class.Body.body; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
          ) else (
            Expect.error env T_LCURLY;
            { Ast.Class.Body.body = []; comments = None }
          ))
        env

  let _class ?(decorators = []) env ~optional_id ~expression =
    (* 10.2.1 says all parts of a class definition are strict *)
    let env = env |> with_strict true in
    let decorators = decorators @ decorator_list env in
    let leading = Peek.comments env in
    let abstract =
      match Peek.token env with
      | T_IDENTIFIER { raw = "abstract"; _ } ->
        Eat.token env;
        true
      | _ -> false
    in
    Expect.token env T_CLASS;
    let id =
      let tmp_env = env |> with_no_let true in
      match (optional_id, Peek.token tmp_env) with
      | (true, (T_EXTENDS | T_IMPLEMENTS | T_LESS_THAN | T_LCURLY)) -> None
      | _ when Peek.is_identifier env ->
        let id = Parse.identifier tmp_env in
        let { remove_trailing; _ } = trailing_and_remover env in
        let id = remove_trailing id (fun remover id -> remover#identifier id) in
        Some id
      | _ ->
        (* error, but don't consume a token like Parse.identifier does. this helps
           with recovery, and the parser won't get stuck because we consumed the
           `class` token above. *)
        error_nameless_declaration env "class";
        Some (Peek.loc env, { Identifier.name = ""; comments = None })
    in
    let tparams =
      match Type.type_params env with
      | None -> None
      | Some tparams ->
        let { remove_trailing; _ } = trailing_and_remover env in
        Some
          (remove_trailing tparams (fun remover tparams ->
               remover#type_params ~kind:Flow_ast_mapper.ClassTP tparams
           )
          )
    in
    let (extends, implements) = class_heritage env in
    let body = class_body env ~expression ~abstract in
    let comments = Flow_ast_utils.mk_comments_opt ~leading () in
    {
      Class.id;
      body;
      tparams;
      extends;
      implements;
      class_decorators = decorators;
      abstract;
      comments;
    }

  let class_declaration env decorators =
    with_loc
      (fun env ->
        let optional_id = in_export_default env in
        Ast.Statement.ClassDeclaration (_class env ~decorators ~optional_id ~expression:false))
      env

  let class_expression =
    with_loc (fun env -> Ast.Expression.Class (_class env ~optional_id:true ~expression:true))

  let record_body =
    let open Statement.RecordDeclaration in
    let rec elements env acc =
      match Peek.token env with
      | T_EOF
      | T_RCURLY ->
        List.rev acc
      | _ ->
        let maybe_eat_and_get_comments token env =
          let cond =
            Peek.token env = token
            &&
            match Peek.ith_token ~i:1 env with
            | T_COLON (* token: T *)
            | T_LESS_THAN (* token<T>() {} *)
            | T_LPAREN (* token() {} *)
            | T_EOF (* incomplete property *)
            | T_RCURLY (* end of record *) ->
              false
            | _ -> true
          in
          let comments =
            if cond then (
              let leading = Peek.comments env in
              Eat.token env;
              leading
            ) else
              []
          in
          (cond, comments)
        in
        let start_loc = Peek.loc env in
        let (static, leading_static) = maybe_eat_and_get_comments T_STATIC env in
        let (async, leading_async) = maybe_eat_and_get_comments T_ASYNC env in
        let (generator, leading_generator) = Declaration.generator env in
        let parse_readonly =
          Peek.ith_is_identifier ~i:1 env || Peek.ith_token ~i:1 env = T_LBRACKET
        in
        let variance = Declaration.variance env ~parse_readonly async generator in
        let (generator, leading_generator) =
          match (generator, variance) with
          | (false, Some _) -> Declaration.generator env
          | _ -> (generator, leading_generator)
        in
        let leading_key = Peek.comments env in
        let leading = List.concat [leading_static; leading_async; leading_generator; leading_key] in
        if Peek.token env = T_POUND then (
          let error_loc = Peek.loc env in
          Eat.token env;
          error_at env (error_loc, Parse_error.RecordPrivateElementUnsupported)
        );
        let (key_loc, key) = key ~class_body:false env in
        let key =
          match key with
          | Ast.Expression.Object.Property.Computed _ ->
            error_at env (key_loc, Parse_error.RecordComputedPropertyUnsupported);
            Ast.Expression.Object.Property.Identifier
              (key_loc, { Identifier.name = ""; comments = None })
          | _ -> key
        in
        let check_invalid_name env ~method_ =
          match (string_value_of_key key, static) with
          | (Some (key_loc, ("constructor" as key_name)), _)
          | (Some (key_loc, ("prototype" as key_name)), true) ->
            error_at
              env
              (key_loc, Parse_error.RecordInvalidPropertyName { name = key_name; static; method_ })
          | _ -> ()
        in
        let empty_invalid_syntax =
          {
            InvalidPropertySyntax.invalid_suffix_semicolon = None;
            invalid_optional = None;
            invalid_variance = None;
          }
        in
        let invalid_syntax =
          Option.map
            (fun variance ->
              { empty_invalid_syntax with InvalidPropertySyntax.invalid_variance = Some variance })
            variance
        in
        let invalid_syntax =
          if Peek.token env = T_PLING then (
            let error_loc = Peek.loc env in
            Eat.token env;
            let invalid_syntax = Option.value invalid_syntax ~default:empty_invalid_syntax in
            Some { invalid_syntax with InvalidPropertySyntax.invalid_optional = Some error_loc }
          ) else
            invalid_syntax
        in
        let end_property () =
          match Peek.token env with
          | T_EOF
          | T_RCURLY ->
            invalid_syntax
          | T_SEMICOLON ->
            let semicolon_loc = Peek.loc env in
            Eat.token env;
            let invalid_syntax = Option.value invalid_syntax ~default:empty_invalid_syntax in
            Some
              {
                invalid_syntax with
                InvalidPropertySyntax.invalid_suffix_semicolon = Some semicolon_loc;
              }
          | _ ->
            Expect.token env T_COMMA;
            invalid_syntax
        in
        (match Peek.token env with
        | T_COLON when static ->
          check_invalid_name env ~method_:false;
          let prop =
            with_loc
              ~start_loc
              (fun env ->
                let annot = Type.annotation env in
                Expect.token env T_ASSIGN;
                let value = Expression.assignment env in
                let invalid_syntax = end_property () in
                let comments = Flow_ast_utils.mk_comments_opt ~leading () in
                { StaticProperty.key; annot; value; comments; invalid_syntax })
              env
          in
          elements env (Body.StaticProperty prop :: acc)
        | T_COLON when not (async || generator) ->
          check_invalid_name env ~method_:false;
          let prop =
            with_loc
              ~start_loc
              (fun env ->
                let annot = Type.annotation env in
                let default_value =
                  if Eat.maybe env T_ASSIGN then
                    Some (Expression.assignment env)
                  else
                    None
                in
                let invalid_syntax = end_property () in
                let comments = Flow_ast_utils.mk_comments_opt ~leading () in
                { Property.key; annot; default_value; comments; invalid_syntax })
              env
          in
          elements env (Body.Property prop :: acc)
        | T_SEMICOLON
        | T_COMMA ->
          error_at env (key_loc, Parse_error.RecordPropertyAnnotationRequired);
          ignore @@ end_property ();
          elements env acc
        | T_ASSIGN ->
          error_at env (key_loc, Parse_error.RecordPropertyAnnotationRequired);
          Eat.token env;
          ignore @@ Expression.assignment env;
          ignore @@ end_property ();
          elements env acc
        | _ ->
          check_invalid_name env ~method_:true;
          let meth =
            with_loc
              ~start_loc
              (fun env ->
                let value = parse_method env ~async ~generator ~leading in
                let comments = Flow_ast_utils.mk_comments_opt ~leading () in
                {
                  Class.Method.key;
                  value;
                  kind = Class.Method.Method;
                  static;
                  ts_accessibility = None;
                  decorators = [];
                  comments;
                })
              env
          in
          elements env (Body.Method meth :: acc))
    in
    with_loc (fun env ->
        let leading = Peek.comments env in
        if Eat.maybe env T_LCURLY then (
          let body = elements env [] in
          let trailing =
            match Peek.token env with
            | T_RCURLY
            | T_EOF ->
              Eat.trailing_comments env
            | _ when Peek.is_line_terminator env -> Eat.comments_until_next_line env
            | _ -> []
          in
          Expect.token env T_RCURLY;
          { Body.body; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () }
        ) else (
          Expect.error env T_LCURLY;
          { Body.body = []; comments = None }
        )
    )

  let record_declaration =
    with_loc (fun env ->
        let leading = Peek.comments env in
        Expect.token env T_RECORD;
        let id = Parse.identifier env in
        let tparams =
          match Type.type_params env with
          | None -> None
          | Some tparams ->
            let { remove_trailing; _ } = trailing_and_remover env in
            Some
              (remove_trailing tparams (fun remover tparams ->
                   remover#type_params ~kind:Flow_ast_mapper.RecordTP tparams
               )
              )
        in
        let invalid_syntax =
          if Peek.token env = T_ASSIGN then (
            let invalid_syntax =
              {
                Ast.Statement.RecordDeclaration.InvalidSyntax.invalid_infix_equals =
                  Some (Peek.loc env);
              }
            in
            Eat.token env;
            Some invalid_syntax
          ) else
            None
        in
        if Peek.token env = T_EXTENDS then (
          error_at env (Peek.loc env, Parse_error.RecordExtendsUnsupported);
          let leading = Peek.comments env in
          Eat.token env;
          ignore @@ class_extends ~leading env
        );
        let implements =
          if Peek.token env = T_IMPLEMENTS then
            Some (class_implements_remove_trailing env (class_implements env ~attach_leading:true))
          else
            None
        in
        let body = record_body env in
        let comments = Flow_ast_utils.mk_comments_opt ~leading () in
        Ast.Statement.RecordDeclaration
          {
            Ast.Statement.RecordDeclaration.id;
            tparams;
            implements;
            body;
            comments;
            invalid_syntax;
          }
    )
end
