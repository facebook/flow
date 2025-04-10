(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_common
open Parser_env
open Flow_ast
open Comment_attachment

module Declaration (Parse : Parser_common.PARSER) (Type : Parser_common.TYPE) :
  Parser_common.DECLARATION = struct
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
          check_env
      )
    and _object check_env o = List.fold_left object_property check_env o.Pattern.Object.properties
    and object_property check_env =
      let open Pattern.Object in
      function
      | Property (_, { Property.pattern = patt; key = _; shorthand = _; default = _ }) ->
        pattern check_env patt
      | RestElement (_, { Pattern.RestElement.argument; comments = _ }) ->
        pattern check_env argument
    and _array check_env arr = List.fold_left array_element check_env arr.Pattern.Array.elements
    and array_element check_env =
      let open Pattern.Array in
      function
      | Hole _ -> check_env
      | Element (_, { Element.argument; default = _ }) -> pattern check_env argument
      | RestElement (_, { Pattern.RestElement.argument; comments = _ }) ->
        pattern check_env argument
    and identifier_pattern check_env { Pattern.Identifier.name = id; _ } = identifier check_env id
    and identifier (env, param_names) ((loc, { Identifier.name; comments = _ }) as id) =
      if SSet.mem name param_names then error_at env (loc, Parse_error.StrictParamDupe);
      let (env, param_names) = identifier_no_dupe_check (env, param_names) id in
      (env, SSet.add name param_names)
    and identifier_no_dupe_check (env, param_names) (loc, { Identifier.name; comments = _ }) =
      if is_restricted name then strict_error_at env (loc, Parse_error.StrictParamName);
      if is_strict_reserved name then strict_error_at env (loc, Parse_error.StrictReservedWord);
      (env, param_names)
    in
    pattern

  (** Errors if there are any duplicate formal parameters

      https://tc39.es/ecma262/#sec-parameter-lists-static-semantics-early-errors *)
  let check_unique_formal_parameters env params =
    let (_, { Ast.Function.Params.params; rest; this_ = _; comments = _ }) = params in
    let acc =
      List.fold_left
        (fun acc (_, { Function.Param.argument; default = _ }) -> check_param acc argument)
        (env, SSet.empty)
        params
    in
    match rest with
    | Some (_, { Function.RestParam.argument; comments = _ }) -> ignore (check_param acc argument)
    | None -> ()

  (** This does the same check as check_unique_formal_parameters. However, it converts the component
   *  params to a single object destructure, then runs the check. This is done to best match the behavior
   *  of components still using function syntax. *)
  let check_unique_component_formal_parameters env params =
    let (_, { Ast.Statement.ComponentDeclaration.Params.params; rest; comments = _ }) = params in
    let pattern_obj_props =
      List.map
        (fun (_, { Ast.Statement.ComponentDeclaration.Param.name; local; default; shorthand }) ->
          let key =
            match name with
            | Ast.Statement.ComponentDeclaration.Param.StringLiteral (_, lit) ->
              Ast.Pattern.Object.Property.StringLiteral (Loc.none, lit)
            | Ast.Statement.ComponentDeclaration.Param.Identifier id ->
              Ast.Pattern.Object.Property.Identifier id
          in
          Ast.Pattern.Object.Property
            (Loc.none, { Ast.Pattern.Object.Property.key; pattern = local; default; shorthand }))
        params
    in
    let obj_param =
      ( Loc.none,
        Ast.Pattern.Object
          {
            Ast.Pattern.Object.properties = pattern_obj_props;
            annot = Ast.Type.Missing Loc.none;
            comments = None;
          }
      )
    in
    let acc = check_param (env, SSet.empty) obj_param in
    match rest with
    | Some (_, { Ast.Statement.ComponentDeclaration.RestParam.argument; comments = _ }) ->
      ignore (check_param acc argument)
    | None -> ()

  type param_type =
    | FunctionParams of (Loc.t, Loc.t) Ast.Function.Params.t
    | ComponentParams of (Loc.t, Loc.t) Ast.Statement.ComponentDeclaration.Params.t

  let strict_post_check env ~contains_use_strict id params =
    let strict_mode = Parser_env.in_strict_mode env in
    let simple =
      match params with
      | FunctionParams p -> is_simple_parameter_list p
      | ComponentParams _ ->
        (* Component params are equivalent to an object destructure so not simple *)
        false
    in
    (* If we were already in strict mode and therefore already threw strict
       errors, we want to do these checks outside of strict mode. If we
       were in non-strict mode but the function contains "use strict", then
       we want to do these checks in strict mode *)
    let env =
      if strict_mode then
        with_strict false env
      else
        with_strict contains_use_strict env
    in
    if contains_use_strict || strict_mode || not simple then (
      (match id with
      | Some (loc, { Identifier.name; comments = _ }) ->
        if is_restricted name then strict_error_at env (loc, Parse_error.StrictFunctionName);
        if is_strict_reserved name then strict_error_at env (loc, Parse_error.StrictReservedWord)
      | None -> ());
      match params with
      | FunctionParams p -> check_unique_formal_parameters env p
      | ComponentParams p -> check_unique_component_formal_parameters env p
    )

  let strict_function_post_check env ~contains_use_strict id params =
    strict_post_check env ~contains_use_strict id (FunctionParams params)

  let strict_component_post_check env ~contains_use_strict id params =
    strict_post_check env ~contains_use_strict (Some id) (ComponentParams params)

  let rest_param env t =
    if t = T_ELLIPSIS then
      let leading = Peek.comments env in
      let (loc, id) =
        with_loc
          (fun env ->
            Expect.token env T_ELLIPSIS;
            Parse.pattern env Parse_error.StrictParamName)
          env
      in
      let comments = Flow_ast_utils.mk_comments_opt ~leading () in
      Some (loc, id, comments)
    else
      None

  let function_params =
    let rec param =
      with_loc (fun env ->
          if Peek.token env = T_THIS then error env Parse_error.ThisParamMustBeFirst;
          let argument = Parse.pattern env Parse_error.StrictParamName in
          let default =
            if Peek.token env = T_ASSIGN then (
              Expect.token env T_ASSIGN;
              Some (Parse.assignment env)
            ) else
              None
          in
          { Function.Param.argument; default }
      )
    and param_list env acc =
      match Peek.token env with
      | (T_EOF | T_RPAREN | T_ELLIPSIS) as t ->
        let rest =
          rest_param env t
          |> Option.map (fun (loc, id, comments) ->
                 (loc, { Function.RestParam.argument = id; comments })
             )
        in
        if Peek.token env <> T_RPAREN then error env Parse_error.ParameterAfterRestParameter;
        (List.rev acc, rest)
      | _ ->
        let the_param = param env in
        if Peek.token env <> T_RPAREN then Expect.token env T_COMMA;
        param_list env (the_param :: acc)
    in
    let this_param_annotation env =
      if should_parse_types env && Peek.token env = T_THIS then (
        let leading = Peek.comments env in
        let (this_loc, this_param) =
          with_loc
            (fun env ->
              Expect.token env T_THIS;
              if Peek.token env <> T_COLON then begin
                error env Parse_error.ThisParamAnnotationRequired;
                None
              end else
                Some (Type.annotation env))
            env
        in
        match this_param with
        | None -> None
        | Some annot ->
          if Peek.token env = T_COMMA then Eat.token env;
          Some
            ( this_loc,
              {
                Ast.Function.ThisParam.annot;
                comments = Flow_ast_utils.mk_comments_opt ~leading ();
              }
            )
      ) else
        None
    in
    fun ~await ~yield ->
      with_loc (fun env ->
          let env =
            env
            |> with_allow_await await
            |> with_allow_yield yield
            |> with_in_formal_parameters true
          in
          let leading = Peek.comments env in
          Expect.token env T_LPAREN;
          let this_ = this_param_annotation env in
          let (params, rest) = param_list env [] in
          let internal = Peek.comments env in
          Expect.token env T_RPAREN;
          let trailing = Eat.trailing_comments env in
          {
            Ast.Function.Params.params;
            rest;
            comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
            this_;
          }
      )

  let function_or_component_body env ~async ~generator ~expression ~simple_params =
    let env = enter_function env ~async ~generator ~simple_params in
    Parse.function_block_body env ~expression

  let function_body env ~async ~generator ~expression ~simple_params =
    let (body_block, contains_use_strict) =
      function_or_component_body env ~async ~generator ~expression ~simple_params
    in
    (Function.BodyBlock body_block, contains_use_strict)

  let variance env ~parse_readonly is_async is_generator =
    let loc = Peek.loc env in
    let variance =
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
            {
              Variance.kind = Variance.Minus;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            }
          )
      | T_IDENTIFIER { raw = "readonly"; _ } when parse_readonly ->
        let leading = Peek.comments env in
        Eat.token env;
        Some
          ( loc,
            {
              Variance.kind = Variance.Readonly;
              comments = Flow_ast_utils.mk_comments_opt ~leading ();
            }
          )
      | _ -> None
    in
    match variance with
    | Some (loc, _) when is_async || is_generator ->
      error_at env (loc, Parse_error.UnexpectedVariance);
      None
    | _ -> variance

  let generator env =
    if Peek.token env = T_MULT then (
      let leading = Peek.comments env in
      Eat.token env;
      (true, leading)
    ) else
      (false, [])

  (* Returns true and consumes a token if the token is `async` and the token after it is on
     the same line (see https://tc39.github.io/ecma262/#sec-async-function-definitions) *)
  let async env =
    if Peek.token env = T_ASYNC && not (Peek.ith_is_line_terminator ~i:1 env) then
      let leading = Peek.comments env in
      let () = Eat.token env in
      (true, leading)
    else
      (false, [])

  let _function =
    with_loc (fun env ->
        let (async, leading_async) = async env in
        let (sig_loc, (generator, effect_, tparams, id, params, return, predicate, leading)) =
          with_loc
            (fun env ->
              let leading_function = Peek.comments env in
              let (effect_, (generator, leading_generator)) =
                match Peek.token env with
                | T_FUNCTION ->
                  Eat.token env;
                  (Function.Arbitrary, generator env)
                | T_IDENTIFIER { raw = "hook"; _ } when not async ->
                  Eat.token env;
                  (Function.Hook, (false, []))
                | t ->
                  Expect.error env t;
                  (Function.Arbitrary, generator env)
              in
              let leading = List.concat [leading_async; leading_function; leading_generator] in
              let (tparams, id) =
                match (in_export_default env, Peek.token env) with
                | (true, T_LPAREN) -> (None, None)
                | (true, T_LESS_THAN) ->
                  let tparams =
                    type_params_remove_trailing
                      env
                      ~kind:Flow_ast_mapper.DeclareFunctionTP
                      (Type.type_params env)
                  in
                  let id =
                    if Peek.token env = T_LPAREN then
                      None
                    else
                      let id =
                        id_remove_trailing
                          env
                          (Parse.identifier ~restricted_error:Parse_error.StrictFunctionName env)
                      in
                      Some id
                  in
                  (tparams, id)
                | _ ->
                  let id =
                    if Peek.is_identifier env then
                      id_remove_trailing
                        env
                        (Parse.identifier ~restricted_error:Parse_error.StrictFunctionName env)
                    else (
                      (* don't consume the identifier here like Parse.identifier does. *)
                      error_nameless_declaration env "function";
                      (Peek.loc env, { Identifier.name = ""; comments = None })
                    )
                  in
                  let tparams =
                    type_params_remove_trailing
                      env
                      ~kind:Flow_ast_mapper.DeclareFunctionTP
                      (Type.type_params env)
                  in
                  (tparams, Some id)
              in
              let params =
                let params = function_params ~await:async ~yield:generator env in
                if Peek.token env = T_COLON then
                  params
                else
                  function_params_remove_trailing env params
              in
              let (return, predicate) = Type.function_return_annotation_and_predicate_opt env in
              let (return, predicate) =
                match predicate with
                | None -> (return_annotation_remove_trailing env return, predicate)
                | Some _ -> (return, predicate_remove_trailing env predicate)
              in
              (generator, effect_, tparams, id, params, return, predicate, leading))
            env
        in
        let simple_params = is_simple_parameter_list params in
        let (body, contains_use_strict) =
          function_body env ~async ~generator ~expression:false ~simple_params
        in
        strict_function_post_check env ~contains_use_strict id params;
        Statement.FunctionDeclaration
          {
            Function.id;
            params;
            body;
            generator;
            effect_;
            async;
            predicate;
            return;
            tparams;
            sig_loc;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          }
    )

  let variable_declaration_list =
    let variable_declaration env =
      let (loc, (decl, err)) =
        with_loc
          (fun env ->
            let id = Parse.pattern env Parse_error.StrictVarName in
            let (init, err) =
              if Eat.maybe env T_ASSIGN then
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
      if Eat.maybe env T_COMMA then
        helper env decls errs
      else
        (List.rev decls, List.rev errs)
    in
    (fun env -> helper env [] [])

  let declarations token env =
    let leading = Peek.comments env in
    Expect.token env token;
    if (parse_options env).enums && token = T_CONST && Peek.token env = T_ENUM then
      error env Parse_error.EnumInvalidConstPrefix;
    let (declarations, errs) = variable_declaration_list env in
    (declarations, leading, errs)

  let var = declarations T_VAR

  let const env =
    let env = env |> with_no_let true in
    let (declarations, leading_comments, errs) = declarations T_CONST env in
    (* Make sure all consts defined are initialized *)
    let errs =
      List.fold_left
        (fun errs decl ->
          match decl with
          | (loc, { Statement.VariableDeclaration.Declarator.init = None; _ }) ->
            (loc, Parse_error.NoUninitializedConst) :: errs
          | _ -> errs)
        errs
        declarations
    in
    (declarations, leading_comments, List.rev errs)

  let let_ env =
    let env = env |> with_no_let true in
    declarations T_LET env

  let enum_declaration ?leading =
    with_loc (fun env ->
        let enum = Enum.declaration ?leading env in
        Statement.EnumDeclaration enum
    )

  let component_params =
    let rec param =
      with_loc (fun env ->
          let leading = Peek.comments env in
          let (name, local, shorthand) =
            match (Peek.token env, Peek.ith_token ~i:1 env) with
            (* "prop-key" as propKey *)
            | ( T_STRING (loc, value, raw, octal),
                ((T_COLON | T_PLING | T_IDENTIFIER { raw = "as"; _ }) as next_token)
              ) ->
              if octal then strict_error env Parse_error.StrictOctalLiteral;
              Expect.token env (T_STRING (loc, value, raw, octal));
              let trailing = Eat.trailing_comments env in
              let name =
                Statement.ComponentDeclaration.Param.StringLiteral
                  ( loc,
                    {
                      StringLiteral.value;
                      raw;
                      comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
                    }
                  )
              in
              (match next_token with
              | T_COLON
              | T_PLING ->
                (* This is an error probably due to someone learning component syntax. Let's make a
                 * good error message and supply a quick fix *)
                let optional = next_token = T_PLING in
                error
                  env
                  Parse_error.(InvalidComponentStringParameterBinding { optional; name = value });
                if optional then Eat.token env;
                let loc = Peek.loc env in
                let fallback_ident = (loc, { Ast.Identifier.name = ""; comments = None }) in
                let annot = Type.annotation_opt env in
                let local =
                  ( loc,
                    Ast.Pattern.Identifier
                      { Ast.Pattern.Identifier.name = fallback_ident; annot; optional }
                  )
                in
                (name, local, false)
              | _ ->
                Eat.token env;
                let local = Parse.pattern env Parse_error.StrictParamName in
                (name, local, false))
            | (_, T_IDENTIFIER { raw = "as"; _ }) ->
              let name = Statement.ComponentDeclaration.Param.Identifier (identifier_name env) in
              Expect.identifier env "as";
              (name, Parse.pattern env Parse_error.StrictParamName, false)
            | (T_LCURLY, _) ->
              error env Parse_error.InvalidComponentParamName;
              let fake_name_loc = Peek.loc env in
              let fallback_ident = (fake_name_loc, { Ast.Identifier.name = ""; comments = None }) in
              let name = Statement.ComponentDeclaration.Param.Identifier fallback_ident in
              let local = Parse.pattern env Parse_error.StrictParamName in
              (name, local, false)
            | (_, _) ->
              let id = Parse.identifier_with_type env Parse_error.StrictParamName in
              (match id with
              | (loc, ({ Ast.Pattern.Identifier.name; _ } as id)) ->
                ( Ast.Statement.ComponentDeclaration.Param.Identifier name,
                  (loc, Ast.Pattern.Identifier id),
                  true
                ))
          in

          let default =
            if Peek.token env = T_ASSIGN then (
              Expect.token env T_ASSIGN;
              Some (Parse.assignment env)
            ) else
              None
          in
          { Statement.ComponentDeclaration.Param.name; local; default; shorthand }
      )
    and param_list env acc =
      match Peek.token env with
      | (T_EOF | T_RPAREN | T_ELLIPSIS) as t ->
        let rest =
          rest_param env t
          |> Option.map (fun (loc, id, comments) ->
                 if Peek.token env = T_COMMA then Eat.token env;
                 (loc, { Statement.ComponentDeclaration.RestParam.argument = id; comments })
             )
        in
        if Peek.token env <> T_RPAREN then error env Parse_error.ParameterAfterRestParameter;
        (List.rev acc, rest)
      | _ ->
        let the_param = param env in
        if Peek.token env <> T_RPAREN then Expect.token env T_COMMA;
        param_list env (the_param :: acc)
    in
    with_loc (fun env ->
        let env = env |> with_in_formal_parameters true in
        let leading = Peek.comments env in
        Expect.token env T_LPAREN;
        let (params, rest) = param_list env [] in
        let internal = Peek.comments env in
        Expect.token env T_RPAREN;
        let trailing = Eat.trailing_comments env in
        {
          Ast.Statement.ComponentDeclaration.Params.params;
          rest;
          comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal ();
        }
    )

  let component_body env =
    function_or_component_body
      env
      ~async:false
      ~generator:false
      ~expression:false
      ~simple_params:false

  let component =
    with_loc (fun env ->
        let (sig_loc, (tparams, id, params, renders, leading)) =
          with_loc
            (fun env ->
              let leading = Peek.comments env in
              Expect.identifier env "component";
              let id =
                id_remove_trailing
                  env
                  (* Components should have at least the same strictness as functions *)
                  (Parse.identifier ~restricted_error:Parse_error.StrictFunctionName env)
              in
              let tparams =
                type_params_remove_trailing
                  env
                  ~kind:Flow_ast_mapper.DeclareComponentTP
                  (Type.type_params env)
              in
              let params =
                let params = component_params env in
                if Peek.is_renders_ident env then
                  params
                else
                  component_params_remove_trailing env params
              in
              let renders = Type.renders_annotation_opt env in
              let renders = component_renders_annotation_remove_trailing env renders in
              (tparams, id, params, renders, leading))
            env
        in
        let (body, contains_use_strict) = component_body env in
        strict_component_post_check env ~contains_use_strict id params;
        Statement.ComponentDeclaration
          {
            Statement.ComponentDeclaration.id;
            params;
            body;
            renders;
            tparams;
            sig_loc;
            comments = Flow_ast_utils.mk_comments_opt ~leading ();
          }
    )
end
