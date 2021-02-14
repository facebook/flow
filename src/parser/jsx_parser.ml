(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Token
open Parser_common
open Parser_env
open Flow_ast

module JSX (Parse : Parser_common.PARSER) = struct
  (* Consumes and returns the trailing comments after the end of a JSX tag name,
     attribute, or spread attribute.

     If the component is followed by the end of the JSX tag, then all trailing
     comments are returned. If the component is instead followed by another tag
     component on another line, only trailing comments on the same line are
     returned. If the component is followed by another tag component on the same
     line, all trailing comments will instead be leading the next component. *)
  let tag_component_trailing_comments env =
    match Peek.token env with
    | T_EOF
    | T_DIV
    | T_GREATER_THAN ->
      Eat.trailing_comments env
    | _ when Peek.is_line_terminator env -> Eat.comments_until_next_line env
    | _ -> []

  let spread_attribute env =
    let leading = Peek.comments env in
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let (loc, argument) =
      with_loc
        (fun env ->
          Expect.token env T_LCURLY;
          Expect.token env T_ELLIPSIS;
          let argument = Parse.assignment env in
          Expect.token env T_RCURLY;
          argument)
        env
    in
    Eat.pop_lex_mode env;
    let trailing = tag_component_trailing_comments env in
    ( loc,
      {
        JSX.SpreadAttribute.argument;
        comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
      } )

  let expression_container_contents env =
    if Peek.token env = T_RCURLY then
      JSX.ExpressionContainer.EmptyExpression
    else
      JSX.ExpressionContainer.Expression (Parse.expression env)

  let expression_container env =
    let leading = Peek.comments env in
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let (loc, expression) =
      with_loc
        (fun env ->
          Expect.token env T_LCURLY;
          let expression = expression_container_contents env in
          Expect.token env T_RCURLY;
          expression)
        env
    in
    Eat.pop_lex_mode env;
    let trailing = tag_component_trailing_comments env in
    ( loc,
      {
        JSX.ExpressionContainer.expression;
        comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal:[];
      } )

  let expression_container_or_spread_child env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let (loc, result) =
      with_loc
        (fun env ->
          Expect.token env T_LCURLY;
          let result =
            match Peek.token env with
            | T_ELLIPSIS ->
              let leading = Peek.comments env in
              Expect.token env T_ELLIPSIS;
              let expression = Parse.assignment env in
              JSX.SpreadChild
                {
                  JSX.SpreadChild.expression;
                  comments = Flow_ast_utils.mk_comments_opt ~leading ();
                }
            | _ ->
              let expression = expression_container_contents env in
              let internal =
                match expression with
                | JSX.ExpressionContainer.EmptyExpression -> Peek.comments env
                | _ -> []
              in
              JSX.ExpressionContainer
                {
                  JSX.ExpressionContainer.expression;
                  comments = Flow_ast_utils.mk_comments_with_internal_opt internal;
                }
          in
          Expect.token env T_RCURLY;
          result)
        env
    in
    Eat.pop_lex_mode env;
    (loc, result)

  let identifier env =
    let loc = Peek.loc env in
    let name =
      match Peek.token env with
      | T_JSX_IDENTIFIER { raw } -> raw
      | _ ->
        error_unexpected ~expected:"an identifier" env;
        ""
    in
    let leading = Peek.comments env in
    Eat.token env;
    (* Unless this identifier is the first part of a namespaced name, member
       expression, or attribute name, it is the end of a tag component. *)
    let trailing =
      match Peek.token env with
      (* Namespaced name *)
      | T_COLON
      (* Member expression *)
      | T_PERIOD
      (* Attribute name *)
      | T_ASSIGN ->
        Eat.trailing_comments env
      | _ -> tag_component_trailing_comments env
    in
    (loc, JSX.Identifier.{ name; comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () })

  let name =
    let rec member_expression env member =
      match Peek.token env with
      | T_PERIOD ->
        let (start_loc, _) = member in
        let member =
          with_loc
            ~start_loc
            (fun env ->
              Expect.token env T_PERIOD;
              let property = identifier env in
              {
                JSX.MemberExpression._object = JSX.MemberExpression.MemberExpression member;
                property;
              })
            env
        in
        member_expression env member
      | _ -> member
    in
    fun env ->
      match Peek.ith_token ~i:1 env with
      | T_COLON ->
        let namespaced_name =
          with_loc
            (fun env ->
              let namespace = identifier env in
              Expect.token env T_COLON;
              let name = identifier env in
              { JSX.NamespacedName.namespace; name })
            env
        in
        JSX.NamespacedName namespaced_name
      | T_PERIOD ->
        let member =
          with_loc
            (fun env ->
              let _object = JSX.MemberExpression.Identifier (identifier env) in
              Expect.token env T_PERIOD;
              let property = identifier env in
              { JSX.MemberExpression._object; property })
            env
        in
        JSX.MemberExpression (member_expression env member)
      | _ ->
        let name = identifier env in
        JSX.Identifier name

  let attribute env =
    with_loc
      (fun env ->
        let name =
          match Peek.ith_token ~i:1 env with
          | T_COLON ->
            let namespaced_name =
              with_loc
                (fun env ->
                  let namespace = identifier env in
                  Expect.token env T_COLON;
                  let name = identifier env in
                  { JSX.NamespacedName.namespace; name })
                env
            in
            JSX.Attribute.NamespacedName namespaced_name
          | _ ->
            let name = identifier env in
            JSX.Attribute.Identifier name
        in
        let value =
          match Peek.token env with
          | T_ASSIGN ->
            Expect.token env T_ASSIGN;
            let leading = Peek.comments env in
            let tkn = Peek.token env in
            begin
              match tkn with
              | T_LCURLY ->
                let (loc, expression_container) = expression_container env in
                JSX.ExpressionContainer.(
                  match expression_container.expression with
                  | EmptyExpression ->
                    error_at env (loc, Parse_error.JSXAttributeValueEmptyExpression)
                  | _ -> ());
                Some (JSX.Attribute.ExpressionContainer (loc, expression_container))
              | T_JSX_TEXT (loc, value, raw) as token ->
                Expect.token env token;
                let value = Ast.Literal.String value in
                let trailing = tag_component_trailing_comments env in
                Some
                  (JSX.Attribute.Literal
                     ( loc,
                       {
                         Ast.Literal.value;
                         raw;
                         comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
                       } ))
              | _ ->
                error env Parse_error.InvalidJSXAttributeValue;
                let loc = Peek.loc env in
                let raw = "" in
                let value = Ast.Literal.String "" in
                Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw; comments = None }))
            end
          | _ -> None
        in
        { JSX.Attribute.name; value })
      env

  let opening_element =
    let rec attributes env acc =
      match Peek.token env with
      | T_JSX_IDENTIFIER _ ->
        let attribute = JSX.Opening.Attribute (attribute env) in
        attributes env (attribute :: acc)
      | T_LCURLY ->
        let attribute = JSX.Opening.SpreadAttribute (spread_attribute env) in
        attributes env (attribute :: acc)
      | _ -> List.rev acc
    in
    fun env ->
      with_loc
        (fun env ->
          Expect.token env T_LESS_THAN;
          match Peek.token env with
          | T_GREATER_THAN ->
            Eat.token env;
            Ok `Fragment
          | T_JSX_IDENTIFIER _ ->
            let name = name env in
            let attributes = attributes env [] in
            let self_closing = Eat.maybe env T_DIV in
            let element = `Element { JSX.Opening.name; self_closing; attributes } in
            if Eat.maybe env T_GREATER_THAN then
              Ok element
            else (
              Expect.error env T_GREATER_THAN;
              Error element
            )
          | _ ->
            (* TODO: also say that we could expect an identifier, or if we're in a JSX child
               then suggest escaping the < as `{'<'}` *)
            Expect.error env T_GREATER_THAN;
            Error `Fragment)
        env

  let closing_element env =
    with_loc
      (fun env ->
        Expect.token env T_LESS_THAN;
        Expect.token env T_DIV;
        match Peek.token env with
        | T_GREATER_THAN ->
          Eat.token env;
          `Fragment
        | T_JSX_IDENTIFIER _ ->
          let name = name env in
          Expect.token_opt env T_GREATER_THAN;
          `Element { JSX.Closing.name }
        | _ ->
          Expect.error env T_GREATER_THAN;
          `Fragment)
      env

  let rec child env =
    match Peek.token env with
    | T_LCURLY -> expression_container_or_spread_child env
    | T_JSX_TEXT (loc, value, raw) as token ->
      Expect.token env token;
      (loc, JSX.Text { JSX.Text.value; raw })
    | _ ->
      (match element_or_fragment env with
      | (loc, `Element element) -> (loc, JSX.Element element)
      | (loc, `Fragment fragment) -> (loc, JSX.Fragment fragment))

  and element =
    let children_and_closing =
      let rec children_and_closing env acc =
        let previous_loc = last_loc env in
        match Peek.token env with
        | T_LESS_THAN ->
          Eat.push_lex_mode env Lex_mode.JSX_TAG;
          begin
            match (Peek.token env, Peek.ith_token ~i:1 env) with
            | (T_LESS_THAN, T_EOF)
            | (T_LESS_THAN, T_DIV) ->
              let closing =
                match closing_element env with
                | (loc, `Element ec) -> `Element (loc, ec)
                | (loc, `Fragment) -> `Fragment loc
              in
              (* We double pop to avoid going back to childmode and re-lexing the
               * lookahead *)
              Eat.double_pop_lex_mode env;
              (List.rev acc, previous_loc, closing)
            | _ ->
              let child =
                match element env with
                | (loc, `Element e) -> (loc, JSX.Element e)
                | (loc, `Fragment f) -> (loc, JSX.Fragment f)
              in
              children_and_closing env (child :: acc)
          end
        | T_EOF ->
          error_unexpected env;
          (List.rev acc, previous_loc, `None)
        | _ -> children_and_closing env (child env :: acc)
      in
      fun env ->
        let start_loc = Peek.loc env in
        let (children, last_child_loc, closing) = children_and_closing env [] in
        let last_child_loc =
          match last_child_loc with
          | Some x -> x
          | None -> start_loc
        in
        (* It's a little bit tricky to untangle the parsing of the child elements from the parsing
         * of the closing element, so we can't easily use `with_loc` here. Instead, we'll use the
         * same logic that `with_loc` uses, but manipulate the locations explicitly. *)
        let children_loc = Loc.btwn start_loc last_child_loc in
        ((children_loc, children), closing)
    in
    let rec normalize name =
      JSX.(
        match name with
        | Identifier (_, { Identifier.name; comments = _ }) -> name
        | NamespacedName (_, { NamespacedName.namespace; name }) ->
          (snd namespace).Identifier.name ^ ":" ^ (snd name).Identifier.name
        | MemberExpression (_, { MemberExpression._object; property }) ->
          let _object =
            match _object with
            | MemberExpression.Identifier (_, { Identifier.name = id; _ }) -> id
            | MemberExpression.MemberExpression e -> normalize (JSX.MemberExpression e)
          in
          _object ^ "." ^ (snd property).Identifier.name)
    in
    let is_self_closing = function
      | (_, Ok (`Element e)) -> e.JSX.Opening.self_closing
      | (_, Ok `Fragment) -> false
      | (_, Error _) -> true
    in
    fun env ->
      let leading = Peek.comments env in
      let opening_element = opening_element env in
      Eat.pop_lex_mode env;
      let (children, closing_element) =
        if is_self_closing opening_element then
          (with_loc (fun _ -> []) env, `None)
        else (
          Eat.push_lex_mode env Lex_mode.JSX_CHILD;
          children_and_closing env
        )
      in
      let trailing = Eat.trailing_comments env in
      let end_loc =
        match closing_element with
        | `Element (loc, { JSX.Closing.name }) ->
          (match snd opening_element with
          | Ok (`Element { JSX.Opening.name = opening_name; _ }) ->
            let opening_name = normalize opening_name in
            if normalize name <> opening_name then
              error env (Parse_error.ExpectedJSXClosingTag opening_name)
          | Ok `Fragment -> error env (Parse_error.ExpectedJSXClosingTag "JSX fragment")
          | Error _ -> ());
          loc
        | `Fragment loc ->
          (match snd opening_element with
          | Ok (`Element { JSX.Opening.name = opening_name; _ }) ->
            error env (Parse_error.ExpectedJSXClosingTag (normalize opening_name))
          | Ok `Fragment -> ()
          | Error _ -> ());
          loc
        | _ -> fst opening_element
      in
      let result =
        match opening_element with
        | (start_loc, Ok (`Element e))
        | (start_loc, Error (`Element e)) ->
          `Element
            JSX.
              {
                opening_element = (start_loc, e);
                closing_element =
                  (match closing_element with
                  | `Element e -> Some e
                  | _ -> None);
                children;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }
        | (start_loc, Ok `Fragment)
        | (start_loc, Error `Fragment) ->
          `Fragment
            JSX.
              {
                frag_opening_element = start_loc;
                frag_closing_element =
                  (match closing_element with
                  | `Fragment loc -> loc
                  (* the following are parse erros *)
                  | `Element (loc, _) -> loc
                  | _ -> end_loc);
                frag_children = children;
                frag_comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }
      in
      (Loc.btwn (fst opening_element) end_loc, result)

  and element_or_fragment env =
    Eat.push_lex_mode env Lex_mode.JSX_TAG;
    element env
end
