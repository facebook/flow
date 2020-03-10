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
  let spread_attribute env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let attr =
      with_loc
        (fun env ->
          Expect.token env T_LCURLY;
          Expect.token env T_ELLIPSIS;
          let argument = Parse.assignment env in
          Expect.token env T_RCURLY;
          { JSX.SpreadAttribute.argument })
        env
    in
    Eat.pop_lex_mode env;
    attr

  let expression_container' env =
    let expression =
      if Peek.token env = T_RCURLY then
        JSX.ExpressionContainer.EmptyExpression
      else
        JSX.ExpressionContainer.Expression (Parse.expression env)
    in
    { JSX.ExpressionContainer.expression }

  let expression_container env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let container =
      with_loc
        (fun env ->
          Expect.token env T_LCURLY;
          let container = expression_container' env in
          Expect.token env T_RCURLY;
          container)
        env
    in
    Eat.pop_lex_mode env;
    container

  let expression_container_or_spread_child env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let (loc, result) =
      with_loc
        (fun env ->
          Expect.token env T_LCURLY;
          let result =
            match Peek.token env with
            | T_ELLIPSIS ->
              Expect.token env T_ELLIPSIS;
              let expr = Parse.assignment env in
              JSX.SpreadChild expr
            | _ ->
              let container = expression_container' env in
              JSX.ExpressionContainer container
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
    Eat.token env;
    (loc, JSX.Identifier.{ name })

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
            let trailing = Peek.comments env in
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
                Some
                  (JSX.Attribute.Literal
                     ( loc,
                       {
                         Ast.Literal.value;
                         raw;
                         comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
                       } ))
            end
          | _ -> None
        in
        { JSX.Attribute.name; value })
      env

  let opening_element =
    let rec attributes env acc =
      match Peek.token env with
      | T_EOF
      | T_DIV
      | T_GREATER_THAN ->
        List.rev acc
      | T_LCURLY ->
        let attribute = JSX.Opening.SpreadAttribute (spread_attribute env) in
        attributes env (attribute :: acc)
      | _ ->
        let attribute = JSX.Opening.Attribute (attribute env) in
        attributes env (attribute :: acc)
    in
    fun env ->
      with_loc
        (fun env ->
          Expect.token env T_LESS_THAN;
          let element =
            match Peek.token env with
            | T_GREATER_THAN -> `Fragment
            | _ ->
              let name = name env in
              let attributes = attributes env [] in
              let selfClosing = Expect.maybe env T_DIV in
              `Element { JSX.Opening.name; selfClosing; attributes }
          in
          Expect.token env T_GREATER_THAN;
          element)
        env

  let closing_element env =
    with_loc
      (fun env ->
        Expect.token env T_LESS_THAN;
        Expect.token env T_DIV;
        let element =
          match Peek.token env with
          | T_GREATER_THAN -> `Fragment
          | _ -> `Element { JSX.Closing.name = name env }
        in
        Expect.token env T_GREATER_THAN;
        element)
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
        | Identifier (_, { Identifier.name }) -> name
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
      | (_, `Element e) -> e.JSX.Opening.selfClosing
      | (_, `Fragment) -> false
    in
    fun env ->
      let leading = Peek.comments env in
      let openingElement = opening_element env in
      Eat.pop_lex_mode env;
      let (children, closingElement) =
        if is_self_closing openingElement then
          (with_loc (fun _ -> []) env, `None)
        else (
          Eat.push_lex_mode env Lex_mode.JSX_CHILD;
          children_and_closing env
        )
      in
      let trailing = Peek.comments env in
      let end_loc =
        match closingElement with
        | `Element (loc, { JSX.Closing.name }) ->
          (match snd openingElement with
          | `Element e ->
            let opening_name = normalize e.JSX.Opening.name in
            if normalize name <> opening_name then
              error env (Parse_error.ExpectedJSXClosingTag opening_name)
          | `Fragment -> error env (Parse_error.ExpectedJSXClosingTag "JSX fragment"));
          loc
        | `Fragment loc ->
          (match snd openingElement with
          | `Element e ->
            error env (Parse_error.ExpectedJSXClosingTag (normalize e.JSX.Opening.name))
          | _ -> ());
          loc
        | _ -> fst openingElement
      in
      let result =
        match openingElement with
        | (start_loc, `Element e) ->
          `Element
            JSX.
              {
                openingElement = (start_loc, e);
                closingElement =
                  (match closingElement with
                  | `Element e -> Some e
                  | _ -> None);
                children;
                comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }
        | (start_loc, `Fragment) ->
          `Fragment
            JSX.
              {
                frag_openingElement = start_loc;
                frag_closingElement =
                  (match closingElement with
                  | `Fragment loc -> loc
                  (* the following are parse erros *)
                  | `Element (loc, _) -> loc
                  | _ -> end_loc);
                frag_children = children;
                frag_comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing ();
              }
      in
      (Loc.btwn (fst openingElement) end_loc, result)

  and element_or_fragment env =
    Eat.push_lex_mode env Lex_mode.JSX_TAG;
    element env
end
