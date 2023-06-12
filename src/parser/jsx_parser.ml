(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Token
open Parser_common
open Parser_env
open Flow_ast

module JSX (Parse : Parser_common.PARSER) (Expression : Expression_parser.EXPRESSION) = struct
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
      }
    )

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
        comments = Flow_ast_utils.mk_comments_with_internal_opt ~leading ~trailing ~internal:[] ();
      }
    )

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
                  comments = Flow_ast_utils.mk_comments_with_internal_opt ~internal ();
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
      | T_JSX_IDENTIFIER { raw; _ } -> raw
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

  let names_are_equal =
    let identifiers_are_equal a b =
      let (_, { JSX.Identifier.name = a; _ }) = a in
      let (_, { JSX.Identifier.name = b; _ }) = b in
      String.equal a b
    in
    let rec member_expressions_are_equal a b =
      let (_, { JSX.MemberExpression._object = a_obj; property = a_prop }) = a in
      let (_, { JSX.MemberExpression._object = b_obj; property = b_prop }) = b in
      let objs_equal =
        match (a_obj, b_obj) with
        | (JSX.MemberExpression.Identifier a, JSX.MemberExpression.Identifier b) ->
          identifiers_are_equal a b
        | (JSX.MemberExpression.MemberExpression a, JSX.MemberExpression.MemberExpression b) ->
          member_expressions_are_equal a b
        | _ -> false
      in
      objs_equal && identifiers_are_equal a_prop b_prop
    in
    let namespaced_names_are_equal a b =
      let (_, { JSX.NamespacedName.namespace = a_ns; name = a_name }) = a in
      let (_, { JSX.NamespacedName.namespace = b_ns; name = b_name }) = b in
      identifiers_are_equal a_ns b_ns && identifiers_are_equal a_name b_name
    in
    fun a b ->
      match (a, b) with
      | (JSX.Identifier a, JSX.Identifier b) -> identifiers_are_equal a b
      | (JSX.MemberExpression a, JSX.MemberExpression b) -> member_expressions_are_equal a b
      | (JSX.NamespacedName a, JSX.NamespacedName b) -> namespaced_names_are_equal a b
      | _ -> false

  let loc_of_name = function
    | JSX.Identifier (loc, _) -> loc
    | JSX.NamespacedName (loc, _) -> loc
    | JSX.MemberExpression (loc, _) -> loc

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
                  | _ -> ()
                );
                Some (JSX.Attribute.ExpressionContainer (loc, expression_container))
              | T_JSX_QUOTE_TEXT (loc, value, raw) as token ->
                Expect.token env token;
                let trailing = tag_component_trailing_comments env in
                let comments = Flow_ast_utils.mk_comments_opt ~leading ~trailing () in
                Some (JSX.Attribute.StringLiteral (loc, { Ast.StringLiteral.value; raw; comments }))
              | _ ->
                error env Parse_error.InvalidJSXAttributeValue;
                let loc = Peek.loc env in
                Some
                  (JSX.Attribute.StringLiteral
                     (loc, { Ast.StringLiteral.value = ""; raw = ""; comments = None })
                  )
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
            let targs =
              (* Don't attempt to parse type args if what follows is a closing tag.
                 E.g. in the situation of adding a child `<C><A </C>`
                 Doing so would always be wrong, and having this check improves errors.
              *)
              if
                should_parse_types env
                && Peek.token env = T_LESS_THAN
                && Peek.ith_token ~i:1 env <> T_DIV
              then
                Try.or_else env ~fallback:None Expression.call_type_args
              else
                None
            in
            let attributes = attributes env [] in
            let self_closing = Eat.maybe env T_DIV in
            let element = `Element { JSX.Opening.name; targs; self_closing; attributes } in
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

  let child_is_unpaired opening_name = function
    | ( _,
        JSX.Element
          {
            JSX.opening_element = (_, { JSX.Opening.name = child_opening_name; _ });
            closing_element = Some (_, { JSX.Closing.name = child_closing_name; _ });
            _;
          }
      ) ->
      (not (names_are_equal child_opening_name child_closing_name))
      && names_are_equal opening_name child_closing_name
    | _ -> false

  let rec child ~parent_opening_name env =
    match Peek.token env with
    | T_LCURLY -> expression_container_or_spread_child env
    | T_JSX_CHILD_TEXT (loc, value, raw) as token ->
      Expect.token env token;
      (loc, JSX.Text { JSX.Text.value; raw })
    | _ ->
      (match element_or_fragment ~parent_opening_name env with
      | (loc, `Element element) -> (loc, JSX.Element element)
      | (loc, `Fragment fragment) -> (loc, JSX.Fragment fragment))

  and element =
    let children_and_closing =
      let rec children_and_closing ~parent_opening_name ~opening_name env acc =
        let previous_loc = last_loc env in
        match (acc, opening_name) with
        | (last_child :: rest, Some opening_name) when child_is_unpaired opening_name last_child ->
          (* if the last child's opening and closing tags don't match, and the
             child's closing tag matches ours, then we're in a situation like
             <a><b></b><c></a>, where opening_name = a and the child has opening
             tag c and closing tag a.

             steal the closing tag from the last child, so that <c> has no
             closing tag, but <a>...</a> is properly paired. *)
          let (last_child, closing) =
            match last_child with
            | (loc, JSX.Element ({ JSX.closing_element = Some closing; children; _ } as child)) ->
              let (child_loc, _) = children in
              let loc = Loc.btwn loc child_loc in
              let last_child = (loc, JSX.Element { child with JSX.closing_element = None }) in
              (last_child, `Element closing)
            | _ -> (last_child, `None)
          in
          Eat.pop_lex_mode env;
          (List.rev (last_child :: rest), previous_loc, closing)
        | _ ->
          (match Peek.token env with
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
                  match element ~parent_opening_name:opening_name env with
                  | (loc, `Element e) -> (loc, JSX.Element e)
                  | (loc, `Fragment f) -> (loc, JSX.Fragment f)
                in
                children_and_closing ~parent_opening_name ~opening_name env (child :: acc)
            end
          | T_EOF ->
            error_unexpected env;
            (List.rev acc, previous_loc, `None)
          | _ ->
            let child = child ~parent_opening_name:opening_name env in
            children_and_closing ~parent_opening_name ~opening_name env (child :: acc))
      in
      fun ~parent_opening_name ~opening_name env ->
        let start_loc = Peek.loc env in
        let (children, last_child_loc, closing) =
          children_and_closing ~parent_opening_name ~opening_name env []
        in
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
          _object ^ "." ^ (snd property).Identifier.name
      )
    in
    let is_self_closing = function
      | (_, Ok (`Element e)) -> e.JSX.Opening.self_closing
      | (_, Ok `Fragment) -> false
      | (_, Error _) -> true
    in
    let name_of_opening = function
      | (_, Ok (`Element { JSX.Opening.name; _ }))
      | (_, Error (`Element { JSX.Opening.name; _ })) ->
        Some name
      | (_, Ok `Fragment)
      | (_, Error `Fragment) ->
        None
    in
    fun ~parent_opening_name env ->
      let leading = Peek.comments env in
      let opening_element = opening_element env in
      Eat.pop_lex_mode env;
      let (children, closing_element) =
        if is_self_closing opening_element then
          (with_loc (fun _ -> []) env, `None)
        else (
          Eat.push_lex_mode env Lex_mode.JSX_CHILD;
          let opening_name = name_of_opening opening_element in
          children_and_closing ~parent_opening_name ~opening_name env
        )
      in
      let trailing = Eat.trailing_comments env in
      let end_loc =
        match closing_element with
        | `Element (loc, { JSX.Closing.name }) ->
          (match snd opening_element with
          | Ok (`Element { JSX.Opening.name = opening_name; _ }) ->
            if not (names_are_equal name opening_name) then (
              match parent_opening_name with
              | Some parent_opening_name when names_are_equal parent_opening_name name ->
                (* the opening and closing tags don't match, but the closing
                   tag matches the parent's opening tag. the parent is going
                   to steal the closing tag away from this tag, so error on
                   the opening tag instead. *)
                error_at
                  env
                  ( loc_of_name opening_name,
                    Parse_error.MissingJSXClosingTag (normalize opening_name)
                  )
              | _ ->
                error_at
                  env
                  (loc_of_name name, Parse_error.ExpectedJSXClosingTag (normalize opening_name))
            )
          | Ok `Fragment ->
            error_at env (loc_of_name name, Parse_error.ExpectedJSXClosingTag "JSX fragment")
          | Error _ -> ());
          loc
        | `Fragment loc ->
          (match snd opening_element with
          | Ok (`Element { JSX.Opening.name = opening_name; _ }) ->
            error_at env (loc, Parse_error.ExpectedJSXClosingTag (normalize opening_name))
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
            {
              JSX.frag_opening_element = start_loc;
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

  and element_or_fragment ~parent_opening_name env =
    Eat.push_lex_mode env Lex_mode.JSX_TAG;
    element ~parent_opening_name env
end
