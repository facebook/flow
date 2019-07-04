(**
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
module Error = Parse_error

module JSX (Parse: Parser_common.PARSER) = struct
  let spread_attribute env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    Expect.token env T_ELLIPSIS;
    let argument = Parse.assignment env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Eat.pop_lex_mode env;
    Loc.btwn start_loc end_loc, JSX.SpreadAttribute.({
      argument;
    })

  let expression_container' env start_loc =
    let expression = if Peek.token env = T_RCURLY
      then
        let empty_loc = Loc.btwn_exclusive start_loc (Peek.loc env) in
        JSX.ExpressionContainer.EmptyExpression empty_loc
      else JSX.ExpressionContainer.Expression (Parse.expression env) in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Eat.pop_lex_mode env;
    Loc.btwn start_loc end_loc, JSX.ExpressionContainer.({
      expression;
    })

  let expression_container env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    expression_container' env start_loc

  let expression_container_or_spread_child env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
      match Peek.token env with
        | T_ELLIPSIS ->
            Expect.token env T_ELLIPSIS;
            let expr = Parse.assignment env in
            let end_loc = Peek.loc env in
            Expect.token env T_RCURLY;
            Eat.pop_lex_mode env;
            Loc.btwn start_loc end_loc, JSX.SpreadChild expr
        | _ ->
          let expression_container = expression_container' env start_loc in
          fst expression_container, JSX.ExpressionContainer (snd expression_container)

  let identifier env =
    let loc = Peek.loc env in
    let name = match Peek.token env with
    | T_JSX_IDENTIFIER { raw } -> raw
    | _ -> error_unexpected env; ""
    in
    Eat.token env;
    loc, JSX.Identifier.({ name; })

  let name =
    let rec member_expression env member =
      match Peek.token env with
      | T_PERIOD ->
          let _object = JSX.MemberExpression.MemberExpression member in
          Expect.token env T_PERIOD;
          let property = identifier env in
          let loc = Loc.btwn (fst member) (fst property) in
          let member = loc, JSX.MemberExpression.({
            _object;
            property;
          }) in
          member_expression env member
      | _ -> member

    in fun env ->
      let name = identifier env in
      match Peek.token env with
      | T_COLON ->
          let namespace = name in
          Expect.token env T_COLON;
          let name = identifier env in
          let loc = Loc.btwn (fst namespace) (fst name) in
          JSX.NamespacedName (loc, JSX.NamespacedName.({
            namespace;
            name;
          }))
      | T_PERIOD ->
          let _object = JSX.MemberExpression.Identifier name in
          Expect.token env T_PERIOD;
          let property = identifier env in
          let loc = Loc.btwn (fst name) (fst property) in
          let member = loc, JSX.MemberExpression.({
            _object;
            property;
          }) in
          JSX.MemberExpression (member_expression env member)
      | _ -> JSX.Identifier name


  let attribute env =
    let start_loc = Peek.loc env in
    let name = identifier env in
    let end_loc, name =
      if Peek.token env = T_COLON
      then begin
        Expect.token env T_COLON;
        let namespace = name in
        let name = identifier env in
        let loc = Loc.btwn (fst namespace) (fst name) in
        loc, JSX.Attribute.NamespacedName (loc, JSX.NamespacedName.({
          namespace;
          name;
        }))
      end else fst name, JSX.Attribute.Identifier name in
    let end_loc, value =
      if Peek.token env = T_ASSIGN
      then begin
        Expect.token env T_ASSIGN;
        let leading = Peek.comments env in
        let tkn = Peek.token env in
        let trailing = Peek.comments env in
        match tkn with
        | T_LCURLY ->
            let loc, expression_container = expression_container env in
            begin
              let open JSX.ExpressionContainer in
              match expression_container.expression with
              | EmptyExpression _ ->
                  error_at env (loc, Error.JSXAttributeValueEmptyExpression);
              | _ -> ()
            end;
            loc, Some (JSX.Attribute.ExpressionContainer (loc, expression_container))
        | T_JSX_TEXT (loc, value, raw) as token ->
            Expect.token env token;
            let value = Ast.Literal.String value in
            loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw; comments= (Flow_ast_utils.mk_comments_opt ~leading ~trailing ());}))
        | _ ->
            error env Error.InvalidJSXAttributeValue;
            let loc = Peek.loc env in
            let raw = "" in
            let value = Ast.Literal.String "" in
            loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;comments= (Flow_ast_utils.mk_comments_opt ~leading ~trailing ());}))
      end else end_loc, None in
    Loc.btwn start_loc end_loc, JSX.Attribute.({
      name;
      value;
    })

    let opening_element_without_lt =
      let rec attributes env acc =
        match Peek.token env with
        | T_EOF
        | T_DIV
        | T_GREATER_THAN -> List.rev acc
        | T_LCURLY ->
            let attribute = JSX.Opening.SpreadAttribute (spread_attribute env) in
            attributes env (attribute::acc)
        | _ ->
            let attribute = JSX.Opening.Attribute (attribute env) in
            attributes env (attribute::acc)

      in fun env start_loc ->
        let (name, attributes, selfClosing) = match Peek.token env with
          | T_GREATER_THAN ->
            (None, [], false)
          | _ ->
            let name = Some (name env) in
            let attributes = attributes env [] in
            let selfClosing = Peek.token env = T_DIV in
            (name, attributes, selfClosing) in
        if selfClosing then Expect.token env T_DIV;
        let end_loc = Peek.loc env in
        Expect.token env T_GREATER_THAN;
        Eat.pop_lex_mode env;
        match name with
        | Some name ->
          Loc.btwn start_loc end_loc, `Element JSX.Opening.({
            name;
            selfClosing;
            attributes;
          })
        | None ->
          Loc.btwn start_loc end_loc, `Fragment

    let closing_element_without_lt env start_loc =
      Expect.token env T_DIV;
      let name = match Peek.token env with
        | T_GREATER_THAN -> None
        | _ -> Some (name env) in
      let end_loc = Peek.loc env in
      Expect.token env T_GREATER_THAN;
      (* We double pop to avoid going back to childmode and re-lexing the
       * lookahead *)
      Eat.double_pop_lex_mode env;
      match name with
      | Some name ->
        Loc.btwn start_loc end_loc, `Element JSX.Closing.({
          name;
        })
      | None ->
        Loc.btwn start_loc end_loc, `Fragment

    type element_or_closing =
      | Closing of (Loc.t, Loc.t) JSX.Closing.t
      | ClosingFragment of Loc.t
      | ChildElement of (Loc.t * (Loc.t, Loc.t) JSX.element)
      | ChildFragment of (Loc.t * (Loc.t, Loc.t) JSX.fragment)

    let rec child env =
      match Peek.token env with
      | T_LCURLY -> expression_container_or_spread_child env
      | T_JSX_TEXT (loc, value, raw) as token ->
          Expect.token env token;
          loc, JSX.Text { JSX.Text.value; raw; }
      | _ ->
          (match element_or_fragment env with
          | (loc, `Element element) -> loc, JSX.Element element
          | (loc, `Fragment fragment) -> loc, JSX.Fragment fragment)

    and element_without_lt =
      let element_or_closing env =
        Eat.push_lex_mode env Lex_mode.JSX_TAG;
        let start_loc = Peek.loc env in
        Expect.token env T_LESS_THAN;
        match Peek.token env with
        | T_EOF
        | T_DIV -> (match closing_element_without_lt env start_loc with
            | (loc, `Element ec) -> Closing (loc, ec)
            | (loc, `Fragment) -> ClosingFragment loc)
        | _ -> (match element_without_lt env start_loc with
            | (loc, `Element e) -> ChildElement (loc, e)
            | (loc, `Fragment f) -> ChildFragment (loc, f))

      in let children_and_closing =
        let rec children_and_closing env acc =
          let previous_loc = last_loc env in
          match Peek.token env with
          | T_LESS_THAN -> (
              match element_or_closing env with
              | Closing closingElement ->
                  List.rev acc, previous_loc, `Element closingElement
              | ClosingFragment closingFragment ->
                  List.rev acc, previous_loc, `Fragment closingFragment
              | ChildElement element ->
                  let element = fst element, JSX.Element (snd element) in
                  children_and_closing env (element::acc)
              | ChildFragment fragment ->
                  let fragment = fst fragment, JSX.Fragment (snd fragment) in
                  children_and_closing env (fragment::acc))
          | T_EOF ->
              error_unexpected env;
              List.rev acc, previous_loc, `None
          | _ ->
              children_and_closing env ((child env)::acc)
        in
        fun env ->
          let start_loc = Peek.loc env in
          let children, last_child_loc, closing = children_and_closing env [] in
          let last_child_loc = match last_child_loc with Some x -> x | None -> start_loc in
          (* It's a little bit tricky to untangle the parsing of the child elements from the parsing
           * of the closing element, so we can't easily use `with_loc` here. Instead, we'll use the
           * same logic that `with_loc` uses, but manipulate the locations explicitly. *)
          let children_loc = Loc.btwn start_loc last_child_loc in
          ((children_loc, children), closing)

      in let rec normalize name = JSX.(match name with
        | Identifier (_, { Identifier.name }) -> name
        | NamespacedName (_, { NamespacedName.namespace; name; }) ->
            (snd namespace).Identifier.name ^ ":" ^ (snd name).Identifier.name
        | MemberExpression (_, { MemberExpression._object; property; }) ->
            let _object = match _object with
            | MemberExpression.Identifier (_, {Identifier.name=id; _;}) -> id
            | MemberExpression.MemberExpression e ->
                normalize (JSX.MemberExpression e) in
                _object ^ "." ^ (snd property).Identifier.name
      )

      in fun env start_loc ->
        let openingElement = opening_element_without_lt env start_loc in
        let children, closingElement =
          let selfClosing = match snd openingElement with
            | `Element e -> e.JSX.Opening.selfClosing
            | `Fragment -> false in
          if selfClosing then
            with_loc (fun _ -> []) env, `None
          else begin
            Eat.push_lex_mode env Lex_mode.JSX_CHILD;
            children_and_closing env
          end in
        let end_loc = match closingElement with
          | `Element (loc, { JSX.Closing.name }) ->
              (match snd openingElement with
                | `Element e ->
                  let opening_name = normalize e.JSX.Opening.name in
                  if normalize name <> opening_name
                  then error env (Error.ExpectedJSXClosingTag opening_name)
                | `Fragment -> error env (Error.ExpectedJSXClosingTag "JSX fragment"));
              loc
          | `Fragment loc ->
              (match snd openingElement with
              | `Element e -> error env (Error.ExpectedJSXClosingTag (normalize e.JSX.Opening.name))
              | _ -> ());
              loc
          | _ -> fst openingElement in
        match snd openingElement with
        | `Element e ->
          Loc.btwn (fst openingElement) end_loc, `Element JSX.({
            openingElement = (fst openingElement, e);
            closingElement = (match closingElement with
                             | `Element e -> Some e
                             | _ -> None);
            children;
          })
        | `Fragment ->
          Loc.btwn (fst openingElement) end_loc, `Fragment JSX.({
            frag_openingElement = fst openingElement;
            frag_closingElement = (match closingElement with
                              | `Fragment loc -> loc
                              (* the following are parse erros *)
                              | `Element (loc, _) -> loc
                              | _ -> end_loc);
            frag_children = children;
          })

    and element_or_fragment env =
      let start_loc = Peek.loc env in
      Eat.push_lex_mode env Lex_mode.JSX_TAG;
      Expect.token env T_LESS_THAN;
      element_without_lt env start_loc
end
