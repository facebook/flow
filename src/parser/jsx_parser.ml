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

  let expression_container env =
    Eat.push_lex_mode env Lex_mode.NORMAL;
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
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

  let identifier env =
    let loc = Peek.loc env in
    let name = Peek.value env in
    Expect.token env T_JSX_IDENTIFIER;
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
        match Peek.token env with
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
            loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
        | _ ->
            error env Error.InvalidJSXAttributeValue;
            let loc = Peek.loc env in
            let raw = "" in
            let value = Ast.Literal.String "" in
            loc, Some (JSX.Attribute.Literal (loc, { Ast.Literal.value; raw;}))
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
        let name = name env in
        let attributes = attributes env [] in
        let selfClosing = Peek.token env = T_DIV in
        if selfClosing then Expect.token env T_DIV;
        let end_loc = Peek.loc env in
        Expect.token env T_GREATER_THAN;
        Eat.pop_lex_mode env;
        Loc.btwn start_loc end_loc, JSX.Opening.({
          name;
          selfClosing;
          attributes;
        })

    let closing_element_without_lt env start_loc =
      Expect.token env T_DIV;
      let name = name env in
      let end_loc = Peek.loc env in
      Expect.token env T_GREATER_THAN;
      (* We double pop to avoid going back to childmode and re-lexing the
       * lookahead *)
      Eat.double_pop_lex_mode env;
      Loc.btwn start_loc end_loc, JSX.Closing.({
        name;
      })

    type element_or_closing =
      | Closing of JSX.Closing.t
      | ChildElement of (Loc.t * JSX.element)


    let rec child env =
      match Peek.token env with
      | T_LCURLY ->
          let expression_container = expression_container env in
          fst expression_container, JSX.ExpressionContainer (snd expression_container)
      | T_JSX_TEXT (loc, value, raw) as token ->
          Expect.token env token;
          loc, JSX.Text { JSX.Text.value; raw; }
      | _ ->
          let element = element env in
          fst element, JSX.Element (snd element)

    and element_without_lt =
      let element_or_closing env =
        Eat.push_lex_mode env Lex_mode.JSX_TAG;
        let start_loc = Peek.loc env in
        Expect.token env T_LESS_THAN;
        match Peek.token env with
        | T_EOF
        | T_DIV -> Closing (closing_element_without_lt env start_loc)
        | _ -> ChildElement (element_without_lt env start_loc)

      in let rec children_and_closing env acc =
        match Peek.token env with
        | T_LESS_THAN -> (
            match element_or_closing env with
            | Closing closingElement ->
                List.rev acc, Some closingElement
            | ChildElement element ->
                let element = fst element, JSX.Element (snd element) in
                children_and_closing env (element::acc))
        | T_EOF ->
            error_unexpected env;
            List.rev acc, None
        | _ ->
            children_and_closing env ((child env)::acc)

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
          if (snd openingElement).JSX.Opening.selfClosing
          then [], None
          else begin
            Eat.push_lex_mode env Lex_mode.JSX_CHILD;
            let ret = children_and_closing env [] in
            ret
          end in
        let end_loc = match closingElement with
        | Some (loc, { JSX.Closing.name }) ->
            let opening_name = normalize (snd openingElement).JSX.Opening.name in
            if normalize name <> opening_name
            then error env (Error.ExpectedJSXClosingTag opening_name);
            loc
        | _ -> fst openingElement in
        Loc.btwn (fst openingElement) end_loc, JSX.({
          openingElement;
          closingElement;
          children;
        })

    and element env =
      let start_loc = Peek.loc env in
      Eat.push_lex_mode env Lex_mode.JSX_TAG;
      Expect.token env T_LESS_THAN;
      element_without_lt env start_loc
end
