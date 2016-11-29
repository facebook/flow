(*
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Token = Lexer_flow.Token
open Token
open Parser_env
module Ast = Spider_monkey_ast
open Ast
module Error = Parse_error
open Parser_common

(* Sometimes we add the same error for multiple different reasons. This is hard
   to avoid, so instead we just filter the duplicates out. This function takes
   a reversed list of errors and returns the list in forward order with dupes
   removed. This differs from a set because the original order is preserved. *)
let filter_duplicate_errors =
  let module ErrorSet = Set.Make(struct
    type t = Loc.t * Error.t
    let compare (a_loc, a_error) (b_loc, b_error) =
      let loc = Loc.compare a_loc b_loc in
      if loc = 0
      then Pervasives.compare a_error b_error
      else loc
  end) in
  fun errs ->
    let errs = List.rev errs in
    let _, deduped = List.fold_left (fun (set, deduped) err ->
      if ErrorSet.mem err set then (set, deduped)
      else (ErrorSet.add err set, err::deduped)
    ) (ErrorSet.empty, []) errs in
    List.rev deduped

module rec Parse : PARSER = struct
  module Type = Type_parser.Type (Parse)
  module Declaration = Declaration_parser.Declaration (Parse) (Type)
  module Expression = Expression_parser.Expression (Parse) (Type) (Declaration)
  module Object = Object_parser.Object (Parse) (Type) (Declaration) (Expression)
  module Statement = Statement_parser.Statement (Parse) (Type) (Declaration) (Object)
  module Pattern = Pattern_parser.Pattern (Parse) (Type)

  module JSX = struct
    let spread_attribute env =
      Eat.push_lex_mode env Lex_mode.NORMAL;
      let start_loc = Peek.loc env in
      Expect.token env T_LCURLY;
      Expect.token env T_ELLIPSIS;
      let argument = Expression.assignment env in
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

  let rec program env =
    let stmts = module_body_with_directives env (fun _ -> false) in
    let end_loc = Peek.loc env in
    Expect.token env T_EOF;
    let loc = match stmts with
    | [] -> end_loc
    | _ -> Loc.btwn (fst (List.hd stmts)) (fst (List.hd (List.rev stmts))) in
    let comments = List.rev (comments env) in
    loc, stmts, comments

  and directives =
      let check env (loc, token) =
        match token with
        | T_STRING (_, _, _, octal) ->
            if octal then strict_error_at env (loc, Error.StrictOctalLiteral)
        | _ -> failwith ("Nooo: "^(token_to_string token)^"\n")

      in let rec statement_list env term_fn item_fn (string_tokens, stmts) =
        match Peek.token env with
        | T_EOF -> env, string_tokens, stmts
        | t when term_fn t -> env, string_tokens, stmts
        | _ ->
            let string_token = Peek.loc env, Peek.token env in
            let possible_directive = item_fn env in
            let stmts = possible_directive::stmts in
            (match possible_directive with
            | _, Ast.Statement.Expression {
                Ast.Statement.Expression.expression = loc, Ast.Expression.Literal {
                  Ast.Literal.value = Ast.Literal.String str;
                  _;
                }
              } ->
                (* 14.1.1 says that it has to be "use strict" without any
                  * escapes, so "use\x20strict" is disallowed. We could in theory
                  * keep the raw string around, but that's a pain. This is a hack
                  * that actually seems to work pretty well (make sure the string
                  * has the right length)
                  *)
                let len = Loc.(loc._end.column - loc.start.column) in
                let strict =
                  (in_strict_mode env) ||
                  (str = "use strict" && len = 12)
                in
                let string_tokens = string_token::string_tokens in
                statement_list
                  (env |> with_strict strict)
                  term_fn
                  item_fn
                  (string_tokens, stmts)
            | _ ->
                env, string_tokens, stmts)

      in fun env term_fn item_fn ->
        let env, string_tokens, stmts = statement_list env term_fn item_fn ([], []) in
        List.iter (check env) (List.rev string_tokens);
        env, stmts

  (* 15.2 *)
  and module_item env =
    let decorators = Object.decorator_list env in
    match Peek.token env with
    | T_EXPORT -> Statement.export_declaration env decorators
    | T_IMPORT ->
        error_on_decorators env decorators;
        Statement.import_declaration env
    | T_DECLARE when Peek.token ~i:1 env = T_EXPORT ->
        error_on_decorators env decorators;
        Statement.declare_export_declaration env
    | _ -> statement_list_item env ~decorators

  and module_body_with_directives env term_fn =
    let env, directives = directives env term_fn module_item in
    let stmts = module_body ~term_fn env in
    (* Prepend the directives *)
    List.fold_left (fun acc stmt -> stmt::acc) stmts directives

  and module_body =
    let rec module_item_list env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> module_item_list env term_fn (module_item env::acc)

    in fun ~term_fn env ->
      module_item_list env term_fn []

  and statement_list_with_directives ~term_fn env =
    let env, directives = directives env term_fn statement_list_item in
    let stmts = statement_list ~term_fn env in
    (* Prepend the directives *)
    let stmts = List.fold_left (fun acc stmt -> stmt::acc) stmts directives in
    stmts, (in_strict_mode env)

  and statement_list =
    let rec statements env term_fn acc =
      match Peek.token env with
      | T_EOF -> List.rev acc
      | t when term_fn t -> List.rev acc
      | _ -> statements env term_fn ((statement_list_item env)::acc)

    in fun ~term_fn env -> statements env term_fn []


  and statement_list_item ?(decorators=[]) env =
    if not (Peek.is_class env)
    then error_on_decorators env decorators;
    Statement.(match Peek.token env with
    (* Remember kids, these look like statements but they're not
      * statements... (see section 13) *)
    | T_LET -> _let env
    | T_CONST -> var_or_const env
    | _ when Peek.is_function env -> Declaration._function env
    | _ when Peek.is_class env -> class_declaration env decorators
    | T_INTERFACE -> interface env
    | T_DECLARE -> declare env
    | T_TYPE -> type_alias env
    | _ -> statement env)

  and statement env =
    Statement.(match Peek.token env with
    | T_EOF ->
        error_unexpected env;
        Peek.loc env, Ast.Statement.Empty
    | T_SEMICOLON -> empty env
    | T_LCURLY -> block env
    | T_VAR -> var_or_const env
    | T_BREAK -> break env
    | T_CONTINUE -> continue env
    | T_DEBUGGER -> debugger env
    | T_DO -> do_while env
    | T_FOR -> _for env
    | T_IF -> _if env
    | T_RETURN -> return env
    | T_SWITCH -> switch env
    | T_THROW -> throw env
    | T_TRY -> _try env
    | T_WHILE -> _while env
    | T_WITH -> _with env
    | _ when Peek.is_identifier env -> maybe_labeled env
    (* If we see an else then it's definitely an error, but we can probably
     * assume that this is a malformed if statement that is missing the if *)
    | T_ELSE -> _if env
    (* There are a bunch of tokens that aren't the start of any valid
     * statement. We list them here in order to skip over them, rather than
     * getting stuck *)
    | T_COLON
    | T_RPAREN
    | T_RCURLY
    | T_RBRACKET
    | T_COMMA
    | T_PERIOD
    | T_ARROW
    | T_IN
    | T_INSTANCEOF
    | T_CATCH
    | T_FINALLY
    | T_CASE
    | T_DEFAULT
    | T_EXTENDS
    | T_STATIC
    | T_IMPORT (* TODO *)
    | T_EXPORT (* TODO *)
    | T_ELLIPSIS ->
        error_unexpected env;
        Eat.token env;
        statement env
    | _ -> expression env)

  and expression env =
    let expr = Expression.assignment env in
    match Peek.token env with
    | T_COMMA -> Expression.sequence env [expr]
    | _ ->
        expr

  and conditional = Expression.conditional
  and assignment = Expression.assignment
  and left_hand_side = Expression.left_hand_side
  and object_initializer = Object._initializer
  and object_key = Object.key
  and class_declaration = Object.class_declaration
  and class_expression = Object.class_expression
  and array_initializer = Expression.array_initializer

  and is_assignable_lhs = Expression.is_assignable_lhs

  and identifier ?restricted_error env =
    let loc = Peek.loc env in
    let name = Peek.value env in
    (match Peek.token env with
    | T_LET ->
    (* So "let" is disallowed as an identifier in a few situations. 11.6.2.1
     * lists them out. It is always disallowed in strict mode *)
      if in_strict_mode env
      then strict_error env Error.StrictReservedWord
      else
        if no_let env
        then error env (Error.UnexpectedToken name);
      Eat.token env
    | _ when is_strict_reserved name ->
      strict_error env Error.StrictReservedWord;
      Eat.token env
    | T_DECLARE
    | T_OF
    | T_ASYNC
    | T_AWAIT
    | T_TYPE as t ->
        (* These aren't real identifiers *)
        Expect.token env t
    | _ -> Expect.token env T_IDENTIFIER);
    (match restricted_error with
    | Some err when is_restricted name -> strict_error_at env (loc, err)
    | _ -> ());
    loc, name

  and identifier_or_reserved_keyword = Expression.identifier_or_reserved_keyword

  and identifier_with_type =
    let with_loc_helper no_optional restricted_error env =
      let name = identifier ~restricted_error env in
      let optional = not no_optional && Peek.token env = T_PLING in
      if optional then begin
        if not (should_parse_types env)
        then error env Error.UnexpectedTypeAnnotation;
        Expect.token env T_PLING
      end;
      let typeAnnotation =
        if Peek.token env = T_COLON
        then Some (Type.annotation env)
        else None in
      Ast.Pattern.Identifier.({
        name;
        optional;
        typeAnnotation;
      })

    in fun env ?(no_optional=false) restricted_error ->
      with_loc (with_loc_helper no_optional restricted_error) env

  and block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body = statement_list ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }

  and function_block_body env =
    let start_loc = Peek.loc env in
    Expect.token env T_LCURLY;
    let term_fn = fun t -> t = T_RCURLY in
    let body, strict = statement_list_with_directives ~term_fn env in
    let end_loc = Peek.loc env in
    Expect.token env T_RCURLY;
    Loc.btwn start_loc end_loc, { Ast.Statement.Block.body; }, strict

  and jsx_element = JSX.element

  and pattern = Pattern.pattern
  and pattern_from_expr = Pattern.from_expr
end

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)
let do_parse env parser fail =
  let ast = parser env in
  let error_list = filter_duplicate_errors (errors env) in
  if fail && error_list <> []
  then raise (Error.Error error_list);
  ast, error_list

let parse_program fail ?(token_sink=None) ?(parse_options=None) filename content =
  let env = init_env ~token_sink ~parse_options filename content in
  do_parse env Parse.program fail

let program ?(fail=true) ?(token_sink=None) ?(parse_options=None) content =
  parse_program fail ~token_sink ~parse_options None content

let program_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  parse_program fail ~token_sink ~parse_options filename content

(* even if fail=false, still raises an error on a totally invalid token, since
   there's no legitimate fallback. *)
let json_file ?(fail=true) ?(token_sink=None) ?(parse_options=None) content filename =
  let env = init_env ~token_sink ~parse_options filename content in
  match Peek.token env with
  | T_LBRACKET
  | T_LCURLY
  | T_STRING _
  | T_NUMBER _
  | T_TRUE
  | T_FALSE
  | T_NULL ->
    do_parse env Parse.expression fail
  | T_MINUS ->
    (match Peek.token ~i:1 env with
    | T_NUMBER _ ->
      do_parse env Parse.expression fail
    | _ ->
      error_unexpected env;
      raise (Error.Error (errors env)))
  | _ ->
    error_unexpected env;
    raise (Error.Error (errors env))

let jsx_pragma_expression =
  let left_hand_side env =
    let ast = Parse.left_hand_side (with_no_new true env) in
    Expect.token env T_EOF;
    ast

  in fun content filename ->
    let env = init_env ~token_sink:None ~parse_options:None filename content in
    do_parse env left_hand_side true
