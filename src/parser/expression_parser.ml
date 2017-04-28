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
open Parser_common

module type EXPRESSION = sig
  val array_initializer: env -> Loc.t * Expression.Array.t
  val assignment: env -> Expression.t
  val conditional: env -> Expression.t
  val identifier_or_reserved_keyword: env -> Identifier.t * (Loc.t * Parse_error.t) option
  val is_assignable_lhs: Expression.t -> bool
  val left_hand_side: env -> Expression.t
  val number: env -> number_type -> float
  val sequence: env -> Expression.t list -> Expression.t
end

module Expression
  (Parse: PARSER)
  (Type: Type_parser.TYPE)
  (Declaration: Declaration_parser.DECLARATION)
: EXPRESSION = struct
  type op_precedence = Left_assoc of int | Right_assoc of int
  let is_tighter a b =
    let a_prec = match a with Left_assoc x -> x | Right_assoc x -> x - 1 in
    let b_prec = match b with Left_assoc x -> x | Right_assoc x -> x in
    a_prec >= b_prec

  (* AssignmentExpression :
   *   ConditionalExpression
   *   LeftHandSideExpression = AssignmentExpression
   *   LeftHandSideExpression AssignmentOperator AssignmentExpression
   *   ArrowFunctionFunction
   *
   *   Originally we were parsing this without backtracking, but
   *   ArrowFunctionExpression got too tricky. Oh well.
   *)
  let rec assignment =
    let assignment_but_not_arrow_function env =
      let expr = conditional env in
      (match assignment_op env with
      | Some operator ->
        if not (is_assignable_lhs expr)
        then error_at env (fst expr, Error.InvalidLHSInAssignment);

        (match expr with
        | loc, Expression.Identifier (_, name)
          when is_restricted name ->
            strict_error_at env (loc, Error.StrictLHSAssignment)
        | _ -> ());

        let left = Parse.pattern_from_expr env expr in
        let right = assignment env in
        let loc = Loc.btwn (fst left) (fst right) in

        loc, Expression.(Assignment Assignment.({
          operator;
          left;
          right;
        }))
      | _ -> expr)

    in let error_callback _ _ = raise Try.Rollback

    (* So we may or may not be parsing the first part of an arrow function
     * (the part before the =>). We might end up parsing that whole thing or
     * we might end up parsing only part of it and thinking we're done. We
     * need to look at the next token to figure out if we really parsed an
     * assignment expression or if this is just the beginning of an arrow
     * function *)
    in let try_assignment_but_not_arrow_function env =
      let env = env |> with_error_callback error_callback in
      let ret = assignment_but_not_arrow_function env in
      match Peek.token env with
      | T_ARROW (* x => 123 *)
      | T_COLON -> (* (x): number => 123 *)
        raise Try.Rollback
      (* async x => 123 -- and we've already parsed async as an identifier
       * expression *)
      | _ when Peek.is_identifier env -> begin match snd ret with
        | Expression.Identifier (_, "async")
            when not (Peek.is_line_terminator env) ->
          raise Try.Rollback
        | _ -> ret
        end
      | _ -> ret
    in fun env ->
      match Peek.token env, Peek.is_identifier env with
      | T_YIELD, _ when (allow_yield env) -> yield env
      | T_LPAREN, _
      | T_LESS_THAN, _
      | _, true ->

        (* Ok, we don't know if this is going to be an arrow function or a
         * regular assignment expression. Let's first try to parse it as an
         * assignment expression. If that fails we'll try an arrow function.
         *)
        (match Try.to_parse env try_assignment_but_not_arrow_function with
        | Try.ParsedSuccessfully expr -> expr
        | Try.FailedToParse ->
          (match Try.to_parse env try_arrow_function with
            | Try.ParsedSuccessfully expr -> expr
            | Try.FailedToParse ->

                (* Well shoot. It doesn't parse cleanly as a normal
                 * expression or as an arrow_function. Let's treat it as a
                 * normal assignment expression gone wrong *)
                assignment_but_not_arrow_function env
          )
        )
      | _ -> assignment_but_not_arrow_function env

  and yield env = with_loc (fun env ->
    Expect.token env T_YIELD;
    if not (allow_yield env)
    then error env Error.IllegalYield;
    let delegate = Expect.maybe env T_MULT in
    let has_argument = not (
      Peek.token env = T_SEMICOLON || Peek.is_implicit_semicolon env
    ) in
    let argument =
      if delegate || has_argument
      then Some (assignment env)
      else None in
    Expression.(Yield Yield.({
      argument;
      delegate;
    }))
  ) env

  and is_lhs = Expression.(function
    | _, Identifier _
    | _, Member _
    | _, MetaProperty _
      -> true

    | _, Array _
    | _, ArrowFunction _
    | _, Assignment _
    | _, Binary _
    | _, Call _
    | _, Class _
    | _, Comprehension _
    | _, Conditional _
    | _, Function _
    | _, Generator _
    | _, Import _
    | _, JSXElement _
    | _, Literal _
    | _, Logical _
    | _, New _
    | _, Object _
    | _, Sequence _
    | _, Super
    | _, TaggedTemplate _
    | _, TemplateLiteral _
    | _, This
    | _, TypeCast _
    | _, Unary _
    | _, Update _
    | _, Yield _
      -> false
  )

  and is_assignable_lhs = Expression.(function
    | _, Array _
    | _, Identifier _
    | _, Member _
    | _, MetaProperty _
    | _, Object _
      -> true

    | _, ArrowFunction _
    | _, Assignment _
    | _, Binary _
    | _, Call _
    | _, Class _
    | _, Comprehension _
    | _, Conditional _
    | _, Function _
    | _, Generator _
    | _, Import _
    | _, JSXElement _
    | _, Literal _
    | _, Logical _
    | _, New _
    | _, Sequence _
    | _, Super
    | _, TaggedTemplate _
    | _, TemplateLiteral _
    | _, This
    | _, TypeCast _
    | _, Unary _
    | _, Update _
    | _, Yield _
      -> false
  )


  and assignment_op env =
    let op = Expression.Assignment.(match Peek.token env with
    | T_RSHIFT3_ASSIGN -> Some RShift3Assign
    | T_RSHIFT_ASSIGN -> Some RShiftAssign
    | T_LSHIFT_ASSIGN -> Some LShiftAssign
    | T_BIT_XOR_ASSIGN -> Some BitXorAssign
    | T_BIT_OR_ASSIGN -> Some BitOrAssign
    | T_BIT_AND_ASSIGN -> Some BitAndAssign
    | T_MOD_ASSIGN -> Some ModAssign
    | T_DIV_ASSIGN -> Some DivAssign
    | T_MULT_ASSIGN -> Some MultAssign
    | T_EXP_ASSIGN -> Some ExpAssign
    | T_MINUS_ASSIGN -> Some MinusAssign
    | T_PLUS_ASSIGN -> Some PlusAssign
    | T_ASSIGN -> Some Assign
    | _ -> None) in
    if op <> None then Eat.token env;
    op

  and conditional env =
    let start_loc = Peek.loc env in
    let expr = logical env in
    if Peek.token env = T_PLING
    then begin
      Expect.token env T_PLING;
      (* no_in is ignored for the consequent *)
      let env' = env |> with_no_in false in
      let consequent = assignment env' in
      Expect.token env T_COLON;
      let end_loc, alternate = with_loc assignment env in
      let loc = Loc.btwn start_loc end_loc in
      loc, Expression.(Conditional Conditional.({
        test = expr;
        consequent;
        alternate;
      }))
    end else expr

  and logical =
    let open Expression in
    let make_logical left right operator loc =
      loc, Logical Logical.({operator; left; right;})
    in let rec logical_and env left lloc =
      match Peek.token env with
      | T_AND ->
          Expect.token env T_AND;
          let rloc, right = with_loc binary env in
          let loc = Loc.btwn lloc rloc in
          logical_and env (make_logical left right Logical.And loc) loc
      | _  -> lloc, left
    and logical_or env left lloc =
      match Peek.token env with
      | T_OR ->
          Expect.token env T_OR;
          let rloc, right = with_loc binary env in
          let rloc, right = logical_and env right rloc in
          let loc = Loc.btwn lloc rloc in
          logical_or env (make_logical left right Logical.Or loc) loc
      | _ -> lloc, left
    in fun env ->
      let loc, left = with_loc binary env in
      let loc, left = logical_and env left loc in
      let _, type_ = logical_or env left loc in
      type_

  and binary =
    let binary_op env =
      let ret = Expression.Binary.(match Peek.token env with
      (* Most BinaryExpression operators are left associative *)
      (* Lowest pri *)
      | T_BIT_OR -> Some (BitOr, Left_assoc 2)
      | T_BIT_XOR -> Some (Xor, Left_assoc 3)
      | T_BIT_AND -> Some (BitAnd, Left_assoc 4)
      | T_EQUAL -> Some (Equal, Left_assoc 5)
      | T_STRICT_EQUAL -> Some (StrictEqual, Left_assoc 5)
      | T_NOT_EQUAL -> Some (NotEqual, Left_assoc 5)
      | T_STRICT_NOT_EQUAL -> Some (StrictNotEqual, Left_assoc 5)
      | T_LESS_THAN -> Some (LessThan, Left_assoc 6)
      | T_LESS_THAN_EQUAL -> Some (LessThanEqual, Left_assoc 6)
      | T_GREATER_THAN -> Some (GreaterThan, Left_assoc 6)
      | T_GREATER_THAN_EQUAL -> Some (GreaterThanEqual, Left_assoc 6)
      | T_IN ->
          if (no_in env) then None else Some (In, Left_assoc 6)
      | T_INSTANCEOF -> Some (Instanceof, Left_assoc 6)
      | T_LSHIFT -> Some (LShift, Left_assoc 7)
      | T_RSHIFT -> Some (RShift, Left_assoc 7)
      | T_RSHIFT3 -> Some (RShift3, Left_assoc 7)
      | T_PLUS -> Some (Plus, Left_assoc 8)
      | T_MINUS -> Some (Minus, Left_assoc 8)
      | T_MULT -> Some (Mult, Left_assoc 9)
      | T_DIV -> Some (Div, Left_assoc 9)
      | T_MOD -> Some (Mod, Left_assoc 9)
      | T_EXP -> Some (Exp, Right_assoc 10)
      (* Highest priority *)
      | _ -> None)
      in if ret <> None then Eat.token env;
      ret

    in let make_binary left right operator loc =
      loc, Expression.(Binary Binary.({
        operator;
        left;
        right;
      }))

    in let rec add_to_stack right (rop, rpri) rloc = function
      | (left, (lop, lpri), lloc)::rest when is_tighter lpri rpri ->
          let loc = Loc.btwn lloc rloc in
          let right = make_binary left right lop loc in
          add_to_stack right (rop, rpri) loc rest
      | stack -> (right, (rop, rpri), rloc)::stack

    in let rec collapse_stack right rloc = function
      | [] -> right
      | (left, (lop, _), lloc)::rest ->
          let loc = Loc.btwn lloc rloc in
          collapse_stack (make_binary left right lop loc) loc rest

    in let rec helper env stack =
      let start_loc = Peek.loc env in
      let is_unary = peek_unary_op env <> None in
      let right = unary (env |> with_no_in false) in
      let end_loc = match last_loc env with
      | Some loc -> loc
      | None -> fst right
      in
      let right_loc = Loc.btwn start_loc end_loc in
      if Peek.token env = T_LESS_THAN
      then begin
        match right with
        | _, Expression.JSXElement _ ->
            error env Error.AdjacentJSXElements
        | _ -> ()
      end;
      match binary_op env with
      | None ->
        collapse_stack right right_loc stack
      | Some (rop, rpri) ->
        if is_unary && rop = Expression.Binary.Exp then
          error_at env (right_loc, Error.InvalidLHSInExponentiation);
        helper env (add_to_stack right (rop, rpri) right_loc stack)

    in fun env -> helper env []

  and peek_unary_op env =
    let open Expression.Unary in
    match Peek.token env with
    | T_NOT -> Some Not
    | T_BIT_NOT -> Some BitNot
    | T_PLUS -> Some Plus
    | T_MINUS -> Some Minus
    | T_TYPEOF -> Some Typeof
    | T_VOID -> Some Void
    | T_DELETE -> Some Delete
    (* If we are in a unary expression context, and within an async function,
     * assume that a use of "await" is intended as a keyword, not an ordinary
     * identifier. This is a little bit inconsistent, since it can be used as
     * an identifier in other contexts (such as a variable name), but it's how
     * Babel does it. *)
    | T_AWAIT when allow_await env -> Some Await
    | _ -> None

  and unary env =
    let begin_loc = Peek.loc env in
    let op = peek_unary_op env in
    match op with
    | None -> begin
        let op = Expression.Update.(match Peek.token env with
        | T_INCR -> Some Increment
        | T_DECR -> Some Decrement
        | _ -> None) in
        match op with
        | None -> postfix env
        | Some operator ->
            Eat.token env;
            let end_loc, argument = with_loc unary env in
            if not (is_lhs argument)
            then error_at env (fst argument, Error.InvalidLHSInAssignment);
            (match argument with
            | _, Expression.Identifier (_, name)
              when is_restricted name ->
                strict_error env Error.StrictLHSPrefix
            | _ -> ());
            Loc.btwn begin_loc end_loc, Expression.(Update Update.({
              operator;
              prefix = true;
              argument;
            }))
      end
    | Some operator ->
      Eat.token env;
      let end_loc, argument = with_loc unary env in
      let loc = Loc.btwn begin_loc end_loc in
      Expression.(match operator, argument with
      | Unary.Delete, (_, Identifier _) ->
          strict_error_at env (loc, Error.StrictDelete)
      | _ -> ());
      loc, Expression.(Unary Unary.({
        operator;
        prefix = true;
        argument;
      }))

  and postfix env =
    let argument = left_hand_side env in
    (* No line terminator allowed before operator *)
    if Peek.is_line_terminator env
    then argument
    else let op = Expression.Update.(match Peek.token env with
    | T_INCR -> Some Increment
    | T_DECR -> Some Decrement
    | _ -> None) in
    match op with
    | None -> argument
    | Some operator ->
        if not (is_lhs argument)
        then error_at env (fst argument, Error.InvalidLHSInAssignment);
        (match argument with
        | _, Expression.Identifier (_, name)
          when is_restricted name ->
            strict_error env Error.StrictLHSPostfix
        | _ -> ());
        let end_loc = Peek.loc env in
        Eat.token env;
        Loc.btwn (fst argument) end_loc, Expression.(Update Update.({
          operator;
          prefix = false;
          argument;
        }))

  and left_hand_side env =
    let start_loc = Peek.loc env in
    let allow_new = not (no_new env) in
    let env = with_no_new false env in
    let expr = match Peek.token env with
    | T_NEW when allow_new -> new_expression env
    | T_IMPORT -> import env start_loc
    | _ when Peek.is_function env -> _function env
    | _ -> primary env in
    let expr = member env start_loc expr in
    call env start_loc expr

  and import env start_loc =
    Expect.token env T_IMPORT;
    Expect.token env T_LPAREN;
    let arg = assignment (with_no_in false env) in
    Expect.token env T_RPAREN;
    Expression.(Loc.btwn start_loc (fst arg), Import arg)

  and call env start_loc left =
    match Peek.token env with
    | T_LPAREN when not (no_call env) ->
        let args_loc, arguments = arguments env in
        let loc = Loc.btwn start_loc args_loc in
        call env start_loc (loc, Expression.(Call Call.({
          callee = left;
          arguments;
        })))
    | T_LBRACKET ->
        Expect.token env T_LBRACKET;
        let expr = Parse.expression env in
        let last_loc = Peek.loc env in
        let loc = Loc.btwn start_loc last_loc in
        Expect.token env T_RBRACKET;
        call env start_loc (loc, Expression.(Member Member.({
          _object  = left;
          property = PropertyExpression expr;
          computed = true;
        })))
    | T_PERIOD ->
        Expect.token env T_PERIOD;
        let id, _ = identifier_or_reserved_keyword env in
        let loc = Loc.btwn start_loc (fst id) in
        call env start_loc (loc, Expression.(Member Member.({
          _object  = left;
          property = PropertyIdentifier id;
          computed = false;
        })))
    | T_TEMPLATE_PART part ->
        call env start_loc (tagged_template env start_loc left part)
    | _ -> left

  and new_expression env =
    let start_loc = Peek.loc env in
    Expect.token env T_NEW;

    if in_function env && Peek.token env = T_PERIOD then begin
      Expect.token env T_PERIOD;
      let meta = start_loc, "new" in
      if Peek.value env = "target" then
        let property = Parse.identifier env in
        let end_loc = fst property in
        Loc.btwn start_loc end_loc, Expression.(MetaProperty MetaProperty.({
          meta;
          property;
        }))
      else begin
        error_unexpected env;
        Eat.token env; (* skip unknown identifier *)
        start_loc, Expression.Identifier meta (* return `new` identifier *)
      end
    end else
      let callee_loc = Peek.loc env in
      let expr = match Peek.token env with
      | T_NEW -> new_expression env
      | _ when Peek.is_function env -> _function env
      | _ -> primary env in
      let callee = member (env |> with_no_call true) callee_loc expr in
      (* You can do something like
       *   new raw`42`
       *)
      let callee = match Peek.token env with
      | T_TEMPLATE_PART part -> tagged_template env callee_loc callee part
      | _ -> callee in
      let end_loc, arguments = match Peek.token env with
      | T_LPAREN -> arguments env
      | _ -> fst callee, [] in

      Loc.btwn start_loc end_loc, Expression.(New New.({
        callee;
        arguments;
      }))

  and arguments =
    let argument env =
      match Peek.token env with
      | T_ELLIPSIS ->
          let start_loc = Peek.loc env in
          Expect.token env T_ELLIPSIS;
          let argument = assignment env in
          let loc = Loc.btwn start_loc (fst argument) in
          Expression.(Spread (loc, SpreadElement.({
            argument;
          })))
      | _ -> Expression.Expression (assignment env)

    in let rec arguments' env acc =
      match Peek.token env with
      | T_EOF
      | T_RPAREN -> List.rev acc
      | _ ->
          let acc = (argument env)::acc in
          if Peek.token env <> T_RPAREN
          then Expect.token env T_COMMA;
          arguments' env acc

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LPAREN;

      let args = arguments' env []

      in let end_loc = Peek.loc env in
      Expect.token env T_RPAREN;
      Loc.btwn start_loc end_loc, args

  and member env start_loc left =
    match Peek.token env with
    | T_LBRACKET ->
        Expect.token env T_LBRACKET;
        let expr = Parse.expression (env |> with_no_call false) in
        let last_loc = Peek.loc env in
        Expect.token env T_RBRACKET;
        let loc = Loc.btwn start_loc last_loc in
        call env start_loc (loc, Expression.(Member Member.({
          _object  = left;
          property = PropertyExpression expr;
          computed = true;
        })))
    | T_PERIOD ->
        Expect.token env T_PERIOD;
        let id, _ = identifier_or_reserved_keyword env in
        let loc = Loc.btwn start_loc (fst id) in
        call env start_loc (loc, Expression.(Member Member.({
          _object  = left;
          property = PropertyIdentifier id;
          computed = false;
        })))
    | T_TEMPLATE_PART part ->
        call env start_loc (tagged_template env start_loc left part)
    | _ -> left

  and _function env =
    let start_loc = Peek.loc env in
    let async = Declaration.async env in
    Expect.token env T_FUNCTION;
    let generator = Declaration.generator env in
    let id, typeParameters =
      if Peek.token env = T_LPAREN
      then None, None
      else begin
        let id = match Peek.token env with
          | T_LESS_THAN -> None
          | _ -> Some (Parse.identifier ~restricted_error:Error.StrictFunctionName env) in
        id, Type.type_parameter_declaration env
      end in
    let params = Declaration.function_params env in
    let returnType, predicate = Type.annotation_and_predicate_opt env in
    let end_loc, body, strict =
      Declaration.function_body env ~async ~generator in
    let simple = Declaration.is_simple_function_params params in
    Declaration.strict_post_check env ~strict ~simple id params;
    let expression = Function.(
      match body with
      | BodyBlock _ -> false
      | BodyExpression _ -> true) in
    Loc.btwn start_loc end_loc, Expression.(Function Function.({
      id;
      params;
      body;
      generator;
      async;
      predicate;
      expression;
      returnType;
      typeParameters;
    }))

  and number env number_type =
    let value = Peek.value env in
    let value = match number_type with
    | LEGACY_OCTAL ->
      strict_error env Error.StrictOctalLiteral;
      begin try Int64.to_float (Int64.of_string ("0o"^value))
      with Failure _ -> failwith ("Invalid legacy octal "^value)
      end
    | BINARY
    | OCTAL ->
      begin try Int64.to_float (Int64.of_string value)
      with Failure _ -> failwith ("Invalid binary/octal "^value)
      end
    | NORMAL ->
      begin try Lexer.FloatOfString.float_of_string value
      with
      | _ when Sys.win32 ->
        error env Parse_error.WindowsFloatOfString;
        789.0
      | Failure _ ->
        failwith ("Invalid number "^value)
      end
    in
    Expect.token env (T_NUMBER number_type);
    value

  and primary env =
    let loc = Peek.loc env in
    match Peek.token env with
    | T_THIS ->
        Expect.token env T_THIS;
        loc, Expression.This
    | T_NUMBER number_type ->
        let raw = Peek.value env in
        let value = Literal.Number (number env number_type) in
        loc, Expression.(Literal { Literal.value; raw; })
    | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let value = Literal.String value in
        loc, Expression.(Literal { Literal.value; raw; })
    | (T_TRUE | T_FALSE) as token ->
        let raw = Peek.value env in
        Expect.token env token;
        let value = (Literal.Boolean (token = T_TRUE)) in
        loc, Expression.(Literal { Literal.value; raw; })
    | T_NULL ->
        let raw = Peek.value env in
        Expect.token env T_NULL;
        let value = Literal.Null in
        loc, Expression.(Literal { Literal.value; raw; })
    | T_LPAREN -> group env
    | T_LCURLY -> object_initializer env
    | T_LBRACKET ->
        let loc, arr = array_initializer env in
        loc, Expression.Array arr
    | T_DIV
    | T_DIV_ASSIGN -> regexp env
    | T_LESS_THAN ->
        let loc, element = Parse.jsx_element env in
        loc, Expression.JSXElement element
    | T_TEMPLATE_PART part ->
        let loc, template = template_literal env part in
        loc, Expression.(TemplateLiteral template)
    | T_CLASS -> Parse.class_expression env
    | T_SUPER ->
        let loc = Peek.loc env in
        Expect.token env T_SUPER;
        loc, Expression.Super
    | _ when Peek.is_identifier env ->
        let id = Parse.identifier env in
        fst id, Expression.Identifier id
    | t ->
        error_unexpected env;
        (* Let's get rid of the bad token *)
        if t = T_ERROR
        then Eat.token env;
        (* Really no idea how to recover from this. I suppose a null
         * expression is as good as anything *)
        let value = Literal.Null in
        let raw = "null" in
        loc, Expression.(Literal { Literal.value; raw; })

  and object_initializer env =
    let loc, obj = Parse.object_initializer env in
    loc, Expression.Object obj

  and template_literal =
    let rec template_parts env quasis expressions =
      let expr = Parse.expression env in
      let expressions = expr::expressions in
      match Peek.token env with
      | T_RCURLY ->
          Eat.push_lex_mode env Lex_mode.TEMPLATE;
          let loc, part, is_tail = match Peek.token env with
          | T_TEMPLATE_PART (loc, {cooked; raw; _}, tail) ->
              let open Ast.Expression.TemplateLiteral in
              Eat.token env;
              loc, { Element.value = { Element.cooked; raw; }; tail; }, tail
          | _ -> assert false in
          Eat.pop_lex_mode env;
          let quasis = (loc, part)::quasis in
          if is_tail
          then loc, List.rev quasis, List.rev expressions
          else template_parts env quasis expressions
      | _ ->
          (* Malformed template *)
          error_unexpected env;
          let imaginary_quasi = fst expr, { Expression.TemplateLiteral.Element.
            value = { Expression.TemplateLiteral.Element.
              raw = "";
              cooked = "";
            };
            tail = true;
          } in
          fst expr, List.rev (imaginary_quasi::quasis), List.rev expressions

    in fun env ((start_loc, {cooked; raw; _}, is_tail) as part) ->
      Expect.token env (T_TEMPLATE_PART part);
      let end_loc, quasis, expressions =
        let head = Ast.Expression.TemplateLiteral.(start_loc, {
          Element.value = { Element.cooked; raw; };
          tail = is_tail;
        }) in
        if is_tail
        then start_loc, [head], []
        else template_parts env [head] [] in
      let loc = Loc.btwn start_loc end_loc in
      loc, Expression.TemplateLiteral.({
        quasis;
        expressions;
      })

  and tagged_template env start_loc tag part =
    let quasi = template_literal env part in
    Loc.btwn start_loc (fst quasi), Expression.(TaggedTemplate TaggedTemplate.({
      tag;
      quasi;
    }))

  and group env =
    Expect.token env T_LPAREN;
    let expression = assignment env in
    let ret = (match Peek.token env with
    | T_COMMA -> sequence env [expression]
    | T_COLON ->
        let typeAnnotation = Type.annotation env in
        Expression.(Loc.btwn (fst expression) (fst typeAnnotation),
          TypeCast TypeCast.({
            expression;
            typeAnnotation;
          }))
    | _ -> expression) in
    Expect.token env T_RPAREN;
    ret

  and array_initializer =
    let rec elements env acc =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET -> List.rev acc
      | T_COMMA ->
          Expect.token env T_COMMA;
          elements env (None::acc)
      | T_ELLIPSIS ->
          let start_loc = Peek.loc env in
          Expect.token env T_ELLIPSIS;
          let argument = assignment env in
          let loc = Loc.btwn start_loc (fst argument) in
          let elem = Expression.(Spread (loc, SpreadElement.({
            argument;
          }))) in
          if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
          elements env ((Some elem)::acc)
      | _ ->
          let elem = Expression.Expression (assignment env) in
          if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
          elements env ((Some elem)::acc)

    in fun env ->
      let start_loc = Peek.loc env in
      Expect.token env T_LBRACKET;
      let elements = elements env [] in
      let end_loc = Peek.loc env in
      Expect.token env T_RBRACKET;
      Loc.btwn start_loc end_loc, Expression.Array.({
        elements;
      })

  and regexp env =
    Eat.push_lex_mode env Lex_mode.REGEXP;
    let loc = Peek.loc env in
    let raw, pattern, raw_flags = match Peek.token env with
      | T_REGEXP (_, pattern, flags) ->
          let raw = Peek.value env in
          Eat.token env;
          raw, pattern, flags
      | _ -> assert false in
    Eat.pop_lex_mode env;
    let filtered_flags = Buffer.create (String.length raw_flags) in
    String.iter (function
      | 'g' | 'i' | 'm' | 'u' | 'y' as c -> Buffer.add_char filtered_flags c
      | _ -> ()) raw_flags;
    let flags = Buffer.contents filtered_flags in
    if flags <> raw_flags
    then error env (Error.InvalidRegExpFlags raw_flags);
    let value = Literal.(RegExp { RegExp.pattern; flags; }) in
    loc, Expression.(Literal { Literal.value; raw; })

  and try_arrow_function =
    (* Certain errors (almost all errors) cause a rollback *)
    let error_callback _ = Error.(function
      (* Don't rollback on these errors. *)
      | StrictParamName
      | ParameterAfterRestParameter
      | NewlineBeforeArrow -> ()
      (* Everything else causes a rollback *)
      | _ -> raise Try.Rollback) in

    fun env ->
      let env = env |> with_error_callback error_callback in

      let start_loc = Peek.loc env in
      (* a T_ASYNC could either be a parameter name or it could be indicating
       * that it's an async function *)
      let async = Peek.token ~i:1 env <> T_ARROW && Declaration.async env in
      let typeParameters = Type.type_parameter_declaration env in
      let params, returnType, predicate =
        (* Disallow all fancy features for identifier => body *)
        if Peek.is_identifier env && typeParameters = None
        then
          let loc, name =
            Parse.identifier ~restricted_error:Error.StrictParamName env in
          let param = loc, Pattern.Identifier {
            Pattern.Identifier.name = loc, name;
                               typeAnnotation=None;
                               optional=false;
          } in
          ([param], None), None, None
        else
          let params = Declaration.function_params env in
          (* There's an ambiguity if you use a function type as the return
           * type for an arrow function. So we disallow anonymous function
           * types in arrow function return types unless the function type is
           * enclosed in parens *)
          let returnType, predicate = env
            |> with_no_anon_function_type true
            |> Type.annotation_and_predicate_opt in
          params, returnType, predicate in

      (* It's hard to tell if an invalid expression was intended to be an
       * arrow function before we see the =>. If there are no params, that
       * implies "()" which is only ever found in arrow params. Similarly,
       * rest params indicate arrow functions. Therefore, if we see a rest
       * param or an empty param list then we can disable the rollback and
       * instead generate errors as if we were parsing an arrow function *)
      let env = match params with
        | _, Some _
        | [], _ -> without_error_callback env
        | _ -> env
      in

      if Peek.is_line_terminator env && Peek.token env = T_ARROW
      then error env Error.NewlineBeforeArrow;
      Expect.token env T_ARROW;

      (* Now we know for sure this is an arrow function *)
      let env = without_error_callback env in

      let end_loc, (body, strict) = with_loc
        (Declaration.concise_function_body ~async ~generator:false)
        env
      in
      let simple = Declaration.is_simple_function_params params in
      Declaration.strict_post_check env ~strict ~simple None params;
      let expression = Function.(
        match body with
        | BodyBlock _ -> false
        | BodyExpression _ -> true) in
      let loc = Loc.btwn start_loc end_loc in
      loc, Expression.(ArrowFunction Function.({
        id = None;
        params;
        body;
        async;
        generator = false; (* arrow functions cannot be generators *)
        predicate;
        expression;
        returnType;
        typeParameters;
      }))

  and sequence env acc =
    match Peek.token env with
    | T_COMMA ->
        Expect.token env T_COMMA;
        let expr = assignment env in
        sequence env (expr::acc)
    | _ ->
      let (last_loc, _) = List.hd acc in
      let expressions = List.rev acc in
      let (first_loc, _) = List.hd expressions in
      Loc.btwn first_loc last_loc, Expression.(Sequence Sequence.({
        expressions;
      }))

  (* You can do things like
   * var x = { if : 4 }
   * x.if
   *)
  and identifier_or_reserved_keyword env =
    let lex_token = Peek.token env in
    let lex_value = Peek.value env in
    let lex_loc = Peek.loc env in
    match lex_token with
    (* Anything that is a special token in Flow but not in the ES6 spec
       should be here. *)
    | T_ASYNC
    | T_DECLARE
    | T_IDENTIFIER
    | T_OF
    | T_TYPE
      -> Parse.identifier env, None
    | _ ->
      let err = match lex_token with
      | T_FUNCTION
      | T_IF
      | T_IN
      | T_INSTANCEOF
      | T_RETURN
      | T_SWITCH
      | T_THIS
      | T_THROW
      | T_TRY
      | T_VAR
      | T_WHILE
      | T_WITH
      | T_CONST
      | T_LET
      | T_NULL
      | T_FALSE
      | T_TRUE
      | T_BREAK
      | T_CASE
      | T_CATCH
      | T_CONTINUE
      | T_DEFAULT
      | T_DO
      | T_FINALLY
      | T_FOR
      | T_CLASS
      | T_EXTENDS
      | T_STATIC
      | T_ELSE
      | T_NEW
      | T_DELETE
      | T_TYPEOF
      | T_VOID
      | T_ENUM
      | T_EXPORT
      | T_IMPORT
      | T_SUPER
      | T_IMPLEMENTS
      | T_INTERFACE
      | T_PACKAGE
      | T_PRIVATE
      | T_PROTECTED
      | T_PUBLIC
      | T_YIELD
      | T_ANY_TYPE
      | T_BOOLEAN_TYPE
      | T_NUMBER_TYPE
      | T_STRING_TYPE
      | T_VOID_TYPE
      | T_AWAIT
      | T_DEBUGGER ->
          Some (lex_loc, get_unexpected_error (lex_token, lex_value))
      | _ ->
          error_unexpected env;
          None
      in
      Eat.token env;
      (lex_loc, lex_value), err
end
