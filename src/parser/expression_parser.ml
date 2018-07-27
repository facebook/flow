(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Token
open Parser_env
open Ast
module Error = Parse_error
open Parser_common

module type EXPRESSION = sig
  val assignment: env -> (Loc.t, Loc.t) Expression.t
  val assignment_cover: env -> pattern_cover
  val conditional: env -> (Loc.t, Loc.t) Expression.t
  val property_name_include_private: env -> Loc.t * Loc.t Identifier.t * bool
  val is_assignable_lhs: (Loc.t, Loc.t) Expression.t -> bool
  val left_hand_side: env -> (Loc.t, Loc.t) Expression.t
  val number: env -> number_type -> string -> float
  val sequence: env -> (Loc.t, Loc.t) Expression.t list -> (Loc.t, Loc.t) Expression.t
end

module Expression
  (Parse: PARSER)
  (Type: Type_parser.TYPE)
  (Declaration: Declaration_parser.DECLARATION)
  (Pattern_cover: Pattern_cover.COVER)
: EXPRESSION = struct
  type op_precedence = Left_assoc of int | Right_assoc of int
  let is_tighter a b =
    let a_prec = match a with Left_assoc x -> x | Right_assoc x -> x - 1 in
    let b_prec = match b with Left_assoc x -> x | Right_assoc x -> x in
    a_prec >= b_prec

  let is_assignable_lhs = Expression.(function
    | _, MetaProperty { MetaProperty.meta = (_, "new"); property = (_, "target") }
      -> false (* #sec-static-semantics-static-semantics-isvalidsimpleassignmenttarget *)

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
    | _, JSXFragment _
    | _, Literal _
    | _, Logical _
    | _, New _
    | _, OptionalCall _
    | _, OptionalMember _
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

  let as_expression = Pattern_cover.as_expression
  let as_pattern = Pattern_cover.as_pattern

  (* AssignmentExpression :
   *   ConditionalExpression
   *   LeftHandSideExpression = AssignmentExpression
   *   LeftHandSideExpression AssignmentOperator AssignmentExpression
   *   ArrowFunctionFunction
   *
   *   Originally we were parsing this without backtracking, but
   *   ArrowFunctionExpression got too tricky. Oh well.
   *)
  let rec assignment_cover =
    let assignment_but_not_arrow_function_cover env =
      let expr_or_pattern = conditional_cover env in
      match assignment_op env with
      | Some operator ->
        let left = as_pattern env expr_or_pattern in
        let right = assignment env in
        let loc = Loc.btwn (fst left) (fst right) in

        Cover_expr (loc, Expression.(Assignment { Assignment.
          operator;
          left;
          right;
        }))
      | _ -> expr_or_pattern

    in let error_callback _ = function
      (* Don't rollback on these errors. *)
      | Error.StrictReservedWord -> ()
      (* Everything else causes a rollback *)
      | _ -> raise Try.Rollback

    (* So we may or may not be parsing the first part of an arrow function
     * (the part before the =>). We might end up parsing that whole thing or
     * we might end up parsing only part of it and thinking we're done. We
     * need to look at the next token to figure out if we really parsed an
     * assignment expression or if this is just the beginning of an arrow
     * function *)
    in let try_assignment_but_not_arrow_function env =
      let env = env |> with_error_callback error_callback in
      let ret = assignment_but_not_arrow_function_cover env in
      match Peek.token env with
      | T_ARROW -> (* x => 123 *)
        raise Try.Rollback
      | T_COLON when last_token env = Some T_RPAREN-> (* (x): number => 123 *)
        raise Try.Rollback
      (* async x => 123 -- and we've already parsed async as an identifier
       * expression *)
      | _ when Peek.is_identifier env -> begin match ret with
        | Cover_expr (_, Expression.Identifier (_, "async"))
            when not (Peek.is_line_terminator env) ->
          raise Try.Rollback
        | _ -> ret
        end
      | _ -> ret
    in fun env ->
      match Peek.token env, Peek.is_identifier env with
      | T_YIELD, _ when (allow_yield env) -> Cover_expr (yield env)
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
                assignment_but_not_arrow_function_cover env
          )
        )
      | _ -> assignment_but_not_arrow_function_cover env

  and assignment env =
    as_expression env (assignment_cover env)

  and yield env = with_loc (fun env ->
    if in_formal_parameters env then error env Error.YieldInFormalParameters;
    Expect.token env T_YIELD;
    let argument, delegate =
      if Peek.is_implicit_semicolon env then None, false
      else
        let delegate = Expect.maybe env T_MULT in
        let has_argument = match Peek.token env with
          | T_SEMICOLON
          | T_RBRACKET
          | T_RCURLY
          | T_RPAREN
          | T_COLON
          | T_COMMA -> false
          | _ -> true
        in
        let argument =
          if delegate || has_argument
          then Some (assignment env)
          else None in
        argument, delegate
    in
    Expression.(Yield Yield.({
      argument;
      delegate;
    }))
  ) env

  and is_lhs = Expression.(function
    | _, MetaProperty { MetaProperty.meta = (_, "new"); property = (_, "target") }
      -> false (* #sec-static-semantics-static-semantics-isvalidsimpleassignmenttarget *)

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
    | _, JSXFragment _
    | _, Literal _
    | _, Logical _
    | _, New _
    | _, Object _
    | _, OptionalCall _
    | _, OptionalMember _
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

  and conditional_cover env =
    let start_loc = Peek.loc env in
    let expr = logical_cover env in
    if Peek.token env = T_PLING
    then begin
      Expect.token env T_PLING;
      (* no_in is ignored for the consequent *)
      let env' = env |> with_no_in false in
      let consequent = assignment env' in
      Expect.token env T_COLON;
      let end_loc, alternate = with_loc assignment env in
      let loc = Loc.btwn start_loc end_loc in
      Cover_expr (loc, Expression.(Conditional { Conditional.
        test = as_expression env expr;
        consequent;
        alternate;
      }))
    end else expr

  and conditional env = as_expression env (conditional_cover env)

  and logical_cover =
    let open Expression in
    let make_logical env left right operator loc =
      let left = as_expression env left in
      let right = as_expression env right in
      Cover_expr (loc, Logical {Logical.operator; left; right;})
    in let rec logical_and env left lloc =
      match Peek.token env with
      | T_AND ->
          Expect.token env T_AND;
          let rloc, right = with_loc binary_cover env in
          let loc = Loc.btwn lloc rloc in
          logical_and env (make_logical env left right Logical.And loc) loc
      | _  -> lloc, left
    and logical_or env left lloc =
      let options = parse_options env in
      match Peek.token env with
      | T_OR ->
          Expect.token env T_OR;
          let rloc, right = with_loc binary_cover env in
          let rloc, right = logical_and env right rloc in
          let loc = Loc.btwn lloc rloc in
          logical_or env (make_logical env left right Logical.Or loc) loc
      | T_PLING_PLING ->
          if not options.esproposal_nullish_coalescing
          then error env Parse_error.NullishCoalescingDisabled;

          Expect.token env T_PLING_PLING;
          let rloc, right = with_loc binary_cover env in
          let rloc, right = logical_and env right rloc in
          let loc = Loc.btwn lloc rloc in
          logical_or env (make_logical env left right Logical.NullishCoalesce loc) loc
      | _ -> left
    in fun env ->
      let loc, left = with_loc binary_cover env in
      let loc, left = logical_and env left loc in
      logical_or env left loc

  and binary_cover =
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
      let right_loc, (is_unary, right) = with_loc (fun env ->
        let is_unary = peek_unary_op env <> None in
        let right = unary_cover (env |> with_no_in false) in
        is_unary, right
      ) env in
      if Peek.token env = T_LESS_THAN
      then begin
        match right with
        | Cover_expr (_, Expression.JSXElement _) ->
            error env Error.AdjacentJSXElements
        | _ -> ()
      end;
      match stack, binary_op env with
      | [], None ->
        right
      | _, None ->
        let right = as_expression env right in
        Cover_expr (collapse_stack right right_loc stack)
      | _, Some (rop, rpri) ->
        if is_unary && rop = Expression.Binary.Exp then
          error_at env (right_loc, Error.InvalidLHSInExponentiation);
        let right = as_expression env right in
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

  and unary_cover env =
    let begin_loc = Peek.loc env in
    let op = peek_unary_op env in
    match op with
    | None -> begin
        let op = Expression.Update.(match Peek.token env with
        | T_INCR -> Some Increment
        | T_DECR -> Some Decrement
        | _ -> None) in
        match op with
        | None -> postfix_cover env
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
            let loc = Loc.btwn begin_loc end_loc in
            Cover_expr (loc, Expression.(Update { Update.
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
      | Unary.Delete, (_, Member member) ->
          begin match member.Ast.Expression.Member.property with
          | Ast.Expression.Member.PropertyPrivateName _ ->
              error_at env (loc, Error.PrivateDelete)
          | _ -> () end
      | _ -> ());
      Cover_expr (loc, Expression.(Unary { Unary.
        operator;
        prefix = true;
        argument;
      }))

  and unary env = as_expression env (unary_cover env)

  and postfix_cover env =
    let argument = left_hand_side_cover env in
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
        let argument = as_expression env argument in
        if not (is_lhs argument)
        then error_at env (fst argument, Error.InvalidLHSInAssignment);
        (match argument with
        | _, Expression.Identifier (_, name)
          when is_restricted name ->
            strict_error env Error.StrictLHSPostfix
        | _ -> ());
        let end_loc = Peek.loc env in
        Eat.token env;
        let loc = Loc.btwn (fst argument) end_loc in
        Cover_expr (loc, Expression.(Update { Update.
          operator;
          prefix = false;
          argument;
        }))

  and left_hand_side_cover env =
    let start_loc = Peek.loc env in
    let allow_new = not (no_new env) in
    let env = with_no_new false env in
    let expr = match Peek.token env with
    | T_NEW when allow_new -> Cover_expr (new_expression env)
    | T_IMPORT -> Cover_expr (import env)
    | T_SUPER -> Cover_expr (super env)
    | _ when Peek.is_function env -> Cover_expr (_function env)
    | _ -> primary_cover env in
    call_cover env start_loc expr

  and left_hand_side env = as_expression env (left_hand_side_cover env)

  and super env =
    let allowed, call_allowed = match allow_super env with
    | No_super -> false, false
    | Super_prop -> true, false
    | Super_prop_or_call -> true, true
    in
    let loc = Peek.loc env in
    Expect.token env T_SUPER;
    let super = loc, Expression.Super in
    match Peek.token env with
    | T_PERIOD
    | T_LBRACKET ->
      let super =
        if not allowed then begin
          error_at env (loc, Parse_error.UnexpectedSuper);
          loc, Expression.Identifier (loc, "super")
        end else
          super
      in
      call ~allow_optional_chain:false env loc super
    | T_LPAREN ->
      let super =
        if not call_allowed then begin
          error_at env (loc, Parse_error.UnexpectedSuperCall);
          loc, Expression.Identifier (loc, "super")
        end else
          super
      in
      call ~allow_optional_chain:false env loc super
    | _ ->
      if not allowed
        then error_at env (loc, Parse_error.UnexpectedSuper)
        else error_unexpected env;
      super

  and import env = with_loc (fun env ->
    Expect.token env T_IMPORT;
    Expect.token env T_LPAREN;
    let arg = assignment (with_no_in false env) in
    Expect.token env T_RPAREN;
    Expression.Import arg
  ) env

  and call_cover ?(allow_optional_chain=true) ?(in_optional_chain=false) env start_loc left =
    let left = member_cover ~allow_optional_chain ~in_optional_chain env start_loc left in
    let optional = last_token env = Some T_PLING_PERIOD in
    let arguments ?targs env =
      let args_loc, arguments = arguments env in
      let loc = Loc.btwn start_loc args_loc in
      let call = { Expression.Call.
        callee = as_expression env left;
        targs;
        arguments;
      } in
      let call = if optional || in_optional_chain
        then Expression.(OptionalCall { OptionalCall.
          call;
          optional;
        })
        else Expression.Call call
      in
      call_cover ~allow_optional_chain ~in_optional_chain env start_loc
        (Cover_expr (loc, call))
    in
    if no_call env then left
    else match Peek.token env with
    | T_LPAREN -> arguments env
    | T_LESS_THAN when should_parse_types env ->
        (* If we are parsing types, then f<T>(e) is a function call with a
           type application. If we aren't, it's a nested binary expression. *)
        let error_callback _ _ = raise Try.Rollback in
        let env = env |> with_error_callback error_callback in
        (* Parameterized call syntax is ambiguous, so we fall back to
           standard parsing if it fails. *)
        Try.or_else env ~fallback:left (fun env ->
          let targs = Type.type_parameter_instantiation env in
          arguments ?targs env
        )
    | _ -> left

  and call ?(allow_optional_chain=true) env start_loc left =
    as_expression env (call_cover ~allow_optional_chain env start_loc (Cover_expr left))

  and new_expression env =
    let start_loc = Peek.loc env in
    Expect.token env T_NEW;

    if in_function env && Peek.token env = T_PERIOD then begin
      Expect.token env T_PERIOD;
      let meta = start_loc, "new" in
      match Peek.token env with
      | T_IDENTIFIER { raw = "target"; _ } ->
        let property = Parse.identifier env in
        let end_loc = fst property in
        Loc.btwn start_loc end_loc, Expression.(MetaProperty MetaProperty.({
          meta;
          property;
        }))
      | _ ->
        error_unexpected env;
        Eat.token env; (* skip unknown identifier *)
        start_loc, Expression.Identifier meta (* return `new` identifier *)
    end else
      let callee_loc = Peek.loc env in
      let expr = match Peek.token env with
      | T_NEW -> new_expression env
      | T_SUPER -> super (env |> with_no_call true)
      | _ when Peek.is_function env -> _function env
      | _ -> primary env in
      let callee = member ~allow_optional_chain:false (env |> with_no_call true) callee_loc expr in
      (* You can do something like
       *   new raw`42`
       *)
      let callee = match Peek.token env with
      | T_TEMPLATE_PART part -> tagged_template env callee_loc callee part
      | _ -> callee in
      let targs =
        (* If we are parsing types, then new C<T>(e) is a constructor with a
           type application. If we aren't, it's a nested binary expression. *)
        if should_parse_types env
        then
          (* Parameterized call syntax is ambiguous, so we fall back to
             standard parsing if it fails. *)
          let error_callback _ _ = raise Try.Rollback in
          let env = env |> with_error_callback error_callback in
          Try.or_else env ~fallback:None Type.type_parameter_instantiation
        else
          None
      in
      let end_loc, arguments = match Peek.token env, targs with
      | T_LPAREN, _ -> arguments env
      | _, Some (targs_loc, _) -> targs_loc, []
      | _ -> fst callee, [] in

      Loc.btwn start_loc end_loc, Expression.(New New.({
        callee;
        targs;
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

  and member_cover =
    let dynamic ?(allow_optional_chain=true) ?(in_optional_chain=false)
                ?(optional=false) env start_loc left =
      let expr = Parse.expression (env |> with_no_call false) in
      let last_loc = Peek.loc env in
      Expect.token env T_RBRACKET;
      let loc = Loc.btwn start_loc last_loc in
      let member = Expression.Member.({
        _object  = as_expression env left;
        property = PropertyExpression expr;
        computed = true;
      }) in
      let member = if in_optional_chain
        then Expression.(OptionalMember { OptionalMember.
          member;
          optional;
        })
        else Expression.Member member
      in
      call_cover ~allow_optional_chain ~in_optional_chain env start_loc
        (Cover_expr (loc, member))
    in
    let static ?(allow_optional_chain=true) ?(in_optional_chain=false)
               ?(optional=false) env start_loc left =
      let id_loc, id, is_private = property_name_include_private env in
      if is_private then add_used_private env (snd id) id_loc;
      let loc = Loc.btwn start_loc id_loc in
      let open Expression.Member in
      let property = if is_private then PropertyPrivateName (id_loc, id)
      else PropertyIdentifier id in
      (* super.PrivateName is a syntax error *)
      begin match left with
      | Cover_expr (_, Ast.Expression.Super) when is_private ->
          error_at env (loc, Error.SuperPrivate)
      | _ -> () end;
      let member = Expression.Member.({
        _object = as_expression env left;
        property;
        computed = false;
      }) in
      let member = if in_optional_chain
        then Expression.(OptionalMember { OptionalMember.
          member;
          optional;
        })
        else Expression.Member member
      in
      call_cover ~allow_optional_chain ~in_optional_chain env start_loc
        (Cover_expr (loc, member))
    in
    fun ?(allow_optional_chain=true) ?(in_optional_chain=false) env start_loc left ->
      let options = parse_options env in
      match Peek.token env with
      | T_PLING_PERIOD ->
          if not options.esproposal_optional_chaining
          then error env Parse_error.OptionalChainingDisabled;

          if not allow_optional_chain
          then error env Parse_error.OptionalChainNew;

          Expect.token env T_PLING_PERIOD;
          begin match Peek.token env with
          | T_TEMPLATE_PART _ ->
            error env Parse_error.OptionalChainTemplate;
            left
          | T_LPAREN -> left
          | T_LESS_THAN when should_parse_types env -> left
          | T_LBRACKET ->
            Expect.token env T_LBRACKET;
            dynamic ~allow_optional_chain ~in_optional_chain:true
                    ~optional:true env start_loc left
          | _ ->
            static ~allow_optional_chain ~in_optional_chain:true
                   ~optional:true env start_loc left
          end
      | T_LBRACKET ->
          Expect.token env T_LBRACKET;
          dynamic ~allow_optional_chain ~in_optional_chain env start_loc left
      | T_PERIOD ->
          Expect.token env T_PERIOD;
          static ~allow_optional_chain ~in_optional_chain env start_loc left
      | T_TEMPLATE_PART part ->
          if in_optional_chain
          then error env Parse_error.OptionalChainTemplate;

          let expr = tagged_template env start_loc (as_expression env left) part in
          call_cover ~allow_optional_chain:false env start_loc (Cover_expr expr)
      | _ -> left

  and member ?(allow_optional_chain=true) env start_loc left =
    as_expression env (member_cover ~allow_optional_chain env start_loc (Cover_expr left))

  and _function env =
    let start_loc = Peek.loc env in
    let async = Declaration.async env in
    Expect.token env T_FUNCTION;
    let generator = Declaration.generator env in
    let yield, await = match async, generator with
    | true, true -> true, true (* proposal-async-iteration/#prod-AsyncGeneratorExpression *)
    | true, false -> false, true (* #prod-AsyncFunctionExpression *)
    | false, true -> true, false (* #prod-GeneratorExpression *)
    | false, false -> false, false (* #prod-FunctionExpression *)
    in
    let id, tparams =
      if Peek.token env = T_LPAREN
      then None, None
      else begin
        let id = match Peek.token env with
          | T_LESS_THAN -> None
          | _ ->
            let env = env |> with_allow_await await |> with_allow_yield yield in
            Some (Parse.identifier ~restricted_error:Error.StrictFunctionName env) in
        id, Type.type_parameter_declaration env
      end in

    (* #sec-function-definitions-static-semantics-early-errors *)
    let env = env |> with_allow_super No_super in

    let params = Declaration.function_params ~await ~yield env in
    let return, predicate = Type.annotation_and_predicate_opt env in
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
      return;
      tparams;
    }))

  and number env kind raw =
    let value = match kind with
    | LEGACY_OCTAL ->
      strict_error env Error.StrictOctalLiteral;
      begin try Int64.to_float (Int64.of_string ("0o"^raw))
      with Failure _ -> failwith ("Invalid legacy octal "^raw)
      end
    | BINARY
    | OCTAL ->
      begin try Int64.to_float (Int64.of_string raw)
      with Failure _ -> failwith ("Invalid binary/octal "^raw)
      end
    | NORMAL ->
      begin try Lexer.FloatOfString.float_of_string raw
      with
      | _ when Sys.win32 ->
        error env Parse_error.WindowsFloatOfString;
        789.0
      | Failure _ ->
        failwith ("Invalid number "^raw)
      end
    in
    Expect.token env (T_NUMBER { kind; raw });
    value

  and primary_cover env =
    let loc = Peek.loc env in
    match Peek.token env with
    | T_THIS ->
        Expect.token env T_THIS;
        Cover_expr (loc, Expression.This)
    | T_NUMBER { kind; raw } ->
        let value = Literal.Number (number env kind raw) in
        Cover_expr (loc, Expression.(Literal { Literal.value; raw; }))
    | T_STRING (loc, value, raw, octal) ->
        if octal then strict_error env Error.StrictOctalLiteral;
        Expect.token env (T_STRING (loc, value, raw, octal));
        let value = Literal.String value in
        Cover_expr (loc, Expression.(Literal { Literal.value; raw; }))
    | (T_TRUE | T_FALSE) as token ->
        Expect.token env token;
        let truthy = token = T_TRUE in
        let raw = if truthy then "true" else "false" in
        let value = Literal.Boolean truthy in
        Cover_expr (loc, Expression.(Literal { Literal.value; raw; }))
    | T_NULL ->
        Expect.token env T_NULL;
        let raw = "null" in
        let value = Literal.Null in
        Cover_expr (loc, Expression.(Literal { Literal.value; raw; }))
    | T_LPAREN -> Cover_expr (group env)
    | T_LCURLY ->
        let loc, obj, errs = Parse.object_initializer env in
        Cover_patt ((loc, Expression.Object obj), errs)
    | T_LBRACKET ->
        let loc, arr, errs = array_initializer env in
        Cover_patt ((loc, Expression.Array arr), errs)
    | T_DIV
    | T_DIV_ASSIGN -> Cover_expr (regexp env)
    | T_LESS_THAN ->
        let loc, expression = match Parse.jsx_element_or_fragment env with
            | (loc, `Element e) -> (loc, Expression.JSXElement e)
            | (loc, `Fragment f) -> (loc, Expression.JSXFragment f) in
        Cover_expr (loc, expression)
    | T_TEMPLATE_PART part ->
        let loc, template = template_literal env part in
        Cover_expr (loc, Expression.TemplateLiteral template)
    | T_CLASS -> Cover_expr (Parse.class_expression env)
    | _ when Peek.is_identifier env ->
        let id = Parse.identifier env in
        Cover_expr (fst id, Expression.Identifier id)
    | t ->
        error_unexpected env;
        (* Let's get rid of the bad token *)
        begin match t with T_ERROR _ -> Eat.token env | _ -> () end;
        (* Really no idea how to recover from this. I suppose a null
         * expression is as good as anything *)
        let value = Literal.Null in
        let raw = "null" in
        Cover_expr (loc, Expression.(Literal { Literal.value; raw; }))

  and primary env = as_expression env (primary_cover env)

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
        let annot = Type.annotation env in
        Expression.(Loc.btwn (fst expression) (fst annot),
          TypeCast TypeCast.({
            expression;
            annot;
          }))
    | _ -> expression) in
    Expect.token env T_RPAREN;
    ret

  and array_initializer =
    let rec elements env (acc, errs) =
      match Peek.token env with
      | T_EOF
      | T_RBRACKET -> List.rev acc, Pattern_cover.rev_errors errs
      | T_COMMA ->
          Expect.token env T_COMMA;
          elements env (None::acc, errs)
      | T_ELLIPSIS ->
          let loc, (argument, new_errs) = with_loc (fun env ->
            Expect.token env T_ELLIPSIS;
            match assignment_cover env with
            | Cover_expr argument -> argument, Pattern_cover.empty_errors
            | Cover_patt (argument, new_errs) -> argument, new_errs
          ) env in
          let elem = Expression.(Spread (loc, SpreadElement.({
            argument;
          }))) in
          let is_last = Peek.token env = T_RBRACKET in

          (* if this array is interpreted as a pattern, the spread becomes an AssignmentRestElement
             which must be the last element. We can easily error about additional elements since
             they will be in the element list, but a trailing elision, like `[...x,]`, is not part
             of the AST. so, keep track of the error so we can raise it if this is a pattern. *)
          let new_errs =
            if not is_last && Peek.ith_token ~i:1 env = T_RBRACKET then
              let if_patt = (loc, Parse_error.ElementAfterRestElement)::new_errs.if_patt in
              { new_errs with if_patt }
            else new_errs
          in

          if not is_last then Expect.token env T_COMMA;
          let acc = Some elem :: acc in
          let errs = Pattern_cover.rev_append_errors new_errs errs in
          elements env (acc, errs)
      | _ ->
          let elem, new_errs = match assignment_cover env with
            | Cover_expr elem -> elem, Pattern_cover.empty_errors
            | Cover_patt (elem, new_errs) -> elem, new_errs
          in
          if Peek.token env <> T_RBRACKET then Expect.token env T_COMMA;
          let acc = Some (Expression.Expression elem) :: acc in
          let errs = Pattern_cover.rev_append_errors new_errs errs in
          elements env (acc, errs)

    in fun env ->
      let loc, (elements, errs) = with_loc (fun env ->
        Expect.token env T_LBRACKET;
        let res = elements env ([], Pattern_cover.empty_errors) in
        Expect.token env T_RBRACKET;
        res
      ) env in
      loc, { Expression.Array.elements; }, errs

  and regexp env =
    Eat.push_lex_mode env Lex_mode.REGEXP;
    let loc = Peek.loc env in
    let raw, pattern, raw_flags = match Peek.token env with
      | T_REGEXP (_, pattern, flags) ->
          Eat.token env;
          let raw = "/" ^ pattern ^ "/" ^ flags in
          raw, pattern, flags
      | _ -> assert false in
    Eat.pop_lex_mode env;
    let filtered_flags = Buffer.create (String.length raw_flags) in
    String.iter (function
      | 'g' | 'i' | 'm' | 's' | 'u' | 'y' as c -> Buffer.add_char filtered_flags c
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
      | StrictReservedWord
      | ParameterAfterRestParameter
      | NewlineBeforeArrow
      | YieldInFormalParameters -> ()
      (* Everything else causes a rollback *)
      | _ -> raise Try.Rollback) in

    fun env ->
      let env = env |> with_error_callback error_callback in

      let start_loc = Peek.loc env in
      (* a T_ASYNC could either be a parameter name or it could be indicating
       * that it's an async function *)
      let async = Peek.ith_token ~i:1 env <> T_ARROW && Declaration.async env in
      let tparams = Type.type_parameter_declaration env in
      let params, return, predicate =
        (* Disallow all fancy features for identifier => body *)
        if Peek.is_identifier env && tparams = None
        then
          let loc, name =
            Parse.identifier ~restricted_error:Error.StrictParamName env in
          let param = loc, Pattern.Identifier {
            Pattern.Identifier.name = loc, name;
                               annot=None;
                               optional=false;
          } in
          (loc, { Ast.Function.Params.params = [param]; rest = None }), None, None
        else
          let params =
            let yield = allow_yield env in
            let await = allow_await env in
            Declaration.function_params ~await ~yield env
          in
          (* There's an ambiguity if you use a function type as the return
           * type for an arrow function. So we disallow anonymous function
           * types in arrow function return types unless the function type is
           * enclosed in parens *)
          let return, predicate = env
            |> with_no_anon_function_type true
            |> Type.annotation_and_predicate_opt in
          params, return, predicate in

      (* It's hard to tell if an invalid expression was intended to be an
       * arrow function before we see the =>. If there are no params, that
       * implies "()" which is only ever found in arrow params. Similarly,
       * rest params indicate arrow functions. Therefore, if we see a rest
       * param or an empty param list then we can disable the rollback and
       * instead generate errors as if we were parsing an arrow function *)
      let env = match params with
        | _, { Ast.Function.Params.rest = Some _; _ }
        | _, { Ast.Function.Params.params = []; _ } -> without_error_callback env
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
      Cover_expr (loc, Expression.(ArrowFunction { Function.
        id = None;
        params;
        body;
        async;
        generator = false; (* arrow functions cannot be generators *)
        predicate;
        expression;
        return;
        tparams;
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

  and property_name_include_private env =
    let start_loc = Peek.loc env in
    let is_private = Expect.maybe env T_POUND in
    let id_loc, ident = identifier_name env in
    let loc = Loc.btwn start_loc id_loc in
    loc, (id_loc, ident), is_private
end
