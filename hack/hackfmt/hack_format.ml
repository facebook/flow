(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SyntaxKind = Full_fidelity_syntax_kind
module Syntax = Full_fidelity_editable_syntax
module TriviaKind = Full_fidelity_trivia_kind
module Trivia = Full_fidelity_editable_trivia
open Syntax
open Core

type open_span = {
  open_span_start: int;
  open_span_cost: int;
}

let open_span start cost = {
  open_span_start = start;
  open_span_cost = cost;
}

let builder = object (this)
  val open_spans = Stack.create ();
  val mutable rules = [];

  val mutable chunks = [];

  val mutable next_split_rule = None;
  val mutable space_if_not_split = false;

  val mutable pending_whitespace = "";

  val mutable chunk_groups = [];
  val mutable rule_alloc = Rule_allocator.make ();
  val mutable nesting_alloc = Nesting_allocator.make ();
  val mutable block_indent = 0;

  method add_string s =
    chunks <- (match chunks with
      | hd :: tl when hd.Chunk.is_appendable ->
        let text = hd.Chunk.text ^ pending_whitespace ^ s in
        {hd with Chunk.text = text} :: tl
      | _ -> begin
          let nesting = nesting_alloc.Nesting_allocator.current_nesting in
          match next_split_rule with
            | None -> Chunk.make s (List.hd rules) nesting :: chunks
            | Some rule_type ->
              this#start_rule ~rule_type:rule_type ();
              let cs = Chunk.make s (List.hd rules) nesting :: chunks in
              this#end_rule ();
              next_split_rule <- None;
              cs
      end
    );
    pending_whitespace <- ""

  method add_pending_whitespace s =
    pending_whitespace <- pending_whitespace ^ s

  method split space =
    chunks <- (match chunks with
      | hd :: tl when hd.Chunk.is_appendable ->
        let rule_id = hd.Chunk.rule in
        let rule = match List.hd rules with
          | None when rule_id = Rule.null_rule_id -> this#add_rule Rule.Simple
          | Some r when rule_id = Rule.null_rule_id -> r
          | _ -> rule_id
        in
        let chunk = (Chunk.finalize hd rule space_if_not_split) in
        space_if_not_split <- space;
        chunk :: tl
      | _ -> chunks
    )

  method add_rule rule_kind =
    let ra, rule = Rule_allocator.make_rule rule_alloc rule_kind in
    rule_alloc <- ra;
    rule.Rule.id

  method simple_space_split () =
    this#split true;
    next_split_rule <- Some Rule.Simple;
    ()

  method simple_split () =
    this#split false;
    next_split_rule <- Some Rule.Simple;
    ()

  method hard_split () =
    this#split false;
    next_split_rule <- Some Rule.Always;
    ()

  method nest ?amount:(amount=2) () =
    nesting_alloc <- Nesting_allocator.nest nesting_alloc amount;
    ()

  method unnest () =
    nesting_alloc <- Nesting_allocator.unnest nesting_alloc;
    ()

  method start_rule ?(rule_type=Rule.Simple) () =
    (* Override next_split_rule unless it's an Always rule *)
    next_split_rule <- (match next_split_rule with
      | Some kind when kind <> Rule.Always -> None
      | _ -> next_split_rule
    );
    let rule = this#add_rule rule_type in
    rule_alloc <- Rule_allocator.mark_dependencies rule_alloc rules rule;
    rules <- rule :: rules

  method end_rule () =
    rules <- match rules with
      | hd :: tl -> tl
      | [] -> [] (*TODO: error *)

  method has_rule_kind kind =
    List.exists rules ~f:(fun id ->
      (Rule_allocator.get_rule_kind rule_alloc id) = kind
    )

  method start_span () =
    let os = open_span (List.length chunks) 1 in
    Stack.push os open_spans

  method end_span () =
    let os = Stack.pop open_spans in
    let span = Span.make os.open_span_cost in
    let r_chunks = List.rev chunks in
    chunks <- List.rev_mapi r_chunks ~f:(fun n x ->
      if n <= os.open_span_start then
        x
      else
        (* TODO: handle required hard splits *)
        {x with Chunk.spans = span :: x.Chunk.spans}
    );
    ()


  (*
    TODO: find a better way to represt the rules empty case
    for end chunks and block nesting
  *)
  method start_block_nest () =
    if List.is_empty rules then
      block_indent <- block_indent + 2
    else
      this#nest ()
  method end_block_nest () =
    if List.is_empty rules then
      block_indent <- block_indent - 2
    else
      this#unnest ()

  method end_chunks () =
    this#hard_split ();
    if List.is_empty rules then begin
      chunk_groups <- {
        Chunk_group.chunks = (List.rev chunks);
        rule_map = rule_alloc.Rule_allocator.rule_map;
        rule_dependency_map = rule_alloc.Rule_allocator.dependency_map;
        block_indentation = block_indent;
      } :: chunk_groups;
      chunks <- [];
      rule_alloc <- Rule_allocator.make ();
      nesting_alloc <- Nesting_allocator.make ();
      ()
    end;
    ()

  method _end () =
    (*TODO: warn if not empty? *)
    if not (List.is_empty chunks) then this#end_chunks ();
    List.rev chunk_groups

  method __debug chunks =
    let d_chunks = chunks in
    List.iter d_chunks ~f:(fun c ->
      Printf.printf
        "Span count:%d\t Rule:%s\t Text:%s\n"
        (List.length c.Chunk.spans)
        "Todo" (*TODO: refactor (Rule.to_string c.Chunk.rule) *)
        c.Chunk.text
    )

end

let split ?space:(space=false) () =
  builder#split space

let handle_trivia trivia_list =
  ()
  (*
  List.iter trivia_list ~f:(fun t ->
    match Trivia.kind t with
      | TriviaKind.SingleLineComment ->
        split ();
        builder#start_rule ();
        builder#add_string (Trivia.text t);
        builder#end_rule ();
        builder#hard_split ()
      | TriviaKind.DelimitedComment ->
        split ();
        builder#start_rule ();
        builder#add_string (Trivia.text t);
        builder#end_rule ();
        split ()
      | _ -> ()
  )
  *)

let token x =
  handle_trivia (EditableToken.leading x);
  builder#add_string (EditableToken.text x);
  handle_trivia (EditableToken.trailing x);
  ()

let add_space () =
  builder#add_string " "

let pending_space () =
  builder#add_pending_whitespace " "

let rec transform node =
  let t = transform in
  let span = true in
  let nest = true in
  let space = true in

  let () = match syntax node with
  | Missing -> ()
  | Token x ->
    token x;
    ()
  | Script x ->
    (* TODO script_header*)
    handle_possible_list  x.script_declarations;
  | SimpleTypeSpecifier {simple_type_specifier} -> t simple_type_specifier
  | LiteralExpression x -> t x.literal_expression
  | QualifiedNameExpression x ->
    t x.qualified_name_expression
  | VariableExpression x -> t x.variable_expression
  | PipeVariableExpression x -> t x.pipe_variable_expression
  | PropertyDeclaration x ->
    let (modifiers, prop_type, declarators, semi) =
      get_property_declaration_children x in
    handle_possible_list ~after_each:(fun _ -> add_space ()) modifiers;
    t prop_type;
    tl_with ~nest ~rule:(Some Rule.Argument) ~f:(fun () ->
      handle_possible_list ~before_each:(split ~space) declarators;
    ) ();
    t semi;
    builder#end_chunks ();
    ()
  | PropertyDeclarator x ->
    let (name, prop_initializer) = get_property_declarator_children x in
    t name;
    t prop_initializer;
    ()
  | FunctionDeclaration x ->
    let (attr, header, body) = get_function_declaration_children x in
    t attr;
    if not (is_missing attr) then builder#end_chunks ();
    t header;
    handle_possible_compound_statement body;
    builder#end_chunks ();
    ()
  | FunctionDeclarationHeader x ->
    transform_function_declaration_header ~span_started:false x;
    ()
  | MethodishDeclaration x ->
    let (attr, modifiers, func_decl, body, semi) =
      get_methodish_declaration_children x
    in
    t attr;
    if not (is_missing attr) then builder#end_chunks ();
    builder#start_span ();
    handle_possible_list ~after_each:(fun _ -> add_space ()) modifiers;
    (match syntax func_decl with
      | FunctionDeclarationHeader x ->
        transform_function_declaration_header ~span_started:true x
      | _ ->
        raise (Failure
          "invalid parse tree provided, expecting a function declaration header"
        )
    );
    if not (is_missing body) then handle_possible_compound_statement body;
    t semi;
    builder#end_chunks ();
    ()
  | ClassishDeclaration x ->
    let (attr, modifiers, kw, name, type_params, extends_kw, extends,
      impl_kw, impls, body) = get_classish_declaration_children x
    in
    t attr;
    if not (is_missing attr) then builder#end_chunks ();
    tl_with ~span ~f:(fun () ->
      handle_possible_list ~after_each:(fun _ -> add_space ()) modifiers;
      t kw;
      split ~space ();
      t_with ~nest name;
    ) ();

    if not (is_missing extends_kw) then begin
      split ~space ();
      tl_with ~nest ~f:(fun () ->
        t extends_kw;
        tl_with ~nest ~f:(fun () ->
          handle_possible_list ~before_each:(split ~space) extends
        ) ();
      ) ();
      ()
    end;

    if not (is_missing impl_kw) then begin
      split ~space ();
      tl_with ~nest ~f:(fun () ->
        t impl_kw;
        tl_with ~nest ~f:(fun () ->
          handle_possible_list ~before_each:(split ~space) impls
        ) ();
      ) ();
      ()
    end;
    t body;
    ()
  | ClassishBody x ->
    let (leftb, body, rightb) = get_classish_body_children x in
    add_space ();
    t leftb;
    builder#end_chunks ();
    tl_with ~nest ~f:(fun () ->
      handle_possible_list body
    ) ();
    t rightb;
    builder#end_chunks ();
    ()
  | TraitUse x ->
    let (kw, elements, semi) = get_trait_use_children x in
    t kw;
    tl_with ~nest ~rule:(Some Rule.Argument) ~f:(fun () ->
      handle_possible_list ~before_each:(split ~space) elements;
    ) ();
    t semi;
    builder#end_chunks();
    ()
  | RequireClause x ->
    let (kw, kind, name, semi) = get_require_clause_children x in
    t kw;
    add_space ();
    t kind;
    split ~space ();
    t name;
    t semi;
    builder#end_chunks ();
    ()
  | ConstDeclaration x ->
    let (abstr, kw, const_type, declarators, semi) =
      get_const_declaration_children x in
    t abstr;
    if not (is_missing abstr) then add_space ();
    t kw;
    if not (is_missing const_type) then add_space ();
    t const_type;
    tl_with ~nest ~rule:(Some Rule.Argument) ~f:(fun () ->
      handle_possible_list ~before_each:(split ~space) declarators;
    ) ();
    t semi;
    builder#end_chunks ();
    ()
  | ConstantDeclarator x ->
    let (name, const_initializer) = get_constant_declarator_children x in
    t name;
    t const_initializer;
    ()
  | TypeConstDeclaration x ->
    let (abs, kw, type_kw, name, type_constraint, eq, type_spec, semi) =
      get_type_const_declaration_children x in
    t abs;
    if not (is_missing abs) then add_space ();
    t kw;
    add_space ();
    t type_kw;
    add_space ();
    t name;
    builder#simple_space_split ();
    tl_with ~nest ~f:(fun () ->
      t type_constraint;
      if not (is_missing type_constraint) then add_space ();
      t eq;
      builder#simple_space_split ();
      t type_spec;
      t semi;
      builder#end_chunks ();
    ) ();
    ()
  | DecoratedExpression x ->
    let (decorator, expr) = get_decorated_expression_children x in
    t decorator;
    t expr;
    ()
  | ParameterDeclaration x ->
    let (attr, visibility, param_type, name, default) =
      get_parameter_declaration_children x
    in
    t attr;
    t visibility;
    t param_type;
    if not (is_missing param_type) then add_space ();
    (* TODO: span and split, figure out attr and vis rules *)
    t name;
    t default;
  | AttributeSpecification x ->
    let (left_da, attrs, right_da) = get_attribute_specification_children x in
    transform_argish left_da attrs right_da;
    ()
  | Attribute x ->
    let (name, left_p, values, right_p) = get_attribute_children x in
    t name;
    transform_argish left_p values right_p;
    ()
  | ExpressionStatement x ->
    t x.expression_statement_expression;
    t x.expression_statement_semicolon;
    builder#end_chunks()
  | WhileStatement x ->
    t x.while_keyword;
    add_space ();
    t x.while_left_paren;
    split ();
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      t_with ~nest x.while_condition;
      split ();
      t x.while_right_paren;
    ) ();
    handle_possible_compound_statement x.while_body;
    builder#end_chunks ();
    ()
  | IfStatement x ->
    let (kw, left_p, condition, right_p, if_body, elseif_clauses, else_clause) =
      get_if_statement_children x in
    t kw;
    add_space ();
    t left_p;
    split ();
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      t_with ~nest condition;
      split ();
      t right_p;
    ) ();
    handle_possible_compound_statement if_body;
    handle_possible_list elseif_clauses;
    t else_clause;
    builder#end_chunks ();
    ()
  | ElseifClause x ->
    t x.elseif_keyword;
    add_space ();
    t x.elseif_left_paren;
    split ();
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      t_with ~nest x.elseif_condition;
      split ();
      t x.elseif_right_paren;
    ) ();
    handle_possible_compound_statement x.elseif_statement;
    ()
  | ElseClause x ->
    t x.else_keyword;
    handle_possible_compound_statement x.else_statement;
    ()
  | TryStatement x ->
    (* TODO: revisit *)
    let (kw, body, catch_clauses, finally_clause) =
      get_try_statement_children x in
    t kw;
    handle_possible_compound_statement body;
    handle_possible_list catch_clauses;
    t finally_clause;
  | CatchClause x ->
    let (kw, left_p, ex_type, var, right_p, body) =
      get_catch_clause_children x in
    (* TODO refactor for if statement consistentacy *)
    t kw;
    add_space ();
    t left_p;
    split ();
    tl_with ~nest ~f:(fun () ->
      t ex_type;
      add_space ();
      t var;
      split ();
    ) ();
    t right_p;
    handle_possible_compound_statement body;
    ();
  | ForStatement x ->
    let (kw, left_p, init, semi1, control, semi2, after_iter, right_p, body) =
      get_for_statement_children x in
    t kw;
    add_space ();
    t left_p;
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      split ();
      tl_with ~nest ~f:(fun () ->
        t init;
        t semi1;
        split ~space ();
        t control;
        t semi2;
        split ~space ();
        t after_iter;
      ) ();
      split ();
      t right_p;
    ) ();
    handle_possible_compound_statement body;
    ()
  | ForeachStatement x ->
    (* TODO: revisit this *)
    let (kw, left_p, collection, await_kw, as_kw, key, arrow, value, right_p,
      body) = get_foreach_statement_children x in
    t kw;
    add_space ();
    t left_p;
    t collection;
    if not (is_missing await_kw) then add_space ();
    t await_kw;
    add_space ();
    t as_kw;
    add_space ();
    t key;
    add_space ();
    t arrow;
    split ~space ();
    t value;
    split ();
    t right_p;
    handle_possible_compound_statement body;
    ()
  | SwitchStatement x ->
    let (kw, left_p, expr, right_p, body) = get_switch_statement_children x in
    t kw;
    add_space ();
    t left_p;
    split ();
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      tl_with ~nest ~f:(fun () -> t expr) ();
      t right_p;
    ) ();
    let () = match syntax body with
      | CompoundStatement cs -> handle_switch_body cs
      | _ -> raise (Failure "Switch body must be a compound statement")
    in
    builder#end_chunks ();
    ()
  | CaseStatement x ->
    raise (Failure "CaseStatement should be handled by handle_switch_body")
  | DefaultStatement x ->
    raise (Failure "DefaultStatement should be handled by handle_switch_body")
  | ReturnStatement x ->
    let (kw, expr, semi) = get_return_statement_children x in
    transform_keyword_expression_statement kw expr semi;
    ()
  | ThrowStatement x ->
    let (kw, expr, semi) = get_throw_statement_children x in
    transform_keyword_expression_statement kw expr semi;
    ()
  | BreakStatement x ->
    let (kw, expr, semi) = get_break_statement_children x in
    (* TODO: revisit *)
    transform_keyword_expression_statement kw expr semi;
    ()
  | FunctionStaticStatement x ->
    let (static_kw, declarators, semi) =
      get_function_static_statement_children x in
    t static_kw;
    tl_with ~nest ~rule:(Some Rule.Argument) ~f:(fun () ->
      handle_possible_list ~before_each:(split ~space) declarators;
    ) ();
    t semi;
    builder#end_chunks ();
    ()
  | StaticDeclarator x ->
    let (name, static_initializer) = get_static_declarator_children x in
    t name;
    t static_initializer;
  | SimpleInitializer x ->
    let (eq_kw, value) = get_simple_initializer_children x in
    add_space ();
    t eq_kw;
    builder#simple_space_split ();
    t_with ~nest value;
  | AnonymousFunction x ->
    let (async_kw, fun_kw, lp, params, rp, colon, ret_type, use, body) =
      get_anonymous_function_children x in
    t async_kw;
    if not (is_missing async_kw) then add_space ();
    t fun_kw;
    transform_argish_with_return_type ~in_span:false lp params rp colon
      ret_type;
    t use;
    handle_possible_compound_statement body;
    builder#end_chunks ();
    ()
  | AnonymousFunctionUseClause x ->
    (* TODO: Revisit *)
    let (kw, left_p, vars, right_p) =
      get_anonymous_function_use_clause_children x in
    add_space ();
    t kw;
    add_space ();
    transform_argish left_p vars right_p;
    ()
  | LambdaExpression x ->
    let (async, signature, arrow, body) = get_lambda_expression_children x in
    t async;
    if not (is_missing async) then add_space ();
    t signature;
    add_space ();
    t arrow;
    handle_lambda_body body;
    ()
  | LambdaSignature x ->
    let (lp, params, rp, colon, ret_type) = get_lambda_signature_children x in
    transform_argish_with_return_type ~in_span:false lp params rp colon
      ret_type;
    ()
  | CastExpression x ->
    let (lp, cast_type, rp, op) = get_cast_expression_children x in
    tl_with ~span ~f:(fun () ->
      t lp;
      t cast_type;
      t rp;
      builder#simple_space_split ();
      t_with ~nest op;
    ) ();
    ()
  | ScopeResolutionExpression x ->
    let (qual, operator, name) = get_scope_resolution_expression_children x in
    t qual;
    t operator;
    t name;
    ()
  | MemberSelectionExpression x ->
    let (obj, op, name) = get_member_selection_expression_children x in
    t obj;
    builder#simple_split ();
    t op;
    t name;
    ()
  | PrefixUnaryExpression x ->
    let (operator, operand) = get_prefix_unary_expression_children x in
    t operator;
    (* TODO: remove space for some unary expressions *)
    add_space ();
    t operand;
  | PostfixUnaryExpression x ->
    let (operand, operator) = get_postfix_unary_expression_children x in
    t operand;
    t operator;
  | BinaryExpression x ->
    transform_binary_expression ~is_nested:false x
  | InstanceofExpression x ->
    let (left, kw, right) = get_instanceof_expression_children x in
    t left;
    add_space ();
    t kw;
    builder#simple_space_split ();
    t_with ~nest right;
    ()
  | ConditionalExpression x ->
    let (test_expr, q_kw, true_expr, c_kw, false_expr) =
      get_conditional_expression_children x in
    t test_expr;
    tl_with ~nest ~rule:(Some Rule.Argument) ~f:(fun () ->
      builder#simple_space_split ();
      t q_kw;
      if not (is_missing true_expr) then begin
        add_space ();
        t true_expr;
        builder#simple_space_split ();
      end;
      t c_kw;
      add_space ();
      t false_expr;
    ) ();
    ()
  | FunctionCallExpression x ->
    handle_function_call_expression x
  | CollectionLiteralExpression x ->
    let (name, left_b, initializers, right_b) =
      get_collection_literal_expression_children x
    in
    t name;
    add_space ();
    t left_b;
    if is_missing initializers then begin
      t right_b;
      ()
    end else begin
      split ~space ();
      tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
        tl_with ~nest ~f:(fun () ->
          handle_possible_list ~after_each:after_each_literal initializers
        ) ();
        t right_b;
      ) ();
      ()
    end
  | ObjectCreationExpression x ->
    let (kw, obj_type, left_p, arg_list, right_p) =
      get_object_creation_expression_children x
    in
    t kw;
    add_space ();
    t obj_type;
    transform_argish left_p arg_list right_p;
    ()
  | ArrayIntrinsicExpression x ->
    let (kw, left_p, members, right_p) =
      get_array_intrinsic_expression_children x
    in
    t kw;
    transform_argish left_p members right_p;
    ()
  | ParenthesizedExpression x ->
    let (left_p, expr, right_p) = get_parenthesized_expression_children x in
    t left_p;
    split ();
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      t_with ~nest expr;
      split ();
      t right_p
    ) ();
    ()
  | BracedExpression x ->
    (* TODO: revisit this *)
    let (left_b, expr, right_b) = get_braced_expression_children x in
    t left_b;
    split ();
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      t_with ~nest expr;
      split ();
      t right_b
    ) ();
    ()
  | ListExpression x ->
    let (kw, lp, members, rp) = get_list_expression_children x in
    t kw;
    transform_argish lp members rp;
    ()
  | ElementInitializer x ->
    let (key, arrow, value) = get_element_initializer_children x in
    t key;
    add_space ();
    t arrow;
    builder#simple_space_split ();
    t_with ~nest value;
  | SubscriptExpression x ->
    (* TODO: revisit this *)
    let (receiver, lb, expr, rb) = get_subscript_expression_children x in
    t receiver;
    transform_argish lb expr rb;
    ()
  | XHPOpen x ->
    let (name, attrs, right_a) = get_xhp_open_children x in
    t name;
    if not (is_missing attrs) then begin
      split ~space ();
      tl_with ~nest ~rule:(Some Rule.Argument) ~f:(fun () ->
        handle_possible_list ~after_each:(fun is_last ->
          if not is_last then split ~space (); ()
        ) attrs;
      ) ();
    end;
    handle_xhp_open_right_angle_token right_a;
    ()
  | XHPAttribute x ->
    let (name, eq, expr) = get_xhp_attribute_children x in
    tl_with ~span ~f:(fun () ->
      t name;
      t eq;
      split ();
      t_with ~nest expr
    ) ();
    ()
  | XHPExpression x ->
    let (op, body, close) = get_xhp_expression_children x in

    let handle_body_close = (fun () ->
      tl_with ~nest ~f:(fun () ->
        handle_possible_list ~before_each:(fun _ -> split ()) body
      ) ();
      if not (is_missing close) then split ();
      t close;
      ()
    ) in

    if builder#has_rule_kind Rule.XHPExpression then begin
      t op;
      tl_with ~rule:(Some Rule.XHPExpression) ~f:(handle_body_close) ();
    end else begin
      tl_with ~rule:(Some Rule.XHPExpression) ~f:(fun () ->
        t op;
        handle_body_close ();
      ) ();
    end;
    ()
  | XHPClose x ->
    let (left_a, name, right_a) = get_xhp_close_children x in
    t left_a;
    t name;
    t right_a;
  | VectorTypeSpecifier x ->
    let (kw, left_a, vec_type, right_a) =
      get_vector_type_specifier_children x in
    t kw;
    transform_argish left_a vec_type right_a;
    ()
  | MapTypeSpecifier x ->
    let (kw, la, key, comma_kw, v, ra) = get_map_type_specifier_children x in
    t kw;
    let key_list_item = make_list_item key comma_kw in
    let val_list_item = make_list_item v (make_missing ()) in
    let args = make_list [key_list_item; val_list_item] in
    transform_argish la args ra;
    ()
  | GenericTypeSpecifier x ->
    let (class_type, type_args) = get_generic_type_specifier_children x in
    t class_type;
    t type_args;
    ()
  | NullableTypeSpecifier x ->
    let (question, ntype) = get_nullable_type_specifier_children x in
    t question;
    t ntype;
    ()
  | SoftTypeSpecifier x ->
    let (at, stype) = get_soft_type_specifier_children x in
    t at;
    t stype;
    ()
  | TypeArguments x ->
    let (left_a, type_list, right_a) = get_type_arguments_children x in
    t left_a;
    split ();
    tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
      tl_with ~nest ~f:(fun () ->
        handle_possible_list ~after_each:after_each_argument type_list
      ) ();
      t right_a;
    ) ();
    ()
  | ListItem x ->
    t x.list_item;
    t x.list_separator
  | _ ->
    Printf.printf "%s not supported - exiting \n"
      (SyntaxKind.to_string (kind node));
    exit 1
  in
  ()

and tl_with ?(nest=false) ?(rule=None) ?(span=false) ~f () =
  Option.iter rule ~f:(fun rule_type -> builder#start_rule ~rule_type ());
  if nest then builder#nest ();
  if span then builder#start_span ();

  f ();

  if nest then builder#unnest ();
  if span then builder#end_span ();
  Option.iter rule (fun _ -> builder#end_rule ());
  ()

and t_with ?(nest=false) ?(rule=None) ?(span=false) ?(f=transform) node =
  tl_with ~nest ~rule ~span ~f:(fun () -> f node) ();
  ()

and after_each_argument is_last =
  split ~space:(not is_last) ();

and after_each_literal is_last =
  split ~space:true ();

and handle_lambda_body node =
  match syntax node with
    | CompoundStatement x ->
      handle_compound_statement x;
    | _ ->
      split ~space:true ();
      tl_with ~rule:(Some Rule.Simple) ~nest:true ~f:(fun () ->
        transform node;
      ) ();
      ()

and handle_possible_compound_statement node =
  match syntax node with
    | CompoundStatement x ->
      handle_compound_statement x;
      pending_space ();
      ()
    | _ ->
      builder#end_chunks ();
      builder#start_block_nest ();
      t_with node;
      builder#end_block_nest ();
      ()

and handle_compound_statement cs =
  let (left_b, statements, right_b) = get_compound_statement_children cs in
  add_space ();
  transform left_b;
  builder#end_chunks ();
  builder#start_block_nest ();
  tl_with ~f:(fun () ->
    handle_possible_list statements;
  ) ();
  builder#end_block_nest ();
  transform right_b;
  ()

and handle_possible_list
    ?(before_each=(fun () -> ())) ?(after_each=(fun is_last -> ())) node =
  let rec aux l = (
    match l with
      | hd :: tl ->
        before_each ();
        transform hd;
        after_each (List.is_empty tl);
        aux tl
      | [] -> ()
  ) in
  match syntax node with
    | Missing -> ()
    | SyntaxList x -> aux x
    | _ -> aux [node]

and handle_xhp_open_right_angle_token t =
  match syntax t with
    | Token token ->
      if EditableToken.text token = "/>" then add_space ();
      transform t
    | _ -> raise (Failure "expected xhp_open right_angle token")

and handle_function_call_expression fce =
  let (receiver, lp, args, rp) = get_function_call_expression_children fce in
  let () = match syntax receiver with
    | MemberSelectionExpression mse ->
      handle_possible_method_chaining mse (Some (lp, args, rp))
    | _ ->
      transform receiver;
      transform_argish lp args rp;
  in
  ()

and handle_possible_method_chaining mse argish =
  let (obj, arrow1, member1) = get_member_selection_expression_children mse in
  let rec handle_chaining obj =
    match syntax obj with
      | FunctionCallExpression x ->
        let (receiver, lp, args, rp) =
          get_function_call_expression_children x in
        (match syntax receiver with
          | MemberSelectionExpression mse ->
            let (obj, arrow, member) = get_member_selection_expression_children mse in
            let (obj, l) = handle_chaining obj in
            obj, l @ [(arrow, member, Some (lp, args, rp))]
          | _ -> obj, []
        )
      | _ -> obj, []
  in

  let (obj, l) = handle_chaining obj in
  let l = l @ [(arrow1, member1, argish)] in
  transform obj;

  let transform_chain (arrow, member, argish) =
    transform arrow;
    transform member;
    match argish with
      | Some (lp, args, rp) -> transform_argish lp args rp
      | None -> ()
    ;
  in
  let () = match l with
    | hd :: [] ->
      builder#simple_split ();
      builder#nest ();
      transform_chain hd
    | hd :: tl ->
      List.iteri l ~f:(fun n chain ->
        split ();
        if n = 0 then begin
          builder#nest ();
          builder#start_rule ~rule_type:(Rule.Argument) ();
        end;
        transform_chain chain
      );
      builder#end_rule ()
    | _ -> raise (Failure "Expected a chain of at least length 1")
  in
  builder#unnest ();
  ()

and handle_switch_body sb =
  let (left_b, statements, right_b) = get_compound_statement_children sb in
  add_space ();
  transform left_b;
  builder#end_chunks ();
  builder#start_block_nest ();
  tl_with ~f:(fun () ->
    let statement_list = match syntax statements with
      | Missing -> raise (Failure "Cannot have a missing statement list")
      | SyntaxList x -> x
      | _ -> [statements]
    in

    let handle_stmt stmt = match syntax stmt with
      | CaseStatement x ->
        let (kw, expr, colon, child_statement) =
          get_case_statement_children x in
        transform kw;
        split ~space:true ();
        transform expr;
        transform colon;
        builder#end_chunks ();
        [child_statement]
      | DefaultStatement x ->
        let (kw, colon, child_statement) =
          get_default_statement_children x in
        transform kw;
        transform colon;
        builder#end_chunks ();
        [child_statement]
      | _ ->
        builder#start_block_nest ();
        t_with ~nest:true stmt;
        builder#end_block_nest ();
        []
    in

    let rec iter_stmt stmt_list = match stmt_list with
      | [] -> ()
      | hd :: tl -> iter_stmt ((handle_stmt hd) @ tl)
    in
    iter_stmt statement_list
  ) ();
  builder#end_block_nest ();
  transform right_b;
  ()

and transform_function_declaration_header ~span_started x =
  let (async, kw, amp, name, type_params, leftp, params, rightp, colon,
    ret_type) = get_function_declaration_header_children x
  in

  if not span_started then builder#start_span ();

  transform async;
  if not (is_missing async) then add_space ();
  transform kw;
  add_space ();
  transform amp;
  transform name;
  transform type_params;
  transform_argish_with_return_type ~in_span:true leftp params rightp colon
    ret_type;

and transform_argish_with_return_type ~in_span left_p params right_p colon
    ret_type =
  transform left_p;
  split ();
  if in_span then builder#end_span ();

  tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
    tl_with ~nest:true ~f:(fun () ->
      handle_possible_list ~after_each:after_each_argument params
    ) ();
    transform right_p;
    transform colon;
    if not (is_missing colon) then add_space ();
    transform ret_type;
  ) ();
  ()

and transform_argish left_p arg_list right_p =
  transform left_p;
  split ();
  builder#start_span ();
  tl_with ~rule:(Some Rule.Argument) ~f:(fun () ->
    tl_with ~nest:true ~f:(fun () ->
      handle_possible_list ~after_each:after_each_argument arg_list
    ) ();
    split ();
    transform right_p
  ) ();
  builder#end_span ();
  ()

and transform_keyword_expression_statement kw expr semi =
  transform kw;
  builder#simple_space_split ();
  tl_with ~nest:true ~f:(fun () ->
    transform expr;
    transform semi;
  ) ();
  builder#end_chunks ();
  ()

and transform_binary_expression ~is_nested expr =
  let get_operator_type op =
    match syntax op with
      | Token t -> Full_fidelity_operator.trailing_from_token
        (EditableToken.kind t)
      | _ -> raise (Failure "Operator should always be a token")
  in

  let (left, operator, right) = get_binary_expression_children expr in
  let operator_t = get_operator_type operator in

  if Full_fidelity_operator.(
    is_assignment operator_t ||
    is_comparison operator_t
  ) then begin
    transform left;
    add_space ();
    transform operator;
    builder#simple_space_split ();
    t_with ~nest:true right;
  end else
  let precedence = Full_fidelity_operator.precedence operator_t in

  let rec flatten_expression expr =
    match syntax expr with
      | BinaryExpression x ->
        let (left, operator, right) = get_binary_expression_children x in
        let operator_t = get_operator_type operator in
        let op_precedence = Full_fidelity_operator.precedence operator_t in
        if (op_precedence = precedence) then
          (flatten_expression left) @ (operator :: flatten_expression right)
        else [expr]
      | _ -> [expr]
  in

  let transform_operand operand =
    match syntax operand with
      | BinaryExpression x -> transform_binary_expression ~is_nested:true x
      | _ -> transform operand
  in

  let l = flatten_expression (make_binary_expression left operator right) in
  (match l with
    | hd :: tl ->
      transform_operand hd;
      tl_with ~rule:(Some Rule.Argument) ~nest:is_nested ~f:(fun () ->
        List.iteri tl ~f:(fun i x ->
          if (i mod 2) = 0 then begin add_space (); transform x end
          else begin split ~space:true (); transform_operand x end
        );
        ()
      ) ();
    | _ -> raise (Failure "Expected non empty list of binary expression pieces")
  );
  ()

let debug_chunk_groups chunk_groups =
  Printf.printf "%d\n" (List.length chunk_groups);
  List.iteri chunk_groups ~f:(fun i cg ->
    Printf.printf "%d\n" i;
    Printf.printf "Indentation: %d\n" cg.Chunk_group.block_indentation;
    Printf.printf "Chunk count:%d\n" (List.length cg.Chunk_group.chunks);
    List.iteri cg.Chunk_group.chunks ~f:(fun i c ->
      Printf.printf "\t%d - %s - Nesting:%d\n"
        i (Chunk.to_string c) (Chunk.get_nesting_id c);
    );
    Printf.printf "Rule count %d\n"
      (IMap.cardinal cg.Chunk_group.rule_map);
    IMap.iter (fun k v ->
      Printf.printf "\t%d - %s\n" k (Rule.to_string v);
    ) cg.Chunk_group.rule_map;
  );
  ()

let run ?(debug=false) node =
  transform node;
  (* split (); *)
  let chunk_groups = builder#_end () in
  if debug then debug_chunk_groups chunk_groups;
  chunk_groups
