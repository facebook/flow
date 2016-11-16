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
  val mutable nesting = Nesting.make None 0;
  val mutable rules = [];

  val mutable chunks = [];

  val mutable next_split_rule = None;
  val mutable space_if_not_split = false;

  val mutable pending_whitespace = "";

  method add_string s =
    chunks <- (match chunks with
      | hd :: tl when hd.Chunk.is_appendable ->
        let text = hd.Chunk.text ^ pending_whitespace ^ s in
        {hd with Chunk.text = text} :: tl
      | _ -> begin
          match next_split_rule with
            | None -> [Chunk.make s (List.hd rules) nesting] @ chunks
            | Some id ->
              this#start_rule ~rule:id ();
              let cs = [Chunk.make s (List.hd rules) nesting] @ chunks in
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
        let chunk = (Chunk.finalize hd (List.hd rules) space_if_not_split) in
        space_if_not_split <- space;
        chunk :: tl
      | [] -> raise (Failure "No chunks to split")
      | _ -> chunks
    )

  method simple_space_split () =
    this#split true;
    next_split_rule <- Some (Rule.simple_rule ());
    ()

  method simple_split () =
    this#split false;
    next_split_rule <- Some (Rule.simple_rule ());
    ()

  method hard_split () =
    this#split false;
    next_split_rule <- Some (Rule.always_rule ());
    ()

  method end_chunks () =
    this#hard_split ()

  method nest ?amount:(amount=2) () =
    nesting <- (Nesting.make (Some nesting) 2);
    ()

  method unnest () =
    nesting <- begin match nesting.Nesting.parent with
      | Some p -> p
      | None -> raise (Failure "unnested too far")
    end;
    ()

  method start_rule ?rule:(rule=Rule.simple_rule ()) () =
    (* Override next_split_rule unless it's an Always rule *)
    next_split_rule <- (match next_split_rule with
      | Some id when Rule.get_kind id <> Rule.Always -> None
      | _ -> next_split_rule
    );
    Rule.mark_dependencies rules rule;
    rules <- rule :: rules

  method end_rule () =
    rules <- match rules with
      | hd :: tl -> tl
      | [] -> [] (*TODO: error *)

  method has_rule_kind kind =
    List.exists rules ~f:(fun id -> Rule.get_kind id = kind)

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

  method _end () =
    this#split false;
    List.rev chunks

  method __debug chunks =
    let d_chunks = chunks in
    List.iter d_chunks ~f:(fun c ->
      Printf.printf
        "Span count:%d\t Rule:%s\t Text:%s\n"
        (List.length c.Chunk.spans)
        (Rule.to_string c.Chunk.rule)
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

let start_argument_rule () =
  builder#start_rule ~rule:(Rule.argument_rule()) ()

let end_rule () =
  builder#end_rule ()

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
    t x.script_declarations
  | SimpleTypeSpecifier {simple_type_specifier} -> t simple_type_specifier
  | LiteralExpression x -> t x.literal_expression
  | QualifiedNameExpression x ->
    t x.qualified_name_expression
  | VariableExpression x -> t x.variable_expression
  | PipeVariableExpression x -> t x.pipe_variable_expression
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
    let (leftp, body, rightp) = get_classish_body_children x in
    add_space ();
    t leftp;
    builder#end_chunks ();
    tl_with ~nest ~f:(fun () ->
      handle_possible_list body
    ) ();
    t rightp;
    builder#end_chunks ();
    ()
  | TraitUse x ->
    let (kw, elements, semi) = get_trait_use_children x in
    t kw;
    handle_possible_list ~before_each:(split ~space) elements;
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
    tl_with ~nest ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
    start_argument_rule ();
    t_with ~nest x.while_condition;
    split ();
    t x.while_right_paren;
    end_rule ();
    handle_possible_compound_statement x.while_body;
    builder#end_chunks ();
    ()
  | IfStatement x ->
    t x.if_keyword;
    add_space ();
    t x.if_left_paren;
    split ();
    start_argument_rule ();
    t_with ~nest x.if_condition;
    split ();
    t x.if_right_paren;
    end_rule ();
    handle_possible_compound_statement x.if_statement;
    handle_possible_list x.if_elseif_clauses;
    t x.if_else_clause;
    builder#end_chunks ();
    ()
  | ElseifClause x ->
    t x.elseif_keyword;
    add_space ();
    t x.elseif_left_paren;
    split ();
    t_with ~nest x.elseif_condition;
    split ();
    t x.elseif_right_paren;
    handle_possible_compound_statement x.elseif_statement;
    ()
  | ElseClause x ->
    t x.else_keyword;
    handle_possible_compound_statement x.else_statement;
    ()
  | SwitchStatement x ->
    let (kw, left_p, expr, right_p, body) = get_switch_statement_children x in
    t kw;
    add_space ();
    t left_p;
    split ();
    tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
  | SimpleInitializer x ->
    let (eq_kw, value) = get_simple_initializer_children x in
    add_space ();
    t eq_kw;
    builder#simple_space_split ();
    t_with ~nest value;
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
  | BinaryExpression x ->
    builder#start_span ();
    (* TODO: nested binary expressions split by precedence *)
    t x.binary_left_operand;
    add_space ();
    t x.binary_operator;
    split ~space:true ();
    t_with ~nest ~rule:(Rule.simple_rule ()) x.binary_right_operand;
    builder#end_span ()
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
    tl_with ~nest ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
      tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
    tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
    tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
      tl_with ~nest ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
      tl_with ~rule:(Rule.xhp_expression_rule ()) ~f:(handle_body_close) ();
    end else begin
      tl_with ~rule:(Rule.xhp_expression_rule ()) ~f:(fun () ->
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
    tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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

and tl_with ?(nest=false) ?(rule= -1) ?(span=false) ~f () =
  if rule <> -1 then builder#start_rule ~rule ();
  if nest then builder#nest ();
  if span then builder#start_span ();

  f ();

  if nest then builder#unnest ();
  if rule <> -1 then builder#end_rule ();
  if span then builder#end_span ();
  ()

and t_with ?(nest=false) ?(rule= -1) ?(span=false) ?(f=transform) node =
  if rule <> -1 then builder#start_rule ~rule ();
  if nest then builder#nest ();
  if span then builder#start_span ();

  f node;

  if nest then builder#unnest ();
  if rule <> -1 then builder#end_rule ();
  if span then builder#end_span ();
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
      tl_with ~rule:(Rule.simple_rule ()) ~nest:true ~f:(fun () ->
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
      t_with ~nest:true node;
      ()

and handle_compound_statement cs =
  let (left_b, statements, right_b) = get_compound_statement_children cs in
  add_space ();
  transform left_b;
  builder#end_chunks ();
  tl_with ~nest:true ~f:(fun () ->
    handle_possible_list statements;
  ) ();
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
          builder#start_rule ~rule:(Rule.argument_rule ()) ();
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
  tl_with ~nest:true ~f:(fun () ->
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
        t_with ~nest:true stmt;
        []
    in

    let rec iter_stmt stmt_list = match stmt_list with
      | [] -> ()
      | hd :: tl -> iter_stmt ((handle_stmt hd) @ tl)
    in
    iter_stmt statement_list
  ) ();
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

  tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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
  tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
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

let run ?(debug=false) node =
  transform node;
  split ();
  let chunks = builder#_end () in
  if debug then builder#__debug chunks;
  chunks
