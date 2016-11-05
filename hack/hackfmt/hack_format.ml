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

  val mutable next_split_hard = false;
  val mutable space_if_not_split = false;

  val mutable pending_whitespace = "";

  method add_string s =
    chunks <- (match chunks with
      | hd :: tl when hd.Chunk.is_appendable ->
        let text = hd.Chunk.text ^ pending_whitespace ^ s in
        {hd with Chunk.text = text} :: tl
      | _ -> begin
          let hard_split = next_split_hard in
          next_split_hard <- false;
          if hard_split then this#start_rule ~rule:(Rule.always_rule ()) ();
          let cs = [Chunk.make s (List.hd rules) nesting] @ chunks in
          if hard_split then this#end_rule ();
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

  method hard_split () =
    this#split false;
    next_split_hard <- true;
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
    Rule.mark_dependencies rules rule;
    rules <- rule :: rules

  method end_rule () =
    rules <- match rules with
      | hd :: tl -> tl
      | [] -> [] (*TODO: error *)

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
    handle_possible_compound_statement body;
    t semi;
    builder#end_chunks ();
    ()
  | ClassishDeclaration x ->
    let (attr, modifiers, kw, name, type_params, extends_kw, extends,
      impl_kw, impls, body) = get_classish_declaration_children x
    in
    t attr;
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
  | ListItem x ->
    t x.list_item;
    t x.list_separator
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
  | ReturnStatement x ->
    let (kw, expr, semi) = get_return_statement_children x in
    t kw;
    split ~space ();
    tl_with ~nest ~f:(fun () ->
      t expr;
      t semi;
    ) ();
    builder#end_chunks ();
    ()
  | ScopeResolutionExpression x ->
    let (qual, operator, name) = get_scope_resolution_expression_children x in
    t qual;
    t operator;
    t name;
    ()
  | MemberSelectionExpression x ->
    let (obj, operator, name) = get_member_selection_expression_children x in
    t obj;
    split ();
    tl_with ~nest ~f:(fun () ->
      t operator;
      t name;
    ) ();
    ()
  | BinaryExpression x ->
    builder#start_span ();
    (* TODO: nested binary expressions split by precedence *)
    t x.binary_left_operand;
    add_space ();
    t x.binary_operator;
    split ~space:true ();
    t_with ~nest ~rule:(Rule.simple_rule ()) x.binary_right_operand;
    builder#end_span ()
  | FunctionCallExpression x ->
    builder#start_span ();
    t x.function_call_receiver;
    t x.function_call_left_paren;
    split ();
    tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
      tl_with ~nest ~f:(fun () ->
        handle_possible_list ~after_each:after_each_argument
          x.function_call_argument_list
      ) ();
      t x.function_call_right_paren
    ) ();
    builder#end_span ()
  | ObjectCreationExpression x ->
    let (kw, obj_type, left_p, arg_list, right_p) =
      get_object_creation_expression_children x
    in
    t kw;
    add_space ();
    t obj_type;
    transform_argish left_p arg_list right_p;
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
  | GenericTypeSpecifier x ->
    let (class_type, type_args) = get_generic_type_specifier_children x in
    t class_type;
    t type_args;
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

and handle_possible_compound_statement node =
  match syntax node with
    | CompoundStatement x ->
      add_space ();
      transform x.compound_left_brace;
      builder#end_chunks ();
      t_with ~nest:true x.compound_statements;
      transform x.compound_right_brace;
      pending_space ();
      ()
    | _ ->
      builder#end_chunks ();
      t_with ~nest:true node;
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
    | SyntaxList x -> aux x
    | _ -> aux [node]

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
  transform leftp;
  split ();
  builder#end_span ();

  tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
    tl_with ~nest:true ~f:(fun () ->
      handle_possible_list ~after_each:after_each_argument params
    ) ();
    transform rightp;
    transform colon;
    add_space ();
    transform ret_type;
  ) ();
  ()

and transform_argish left_p arg_list right_p =
  transform left_p;
  split ();
  tl_with ~rule:(Rule.argument_rule ()) ~f:(fun () ->
    tl_with ~nest:true ~f:(fun () ->
      handle_possible_list ~after_each:after_each_argument arg_list
    ) ();
    split ();
    transform right_p
  ) ();
  ()

let run node =
  transform node;
  split ();
  let chunks = builder#_end () in
  builder#__debug chunks;
  chunks
