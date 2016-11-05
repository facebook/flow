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

let token x =
  handle_trivia (EditableToken.leading x);
  builder#add_string (EditableToken.text x);
  handle_trivia (EditableToken.trailing x);
  ()

let space () =
  builder#add_string " "

let pending_space () =
  builder#add_pending_whitespace " "

let rec transform node =
  let t = transform in
  let () = match syntax node with
  | Missing -> ()
  | Token x ->
    token x;
    ()
  | Script x ->
    (* TODO script_header*)
    t x.script_declarations
  | LiteralExpression x -> t x.literal_expression
  | VariableExpression x -> t x.variable_expression
  | QualifiedNameExpression x ->
    t x.qualified_name_expression
  | ListItem x ->
    t x.list_item;
    t x.list_separator
  | IfStatement x ->
    t x.if_keyword;
    space ();
    t x.if_left_paren;
    split ();
    builder#nest ();
    t x.if_condition;
    split ();
    builder#unnest ();
    t x.if_right_paren;
    builder#end_rule ();
    handle_possible_compound_statement x.if_statement;
    handle_possible_list x.if_elseif_clauses;
    t x.if_else_clause;
    builder#end_chunks ();
    ()
  | ElseifClause x ->
    t x.elseif_keyword;
    space ();
    t x.elseif_left_paren;
    split ();
    builder#nest ();
    t x.elseif_condition;
    split ();
    builder#unnest ();
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
  | BinaryExpression x ->
    builder#start_span ();
    (* nest_expression? *)
    builder#start_rule (); (* lazy? *)

    (* TODO: nested binary expressions split by precedence *)
    t x.binary_left_operand;
    space ();
    t x.binary_operator;
    builder#end_rule ();
    split ~space:true ();
    builder#nest ();
    builder#start_rule ();
    t x.binary_right_operand;
    builder#unnest ();
    builder#end_span ();
    builder#end_rule ()
  | FunctionCallExpression x ->
    builder#start_span ();
    t x.function_call_receiver;
    t x.function_call_left_paren;
    split ();
    builder#nest ();
    builder#start_rule ~rule:(Rule.argument_rule ()) ();
    handle_possible_list ~after_each:after_each_argument
      x.function_call_argument_list;
    builder#unnest ();
    t x.function_call_right_paren;
    builder#end_rule ();
    builder#end_span ()
  | _ ->
    Printf.printf "%s not supported - exiting \n"
      (SyntaxKind.to_string (kind node));
    exit 1
  in
  ()

and after_each_argument is_last =
  split ~space:(not is_last) ();

and handle_possible_compound_statement node =
  match syntax node with
    | CompoundStatement x ->
      space ();
      transform x.compound_left_brace;
      builder#end_chunks ();
      builder#nest ();
      transform x.compound_statements;
      builder#unnest ();
      transform x.compound_right_brace;
      pending_space ();
    | _ ->
      builder#end_chunks ();
      builder#nest ();
      transform node;
      builder#unnest ()

and handle_possible_list ?after_each:(after_each=(fun is_last -> ())) node =
  match syntax node with
    | SyntaxList x ->
      let rec aux l = (
        match l with
          | hd :: tl ->
            transform hd;
            after_each (List.is_empty tl);
            aux tl
          | [] -> ()
      ) in
      aux x
    | _ ->
      transform node;
      after_each true

let run node =
  transform node;
  split ();
  let chunks = builder#_end () in
  builder#__debug chunks;
  chunks
