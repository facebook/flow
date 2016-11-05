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

  method add_string s =
    chunks <- match chunks with
      | hd :: tl when hd.Chunk.is_appendable ->
        {hd with Chunk.text = hd.Chunk.text ^ s} :: tl
      | _ -> begin
          let hard_split = next_split_hard in
          next_split_hard <- false;
          if hard_split then this#start_rule ~rule:(Rule.always_rule ()) ();
          let cs = [Chunk.make s (List.hd rules) nesting] @ chunks in
          if hard_split then this#end_rule ();
          cs

      end

  method split () =
    chunks <- (match chunks with
      | hd :: tl ->
        (Chunk.finalize hd (List.hd rules)) :: tl
      | [] -> raise (Failure "No chunks to split")
    );

    this#__debug (List.rev chunks);

  method hard_split () =
    this#split ();
    next_split_hard <- true;
    ()

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

  method _end () =
    this#split ();
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
  builder#split ()

let handle_trivia trivia_list =
  List.iter trivia_list ~f:(fun t ->
    match Trivia.kind t with
      | TriviaKind.SingleLineComment ->
        builder#split ();
        builder#start_rule ();
        builder#add_string (Trivia.text t);
        builder#end_rule ();
        builder#hard_split ();
      | TriviaKind.DelimitedComment ->
        builder#split ();
        builder#start_rule ();
        builder#add_string (Trivia.text t);
        builder#end_rule ();
        builder#split ();
      | _ -> ()
  )

let token x =
  handle_trivia (EditableToken.leading x);
  builder#add_string (EditableToken.text x);
  handle_trivia (EditableToken.trailing x);
  ()

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
  | SyntaxList x ->
    List.iter x ~f:(fun item ->
      t item;
      (* TODO:
         should every syntax list have a split after each item?
         should every syntax list split have a space if no newline?
       *)
      split ~space:true ()
    )
  | ListItem x ->
    t x.list_item;
    t x.list_separator
  | ExpressionStatement x ->
    t x.expression_statement_expression;
    t x.expression_statement_semicolon;
  | BinaryExpression x ->
    builder#start_span ();
    (* nest_expression? *)
    builder#start_rule (); (* lazy? *)

    (* TODO: nested binary expressions split by precedence *)
    t x.binary_left_operand;

    (* TODO: figure out binary spacing *)
    t x.binary_operator;
    builder#end_rule ();
    split ();
    builder#nest ();
    builder#start_rule ();
    t x.binary_right_operand;
    builder#unnest ();
    builder#end_span ();
    builder#end_rule ();
  | FunctionCallExpression x ->
    builder#start_span ();
    t x.function_call_receiver;
    t x.function_call_left_paren;
    split ();
    builder#nest ();
    builder#start_rule ~rule:(Rule.argument_rule ()) ();
    t x.function_call_argument_list;
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

let run node =
  transform node;
  split ();
  let chunks = builder#_end () in
  builder#__debug chunks;
  chunks
