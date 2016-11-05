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

let builder = object
  val mutable open_spans = Stack.create ();
  val mutable rules = [];

  val mutable chunks = [];

  method add_string s =
    chunks <- match chunks with
      | hd :: tl when hd.Chunk.is_appendable ->
        {hd with Chunk.text = hd.Chunk.text ^ s} :: tl
      | _ -> [Chunk.make s (List.hd rules)] @ chunks

  method split () =
    chunks <- match chunks with
      | hd :: tl ->
        (Chunk.finalize hd (List.hd rules)) :: tl
      | [] -> chunks (* TODO: error here *)

  method start_rule ?rule:(rule=Rule.simple_rule ()) () =
    (* TODO: handle rules containing this rule callback
    rules <- List.map rules ~f(fun r ->
    *)
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
    chunks

  method __debug () =
    let d_chunks = List.rev chunks in
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


let token x =
  builder#add_string (EditableToken.text x)

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
    split ();

    t x.binary_right_operand;
    builder#end_span ();
    builder#end_rule ();
  | FunctionCallExpression x ->
    builder#start_span ();
    t x.function_call_receiver;
    t x.function_call_left_paren;
    split ();
    builder#start_rule ~rule:(Rule.argument_rule ()) ();
    t x.function_call_argument_list;
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
  builder#__debug ();
  List.rev (builder#_end ())
