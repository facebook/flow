(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

type t = {
  text: string;
  spans: Span.t list;
  is_appendable: bool;
  space_if_not_split: bool;
  rule: int;
  nesting: Nesting.t
}

let default_chunk = {
  text = "";
  spans = [];
  is_appendable = true;
  space_if_not_split = false;
  rule = Rule.null_rule_id;
  nesting = {Nesting.id = -1; amount = 0; parent = None; };
}

let make text rule nesting =
  let c = match rule with
    | None -> default_chunk
    | Some r -> {default_chunk with rule = r}
  in
  {c with text; nesting;}

let finalize chunk rule space =
  let rule = match rule with
    (* TODO: refactor: | r when (Rule.get_kind r) = Rule.Always -> r *)
    | _ when chunk.rule <> Rule.null_rule_id -> chunk.rule
    | r -> r
  in
  {chunk with
    is_appendable = false;
    rule;
    space_if_not_split = space;
  }

let get_span_split_cost chunk =
  List.fold_left chunk.spans ~init:0 ~f:(fun acc s ->
    acc + s.Span.cost
  )

let get_nesting_id chunk =
  chunk.nesting.Nesting.id

let to_string chunk =
  Printf.sprintf "rule_id:%d\t text:%s" chunk.rule chunk.text
