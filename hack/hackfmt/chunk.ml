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
  rule: Rule.t;
}

let default_rule = {
  text = "";
  spans = [];
  is_appendable = true;
  rule = Rule.null_rule ();
}

let make text rule =
  let c = match rule with
    | None -> default_rule
    | Some r -> {default_rule with rule = r}
  in
  {c with text = text}

let finalize chunk rule =
  let rule = match rule with
    | _ when chunk.rule <> (Rule.null_rule ()) -> chunk.rule
    | None -> Rule.simple_rule ()
    | Some r -> r
  in
  {chunk with
    is_appendable = false;
    rule = rule;
  }

let has_split_before chunk =
  Rule.is_split chunk.rule

let get_span_split_cost chunk =
  List.fold_left chunk.spans ~init:0 ~f:(fun acc s ->
    acc + s.Span.cost
  )
