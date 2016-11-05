(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t = {
  text: string;
  spans: Span.t list;
  is_appendable: bool;
  rule: Rule.t;
}

let make text = {
  text = text;
  spans = [];
  is_appendable = true;
  rule = Rule.N;
}

let finalize chunk rule =
  let rule = match rule with
    | None -> Rule.Simple
    | Some r -> r
  in
  {chunk with
    is_appendable = false;
    rule = rule;
  }
