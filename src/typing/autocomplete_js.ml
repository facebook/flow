(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Constraint_js

type autocomplete_type =
| Acid of (Scope.Entry.t Utils.SMap.t)
| Acmem of (Type.t)

let autocomplete_suffix = "AUTO332"
let suffix_len = String.length autocomplete_suffix
let is_autocomplete x =
  String.length x >= suffix_len &&
  let suffix = String.sub x (String.length x - suffix_len) suffix_len in
  suffix = autocomplete_suffix

let autocomplete_id state cx name loc =
  if is_autocomplete name
  then (
    state := Some (Acid (Env_js.all_entries ()));
    true
  ) else
    false

let autocomplete_member state cx name loc this_t =
  if is_autocomplete name
  then (
    state := Some (Acmem (this_t));
    true
  ) else
    false

let autocomplete_set_hooks () =
  let state = ref None in
  Type_inference_hooks_js.set_id_hook (autocomplete_id state);
  Type_inference_hooks_js.set_member_hook (autocomplete_member state);
  state

let autocomplete_unset_hooks () =
  Type_inference_hooks_js.reset_hooks ()
