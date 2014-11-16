(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

open Constraint_js

type autocomplete_type =
| Acid of (block_entry Utils.SMap.t)
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
    state := Some (Acid (Env_js.flat_env ()));
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
