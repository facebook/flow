(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type autocomplete_type =
| Acid of Scope.Entry.t SMap.t
| Acmem of Type.t
| Acjsx of Type.t
| Acgql_fld of Graphql_schema.t * string
| Acgql_type of Graphql_schema.t * string option

type autocomplete_state = {
  ac_name: string;
  ac_loc: Loc.t;
  ac_type: autocomplete_type;
}

let autocomplete_suffix = "AUTO332"
let suffix_len = String.length autocomplete_suffix
let is_autocomplete x =
  String.length x >= suffix_len &&
  let suffix = String.sub x (String.length x - suffix_len) suffix_len in
  suffix = autocomplete_suffix

let autocomplete_id state _cx ac_name ac_loc =
  if is_autocomplete ac_name
  then (
    state := Some ({
      ac_name;
      ac_loc;
      ac_type = Acid (Env.all_entries ());
    });
    true
  ) else
    false

let autocomplete_member state _cx ac_name ac_loc this_t =
  if is_autocomplete ac_name
  then (
    state := Some ({
      ac_name;
      ac_loc;
      ac_type = Acmem (this_t);
    });
    true
  ) else
    false

let autocomplete_jsx state _cx ac_name ac_loc class_t =
  if is_autocomplete ac_name
  then (
    state := Some ({
      ac_name;
      ac_loc;
      ac_type = Acjsx (class_t);
    });
    true
  ) else
    false

let autocomplete_graphql_field state schema ac_name ac_loc type_name =
  if is_autocomplete ac_name
  then (
    state := Some ({
      ac_name;
      ac_loc;
      ac_type = Acgql_fld (schema, type_name);
    });
    true
  ) else
    false

let autocomplete_graphql_type state schema ac_name ac_loc parent_type =
  if is_autocomplete ac_name
  then (
    state := Some ({
      ac_name;
      ac_loc;
      ac_type = Acgql_type (schema, parent_type);
    });
    true
  ) else
    false

let autocomplete_set_hooks () =
  let state = ref None in
  Type_inference_hooks_js.set_id_hook (autocomplete_id state);
  Type_inference_hooks_js.set_member_hook (autocomplete_member state);
  Type_inference_hooks_js.set_jsx_hook (autocomplete_jsx state);
  Type_inference_hooks_js.set_graphql_field_hook (autocomplete_graphql_field state);
  Type_inference_hooks_js.set_graphql_type_hook (autocomplete_graphql_type state);
  state

let autocomplete_unset_hooks () =
  Type_inference_hooks_js.reset_hooks ()
