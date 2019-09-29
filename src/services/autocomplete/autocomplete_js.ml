(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type autocomplete_type =
  | Acid of Scope.Entry.t SMap.t
  | Ackey (* TODO: track which object, so we can complete the keys.
           for now, just classifying the kind of autocomplete *)
  | Acmem of Type.t
  | Acjsx of Type.t

type autocomplete_state = {
  ac_name: string;
  ac_loc: ALoc.t;
  ac_type: autocomplete_type;
}

let autocomplete_suffix = "AUTO332"

let suffix_len = String.length autocomplete_suffix

let is_autocomplete x =
  String.length x >= suffix_len
  &&
  let suffix = String.sub x (String.length x - suffix_len) suffix_len in
  suffix = autocomplete_suffix

let autocomplete_id from_trigger_character state _cx ac_name ac_loc =
  if is_autocomplete ac_name && not from_trigger_character then (
    state := Some { ac_name; ac_loc; ac_type = Acid (Env.all_entries ()) };
    true
  ) else
    false

let autocomplete_object_key from_trigger_character state _cx ac_name ac_loc =
  if is_autocomplete ac_name && not from_trigger_character then (
    state := Some { ac_name; ac_loc; ac_type = Ackey };
    true
  ) else
    false

let autocomplete_member state _cx ac_name ac_loc this_t =
  if is_autocomplete ac_name then (
    state := Some { ac_name; ac_loc; ac_type = Acmem this_t };
    true
  ) else
    false

let autocomplete_jsx state _cx ac_name ac_loc class_t =
  if is_autocomplete ac_name then (
    state := Some { ac_name; ac_loc; ac_type = Acjsx class_t };
    true
  ) else
    false

let autocomplete_set_hooks ~trigger_character =
  let state = ref None in
  Type_inference_hooks_js.set_id_hook (autocomplete_id (trigger_character <> None) state);
  Type_inference_hooks_js.set_obj_prop_decl_hook
    (autocomplete_object_key (trigger_character <> None) state);
  Type_inference_hooks_js.set_member_hook (autocomplete_member state);
  Type_inference_hooks_js.set_jsx_hook (autocomplete_jsx state);
  state

let autocomplete_unset_hooks () = Type_inference_hooks_js.reset_hooks ()
