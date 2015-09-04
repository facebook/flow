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
open Utils

type getdef_type =
| Gdloc of Loc.t
| Gdmem of (string * Type.t)

let getdef_id (state, loc1) cx name loc2 =
  if Reason_js.in_range loc1 loc2
  then (
    let env = Env_js.all_entries () in
    match SMap.get name env with
    | Some entry ->
      (match Scope.Entry.loc entry with
      | Some loc ->
        state := Some (Gdloc (loc))
      | None ->
        ())
    | None ->
      ());
  false

let getdef_member (state, loc1) cx name loc2 this_t =
  if (Reason_js.in_range loc1 loc2)
  then (
    state := Some (Gdmem (name, this_t))
  );
  false

let getdef_call (state, loc1) cx name loc2 this_t =
  if (Reason_js.in_range loc1 loc2)
  then (
    state := Some (Gdmem (name, this_t))
  )

let getdef_get_result cx state =
  match !state with
  | Some Gdloc (loc) -> loc
  | Some Gdmem (name, this) ->
      let this_t = Flow_js.resolve_type cx this in
      let member_result = Flow_js.Autocomplete.extract_members cx this_t in
      let result_map =
        Flow_js.Autocomplete.map_of_member_result member_result in
      (match SMap.get name result_map with
      | Some t ->
          loc_of_t t
      | None ->
          Loc.none)
  | _ ->
      Loc.none

let getdef_set_hooks pos =
  let state = ref None in
  Type_inference_hooks_js.set_id_hook (getdef_id (state, pos));
  Type_inference_hooks_js.set_member_hook (getdef_member (state, pos));
  Type_inference_hooks_js.set_call_hook (getdef_call (state, pos));
  state

let getdef_unset_hooks () =
  Type_inference_hooks_js.reset_hooks ()
