(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let id_nop _ _ _ = false

let member_nop _ _ _ _ = false

let call_nop _ _ _ _ = ()

let require_nop _ _ _ = ()

type hook_state_t = {
  id_hook:
     (Constraint_js.context ->
      string -> Loc.t ->
      bool);

  member_hook:
     (Constraint_js.context ->
      string -> Loc.t -> Constraint_js.Type.t ->
      bool);

(* TODO: This is inconsistent with the way the id/member hooks work, but we
         currently don't need a way to override call types, so it simplifies
         things a bit *)
  call_hook:
     (Constraint_js.context ->
      string -> Loc.t -> Constraint_js.Type.t ->
      unit);

  require_hook:
     (Constraint_js.context ->
      string -> Loc.t ->
      unit);
}

let nop_hook_state = {
  id_hook = id_nop;
  member_hook = member_nop;
  call_hook = call_nop;
  require_hook = require_nop;
}

let hook_state = ref nop_hook_state

let set_id_hook hook =
  hook_state := { !hook_state with id_hook = hook }

let set_member_hook hook =
  hook_state := { !hook_state with member_hook = hook }

let set_call_hook hook =
  hook_state := { !hook_state with call_hook = hook }

let set_require_hook hook =
  hook_state := { !hook_state with require_hook = hook }

let reset_hooks () =
  hook_state := nop_hook_state

let dispatch_id_hook cx name loc =
  !hook_state.id_hook cx name loc

let dispatch_member_hook cx name loc this_t =
  !hook_state.member_hook cx name loc this_t

let dispatch_call_hook cx name loc this_t =
  !hook_state.call_hook cx name loc this_t

let dispatch_require_hook cx name loc =
  !hook_state.require_hook cx name loc
