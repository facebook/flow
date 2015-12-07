(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let id_nop _ _ _ = false

let lval_nop _ _ _ _ = ()

let member_nop _ _ _ _ = false

let call_nop _ _ _ _ = ()

let require_nop _ _ _ = ()

let import_nop _ _ _ = ()

let jsx_nop _ _ _ _ = false

(* This type represents the possible definition-points for an lvalue. *)
type rhs_def =
  (**
   * Given a simple assignment/initialization such as:
   *
   *   var a = 42; // <-- this
   *   b = a;      // <-- also this
   *
   * We emit the raw location of the expression on the RHS of the equal sign as
   * the "definition" for the lvalue.
   *)
  | RHSLoc of Loc.t

  (*
   * Given a destructuring pattern for an assignment/initialization such as:
   *
   *  var {
   *    a // <-- this
   *  } = {a: {b: 42}};
   *
   *  ( {b} = a );
   *  // ^-- this
   *
   * We emit the tvar for the "parent pattern" being destructured so that we
   * may extract the type of the corresponding property -- and thus the location
   * of that type -- as the "definition" for the binding generated from a
   * destructuring pattern.
   *)
  | RHSType of Type.t

  (**
   * Some lvalues have no RHS such as initializer-less variable declarations
   * or function parameters.
   *)
  | NoRHS

type hook_state_t = {
  id_hook:
     (Context.t ->
      string -> Loc.t ->
      bool);

  lval_hook:
    (Context.t ->
      string -> Loc.t -> rhs_def ->
      unit);

  member_hook:
     (Context.t ->
      string -> Loc.t -> Type.t ->
      bool);

(* TODO: This is inconsistent with the way the id/member hooks work, but we
         currently don't need a way to override call types, so it simplifies
         things a bit *)
  call_hook:
     (Context.t ->
      string -> Loc.t -> Type.t ->
      unit);

  require_hook:
     (Context.t ->
      string -> Loc.t ->
      unit);

  import_hook:
      (Context.t ->
       string -> Loc.t ->
       unit);

  jsx_hook:
      (Context.t ->
       string -> Loc.t -> Type.t ->
       bool);
}

let nop_hook_state = {
  id_hook = id_nop;
  lval_hook = lval_nop;
  member_hook = member_nop;
  call_hook = call_nop;
  require_hook = require_nop;
  import_hook = import_nop;
  jsx_hook = jsx_nop;
}

let hook_state = ref nop_hook_state

let set_id_hook hook =
  hook_state := { !hook_state with id_hook = hook }

let set_lval_hook hook =
  hook_state := { !hook_state with lval_hook = hook }

let set_member_hook hook =
  hook_state := { !hook_state with member_hook = hook }

let set_call_hook hook =
  hook_state := { !hook_state with call_hook = hook }

let set_require_hook hook =
  hook_state := { !hook_state with require_hook = hook }

let set_import_hook hook =
  hook_state := { !hook_state with import_hook = hook }

let set_jsx_hook hook =
  hook_state := { !hook_state with jsx_hook = hook }

let reset_hooks () =
  hook_state := nop_hook_state

let dispatch_id_hook cx name loc =
  !hook_state.id_hook cx name loc

let dispatch_lval_hook cx name lhs_loc rhs_loc =
  !hook_state.lval_hook cx name lhs_loc rhs_loc

let dispatch_member_hook cx name loc this_t =
  !hook_state.member_hook cx name loc this_t

let dispatch_call_hook cx name loc this_t =
  !hook_state.call_hook cx name loc this_t

let dispatch_require_hook cx name loc =
  !hook_state.require_hook cx name loc

let dispatch_import_hook cx name loc =
  !hook_state.import_hook cx name loc

let dispatch_jsx_hook cx name loc this_t =
  !hook_state.jsx_hook cx name loc this_t
