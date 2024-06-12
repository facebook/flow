(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Speculation_state

(* Various functions used to prepare for and execute speculative matching. *)

(* Functions used to initialize and add unresolved tvars during type resolution
   of lower/upper bounds of union/intersection types, respectively *)

type speculation_id = int

let set_speculative cx branch =
  let state = Context.speculation_state cx in
  state := branch :: !state

let restore_speculative cx =
  let state = Context.speculation_state cx in
  state := List.tl !state

let speculating cx =
  let state = Context.speculation_state cx in
  !state <> []

(* Decide, for a flow or unify action encountered during a speculative match,
   whether that action should be deferred. Only a relevant action is deferred.

   As a side effect, whenever we decide to defer an action, we record the
   deferred action and the unresolved tvars involved in it in the current
   case.
*)
let defer_if_relevant branch action =
  let { case; _ } = branch in
  match action with
  | ErrorAction _ ->
    case.actions <- case.actions @ [(true, action)];
    true
  | _ -> false

let defer_action cx action =
  let state = Context.speculation_state cx in
  match !state with
  | [] -> false
  | branch :: _ -> defer_if_relevant branch action
