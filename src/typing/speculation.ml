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

let defer_error cx msg =
  let state = Context.speculation_state cx in
  match !state with
  | [] -> ()
  | { case; _ } :: _ -> case.errors <- case.errors @ [msg]
