(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Functions used to initialize and add unresolved tvars during type resolution
   of lower/upper bounds of union/intersection types, respectively *)

open Speculation_state

type speculation_id = int

val init_speculation : Context.t -> speculation_id -> unit

val add_unresolved_to_speculation : Context.t -> speculation_id -> Type.ident -> unit

(* Maintain a stack of speculative branches. See Speculation for the contents
   of the "branch" data structure.

   When speculating (i.e., when this stack is non-empty), some things are
   handled differently:

   (1) flow and unify actions on unresolved tvars are deferred
   (2) any errors cause short-cutting
*)
val set_speculative : Context.t -> branch -> unit

val restore_speculative : Context.t -> unit

val speculating : Context.t -> bool

(* decide whether an action should be deferred.
   when speculating, actions that involve unresolved tvars are deferred. *)
val defer_action : Context.t -> action -> bool

val case_diff : Context.t -> case -> case -> (Type.ident * Reason.reason) list
