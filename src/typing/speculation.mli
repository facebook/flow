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

val init_speculation : 'phase Context.t_ -> speculation_id -> unit

val add_unresolved_to_speculation : 'phase Context.t_ -> speculation_id -> Type.ident -> unit

(* Maintain a stack of speculative branches. See Speculation for the contents
   of the "branch" data structure.

   When speculating (i.e., when this stack is non-empty), some things are
   handled differently:

   (1) flow and unify actions on unresolved tvars are deferred
   (2) any errors cause short-cutting
*)
val set_speculative : 'phase Context.t_ -> branch -> unit

val restore_speculative : 'phase Context.t_ -> unit

val speculating : 'phase Context.t_ -> bool

(* decide whether an action should be deferred.
   when speculating, actions that involve unresolved tvars are deferred. *)
val defer_action : 'phase Context.t_ -> action -> bool

val case_diff : 'phase Context.t_ -> case -> case -> (Type.ident * Reason.reason) list
