(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Functions used to initialize and add unresolved tvars during type resolution
   of lower/upper bounds of union/intersection types, respectively *)

open Speculation_state

type speculation_id = int

(* Maintain a stack of speculative branches. See Speculation for the contents
   of the "branch" data structure.

   When speculating (i.e., when this stack is non-empty), some things are
   handled differently: any errors cause short-cutting
*)
val set_speculative : Context.t -> branch -> unit

val restore_speculative : Context.t -> unit

val speculating : Context.t -> bool

val defer_error : Context.t -> Error_message.t -> unit
