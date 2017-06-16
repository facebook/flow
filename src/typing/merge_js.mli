(**
 * Copyright (c) 2014-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

val merge_component_strict:
  (* component cxs *)
  Context.t list ->
  (* component impls *)
  (Context.t * string * string * Context.t) list ->
  (* dependency cxs *)
  Context.t list ->
  (* dependency impls *)
  (Context.t * string * string * Context.t) list ->
  (* resources *)
  (string * Loc.t * string * Context.t) list ->
  (* declarations *)
  (string * Loc.t * Modulename.t * Context.t) list ->
  (* unchecked *)
  (string * Loc.t * Context.t) list ->
  (* master cx *)
  Context.t ->
  unit

val restore: Context.t ->
  Context.t list -> Context.t -> unit

val clear_master_shared: Context.t -> Context.t -> unit

val merge_lib_file:
  Context.t ->
  Context.t ->
  Errors.ErrorSet.t * Error_suppressions.t

val merge_type: Reason.t -> Type.t list -> Type.t

module ContextOptimizer: sig
  val sig_context : Context.t list -> SigHash.t
end
