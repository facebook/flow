(**
 * Copyright (c) 2014-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module Reqs : sig
  type impl = Context.t * string * string * Context.t
  type dep_impl = Context.t * string * string * Context.t
  type unchecked = string * Loc.t * Context.t
  type res = string * Loc.t * string * Context.t
  type decl = string * Loc.t * Modulename.t * Context.t
  type t = {
    impls: impl list;
    dep_impls: dep_impl list;
    unchecked: unchecked list;
    res: res list;
    decls: decl list;
  }
  val empty: t
  val add_impl: impl -> t -> t
  val add_dep_impl: dep_impl -> t -> t
  val add_unchecked: unchecked -> t -> t
  val add_res: res -> t -> t
  val add_decl: decl -> t -> t
end

val merge_component_strict:
  Reqs.t ->
  (* component cxs *)
  Context.t list ->
  (* dependency cxs *)
  Context.t list ->
  (* master cx *)
  Context.t ->
  unit

val restore: Context.t ->
  Context.t list -> Context.t -> unit

val clear_master_shared: Context.t -> Context.t -> unit

val merge_lib_file:
  Context.t ->
  Context.t ->
  Errors.ErrorSet.t * Error_suppressions.t * LintSettingsMap.t

val lowers_of_tvar: Context.t -> Reason.t -> Constraint.ident -> Type.t

module ContextOptimizer: sig
  val sig_context : Context.t list -> SigHash.t
end
