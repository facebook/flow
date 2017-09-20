(**
 * Copyright (c) 2014-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

module Reqs : sig
  type impl = Loc.filename * string * string * Loc.filename
  type dep_impl = Context.t * string * string * Loc.filename
  type unchecked = string * Loc.t * Loc.filename
  type res = string * Loc.t * string * Loc.filename
  type decl = string * Loc.t * Modulename.t * Loc.filename
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
  metadata: Context.metadata ->
  lint_severities: Severity.severity LintSettings.t option ->
  file_sigs: File_sig.t Utils_js.FilenameMap.t ->
  get_ast_unsafe: (Loc.filename -> Loc.t Ast.program) ->
  get_docblock_unsafe: (Loc.filename -> Docblock.t) ->
  (* component *)
  Loc.filename list ->
  (* requires *)
  Reqs.t ->
  (* dependency cxs *)
  Context.t list ->
  (* master cx *)
  Context.t ->
  (* merged cx *)
  Context.t

val clear_master_shared: Context.t -> Context.t -> unit

val merge_lib_file:
  Context.t ->
  Context.t ->
  Errors.ErrorSet.t * Error_suppressions.t * ExactCover.lint_severity_cover

val merge_tvar: Context.t -> Reason.t -> Constraint.ident -> Type.t

module ContextOptimizer: sig
  val sig_context : Context.t -> string list -> SigHash.t
end
