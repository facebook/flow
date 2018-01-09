(**
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Reqs : sig
  type impl = File_key.t * string * Loc.t Nel.t * File_key.t
  type dep_impl = Context.t * string * Loc.t Nel.t * File_key.t
  type unchecked = string * Loc.t Nel.t * File_key.t
  type res = Loc.t Nel.t * string * File_key.t
  type decl = string * Loc.t Nel.t * Modulename.t * File_key.t
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
  lint_severities: Severity.severity LintSettings.t ->
  strict_mode: StrictModeSettings.t ->
  file_sigs: File_sig.t Utils_js.FilenameMap.t ->
  get_ast_unsafe: (File_key.t -> Loc.t Ast.program) ->
  get_docblock_unsafe: (File_key.t -> Docblock.t) ->
  ?do_gc: bool ->
  (* component *)
  File_key.t Nel.t ->
  (* requires *)
  Reqs.t ->
  (* dependency cxs *)
  Context.t list ->
  (* master cx *)
  Context.t ->
  (* cxs in component order, hd is merged leader *)
  Context.t Nel.t

val clear_master_shared: Context.t -> Context.t -> unit

val merge_lib_file:
  Context.t ->
  Context.t ->
  Errors.ErrorSet.t * Error_suppressions.t * ExactCover.lint_severity_cover

val merge_tvar: Context.t -> Reason.t -> Constraint.ident -> Type.t

module ContextOptimizer: sig
  val sig_context : Context.t -> string list -> Xx.hash
end
