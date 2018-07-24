(**
 * Copyright (c) 2014-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Reqs : sig
  type t
  val empty: t
  val add_impl: string -> File_key.t -> Utils_js.LocSet.t -> t -> t
  val add_dep_impl: string -> File_key.t -> (Context.sig_t * Utils_js.LocSet.t) -> t -> t
  val add_unchecked: string -> File_key.t -> Utils_js.LocSet.t -> t -> t
  val add_res: string -> File_key.t -> Utils_js.LocSet.t -> t -> t
  val add_decl:string -> File_key.t -> (Utils_js.LocSet.t * Modulename.t) -> t -> t
end

val merge_component_strict:
  metadata: Context.metadata ->
  lint_severities: Severity.severity LintSettings.t ->
  file_options: Files.options option ->
  strict_mode: StrictModeSettings.t ->
  file_sigs: File_sig.t Utils_js.FilenameMap.t ->
  get_ast_unsafe: (File_key.t -> (Loc.t, Loc.t) Ast.program) ->
  get_docblock_unsafe: (File_key.t -> Docblock.t) ->
  ?do_gc: bool ->
  (* component *)
  File_key.t Nel.t ->
  (* requires *)
  Reqs.t ->
  (* dependency cxs *)
  Context.sig_t list ->
  (* master cx *)
  Context.sig_t ->
  (* cxs in component order, hd is merged leader *)
  Context.t Nel.t

val merge_tvar: Context.t -> Reason.t -> Constraint.ident -> Type.t

module ContextOptimizer: sig
  val sig_context : Context.t -> string list -> Xx.hash
end
