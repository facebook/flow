(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type get_ast_return = Loc.t Flow_ast.Comment.t list * (ALoc.t, ALoc.t) Flow_ast.Program.t

module Reqs : sig
  type t

  val empty : t

  val add_impl : string -> File_key.t -> Loc_collections.ALocSet.t -> t -> t

  val add_dep_impl : string -> File_key.t -> Context.sig_t * Loc_collections.ALocSet.t -> t -> t

  val add_unchecked : string -> File_key.t -> Loc_collections.ALocSet.t -> t -> t

  val add_res : string -> File_key.t -> Loc_collections.ALocSet.t -> t -> t

  val add_decl : string -> File_key.t -> Loc_collections.ALocSet.t * Modulename.t -> t -> t
end

val merge_component :
  metadata:Context.metadata ->
  lint_severities:Severity.severity LintSettings.t ->
  strict_mode:StrictModeSettings.t ->
  file_sigs:File_sig.With_ALoc.t Utils_js.FilenameMap.t ->
  get_ast_unsafe:(File_key.t -> get_ast_return) ->
  get_aloc_table_unsafe:(File_key.t -> ALoc.table) ->
  get_docblock_unsafe:(File_key.t -> Docblock.t) ->
  phase:Context.phase ->
  (* component *)
  File_key.t Nel.t ->
  (* requires *)
  Reqs.t ->
  (* dependency cxs *)
  Context.sig_t list ->
  (* master cx *)
  Context.sig_t ->
  (* cxs in component order, hd is merged leader, along with a coverage summary for each file *)
  (Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t) Nel.t

val merge_tvar : Context.t -> Reason.t -> Constraint.ident -> Type.t

module ContextOptimizer : sig
  val sig_context : Context.t -> string list -> Xx.hash
end
