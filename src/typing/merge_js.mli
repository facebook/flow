(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Reqs : sig
  type t

  val empty : t

  val add_dep_impl : string -> Context.sig_t * Loc_collections.ALocSet.t -> t -> t

  val add_unchecked : string -> Loc_collections.ALocSet.t -> t -> t

  val add_res : string -> Loc_collections.ALocSet.t -> t -> t

  val add_decl : string -> Loc_collections.ALocSet.t * Modulename.t -> t -> t
end

type options = {
  metadata: Context.metadata;
  lint_severities: Severity.severity LintSettings.t;
  strict_mode: StrictModeSettings.t;
}

val check_file :
  options:options ->
  File_key.t ->
  Reqs.t ->
  (* dependency cxs *)
  Context.sig_t list ->
  (* master cx *)
  Context.master_context ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  Loc.t Flow_ast.Comment.t list ->
  File_sig.With_ALoc.t ->
  Docblock.t ->
  ALoc.table Lazy.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

val sig_context : Context.t -> string Base.List.t -> Xx.hash

val optimize_builtins : Context.t -> unit

val post_merge_checks :
  Context.t ->
  Context.master_context ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t ->
  Context.metadata ->
  File_sig.With_ALoc.t ->
  unit

val get_lint_severities :
  Context.metadata ->
  StrictModeSettings.t ->
  Severity.severity LintSettings.t ->
  Severity.severity LintSettings.t
