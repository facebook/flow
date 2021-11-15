(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val scan_for_suppressions :
  Context.t -> Severity.severity LintSettings.t -> Loc.t Flow_ast.Comment.t list -> unit

val add_require_tvars :
  unresolved_tvar:(Context.t -> Reason.t -> Type.ident) -> Context.t -> File_sig.With_ALoc.t -> unit

module Make_Inference (Env : Env_sig.S) : sig
  module Statement : Statement_sig.S with module Env := Env

  module Abnormal : Abnormal_sig.S with module Env := Env
end

module type S = sig
  module ImpExp : Import_export.S

  (* Lint suppressions are handled iff lint_severities is Some. *)
  val infer_ast :
    lint_severities:Severity.severity LintSettings.t ->
    Context.t ->
    File_key.t ->
    Loc.t Flow_ast.Comment.t list ->
    (ALoc.t, ALoc.t) Flow_ast.Program.t ->
    (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

  (* Lint suppressions are handled iff lint_severities is Some. *)
  val infer_lib_file :
    exclude_syms:NameUtils.Set.t ->
    lint_severities:Severity.severity LintSettings.t ->
    file_sig:File_sig.With_ALoc.t ->
    Context.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    Reason.name list
end

module Make (_ : Env_sig.S) : S

module NewEnvInference : S

module EnvInference : S

include S
