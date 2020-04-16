(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TypedRunner : sig
  val run :
    genv:ServerEnv.genv ->
    should_print_summary:bool ->
    info:bool ->
    f:((Loc.t, Loc.t) Flow_ast.program -> Codemod_context.Typed.t -> 'a) ->
    Options.t ->
    SSet.t ->
    ( Profiling_js.finished
    * ('a, ALoc.t * Error_message.internal_error) result Utils_js.FilenameMap.t )
    Lwt.t
end

module UntypedRunner : sig
  val run :
    genv:ServerEnv.genv ->
    should_print_summary:bool ->
    f:((Loc.t, Loc.t) Flow_ast.program -> Codemod_context.Untyped.t -> 'a) ->
    Options.t ->
    SSet.t ->
    (Profiling_js.finished * 'a option Utils_js.FilenameMap.t) Lwt.t
end
