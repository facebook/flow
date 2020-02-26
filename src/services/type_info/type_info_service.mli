(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val type_at_pos :
  cx:Context.t ->
  file_sig:File_sig.With_Loc.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.program ->
  expand_aliases:bool ->
  omit_targ_defaults:bool ->
  evaluate_type_destructors:bool ->
  max_depth:int ->
  verbose_normalizer:bool ->
  File_key.t ->
  int ->
  int ->
  (Loc.t * Ty.t option) * (string * Hh_json.json) list

val dump_types :
  expand_aliases:bool ->
  evaluate_type_destructors:bool ->
  Context.t ->
  File_sig.With_Loc.t ->
  (ALoc.t, ALoc.t * Type.t) Flow_ast.program ->
  (Loc.t * string) list

val coverage :
  cx:Context.t ->
  typed_ast:(ALoc.t, ALoc.t * Type.t) Flow_ast.program ->
  force:bool ->
  trust:bool ->
  File_key.t ->
  string ->
  (Loc.t * Coverage_response.expression_coverage) list

val suggest :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  File_key.t ->
  string ->
  ( Errors.ConcreteLocPrintableErrorSet.t
    * (* Typechecking errors *)
      Errors.ConcreteLocPrintableErrorSet.t
    * (* Typechecking warnings *)
      Errors.ConcreteLocPrintableErrorSet.t
    * (* Suggest-related warnings (normalization etc.) *)
      Replacement_printer.patch,
    (* Annotated program *)
    Errors.ConcreteLocPrintableErrorSet.t (* Parsing errors *) )
  result
  Lwt.t

val insert_type :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  file_key:File_key.t ->
  file_content:string ->
  target:Loc.t ->
  expand_aliases:bool ->
  omit_targ_defaults:bool ->
  location_is_strict:bool ->
  ambiguity_strategy:Autofix_options.ambiguity_strategy ->
  (Replacement_printer.patch, string) result Lwt.t

val autofix_exports :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  file_key:File_key.t ->
  file_content:string ->
  (Replacement_printer.patch * string list, string) result Lwt.t

val code_actions_at_loc :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  params:Lsp.CodeActionRequest.params ->
  file_key:File_key.t ->
  file_contents:string ->
  loc:Loc.t ->
  (Lsp.CodeAction.command_or_action list, string) result Lwt.t
