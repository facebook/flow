(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Typed = struct
  type t = {
    file: File_key.t;
    type_sig: Type_sig_collections.Locs.index Packed_type_sig.Module.t;
    file_sig: File_sig.t;
    metadata: Context.metadata;
    options: Options.t;
    cx: Context.t;
    typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t;
    docblock: Docblock.t;
    iteration: int;
    reader: Abstract_state_reader.t;
  }

  type error =
    | MissingTypeAnnotation
    | NormalizationError of Ty_normalizer.error

  let file ccx = ccx.file

  let ty_at_loc norm_opts ccx loc =
    let { cx; file; file_sig; typed_ast; _ } = ccx in
    let aloc = ALoc.of_loc loc in
    match Typed_ast_finder.find_exact_match_annotation cx typed_ast aloc with
    | None -> Error MissingTypeAnnotation
    | Some scheme ->
      let genv = Ty_normalizer_env.mk_genv ~cx ~file ~file_sig ~typed_ast_opt:(Some typed_ast) in
      (match Ty_normalizer_flow.from_scheme ~options:norm_opts ~genv scheme with
      | Ok ty -> Ok ty
      | Error e -> Error (NormalizationError e))

  let file_sig ccx = ccx.file_sig

  let metadata ccx = ccx.metadata

  let context ccx = ccx.cx

  let typed_ast ccx = ccx.typed_ast

  let lint_severities ccx =
    let { docblock; metadata; options; file; _ } = ccx in
    let metadata = Context.docblock_overrides docblock file metadata in
    let { Context.strict; strict_local; _ } = metadata in
    if strict || strict_local then
      StrictModeSettings.fold
        (fun lint_kind lint_severities ->
          LintSettings.set_value lint_kind (Severity.Err, None) lint_severities)
        (Options.strict_mode options)
        (Options.lint_severities options)
    else
      Options.lint_severities options

  let flowfixme_ast ~lint_severities ccx =
    let { options; _ } = ccx in
    let suppress_types = Options.suppress_types options in
    let exact_by_default = Options.exact_by_default options in
    Insert_type_utils.Builtins.flowfixme_ast ~lint_severities ~suppress_types ~exact_by_default
end

module Untyped = struct
  type t = { file: File_key.t }

  let file ccx = ccx.file
end

module UntypedFlowInit = struct
  type t = {
    file: File_key.t;
    reader: State_reader.t;
  }

  let file ccx = ccx.file
end
