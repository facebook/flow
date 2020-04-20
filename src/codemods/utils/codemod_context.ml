(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Typed = struct
  type t = {
    file: File_key.t;
    file_sig: File_sig.With_ALoc.t;
    metadata: Context.metadata;
    full_cx: Context.t;
    typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.program;
  }

  type error =
    | MissingTypeAnnotation
    | NormalizationError of Ty_normalizer.error

  let file ccx = ccx.file

  let ty_at_loc norm_opts ccx loc =
    let { full_cx; file; file_sig; typed_ast; _ } = ccx in
    let aloc = ALoc.of_loc loc in
    match Typed_ast_utils.find_exact_match_annotation typed_ast aloc with
    | None -> Error MissingTypeAnnotation
    | Some scheme ->
      let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~file_sig ~typed_ast in
      (match Ty_normalizer.from_scheme ~options:norm_opts ~genv scheme with
      | Ok ty -> Ok ty
      | Error e -> Error (NormalizationError e))

  let file_sig ccx = ccx.file_sig

  let metadata ccx = ccx.metadata

  let context ccx = ccx.full_cx

  let typed_ast ccx = ccx.typed_ast
end

module Untyped = struct
  type t = {
    file: File_key.t;
    file_sig: File_sig.With_Loc.t;
  }
end

module UntypedFlowInit = struct
  type t = {
    file: File_key.t;
    file_sig: File_sig.With_Loc.t;
    reader: State_reader.t;
  }
end
