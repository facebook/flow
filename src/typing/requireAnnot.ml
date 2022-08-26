(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Hint_api

let add_missing_annotation_error cx ~on_missing reason =
  on_missing ();
  Flow_js.add_output cx (Error_message.EMissingLocalAnnotation reason)

let hint_missing_annotation cx reason ~on_missing = function
  | Ast.Type.Missing _ -> add_missing_annotation_error cx ~on_missing reason
  | Ast.Type.Available _ -> ()

let should_require_annot cx =
  if Context.enforce_local_inference_annotations cx then
    let dirs = Context.local_inference_annotation_dirs cx in
    match dirs with
    | [] -> true
    | _ :: _ ->
      let filename = File_key.to_string @@ Context.file cx in
      let normalized_filename = Sys_utils.normalize_filename_dir_sep filename in
      List.exists (fun str -> Base.String.is_prefix ~prefix:str normalized_filename) dirs
  else
    false

let require_annot_on_pattern cx ~hint ~on_missing pattern_reason pattern =
  match hint with
  | Hint_None ->
    if should_require_annot cx then
      let open Ast.Pattern in
      (match pattern with
      | Object { Object.annot; _ } -> hint_missing_annotation cx ~on_missing pattern_reason annot
      | Array { Array.annot; _ } -> hint_missing_annotation cx ~on_missing pattern_reason annot
      | Identifier { Identifier.annot; _ } ->
        hint_missing_annotation cx ~on_missing pattern_reason annot
      | Expression _ -> add_missing_annotation_error cx ~on_missing pattern_reason)
  | Hint_t _
  | Hint_Decomp _
  | Hint_Placeholder ->
    ()
