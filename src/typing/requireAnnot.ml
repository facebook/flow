(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

let add_missing_annotation_error cx reason =
  Flow_js.add_output cx (Error_message.EMissingLocalAnnotation reason)

let hint_missing_annotation cx reason = function
  | Ast.Type.Missing _ -> add_missing_annotation_error cx reason
  | Ast.Type.Available _ -> ()

let require_annot_on_pattern cx pattern_reason pattern =
  if Context.enforce_local_inference_annotations cx then
    let open Ast.Pattern in
    match pattern with
    | Object { Object.annot; _ } -> hint_missing_annotation cx pattern_reason annot
    | Array { Array.annot; _ } -> hint_missing_annotation cx pattern_reason annot
    | Identifier { Identifier.annot; _ } -> hint_missing_annotation cx pattern_reason annot
    | Expression _ -> add_missing_annotation_error cx pattern_reason
