(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc t =
  | ExpectedSort of Signature_builder_kind.Sort.t * string * 'loc
  | ExpectedAnnotation of 'loc * Expected_annotation_sort.t
  | UnexpectedObjectKey of 'loc * 'loc
  | UnexpectedArraySpread of 'loc * 'loc
  | UnexpectedArrayHole of 'loc
  | EmptyArray of 'loc
  | EmptyObject of 'loc
  | UnexpectedExpression of 'loc * Flow_ast_utils.ExpressionSort.t
  | SketchyToplevelDef of 'loc
  | UnsupportedPredicateExpression of 'loc
  | TODO of string * 'loc
[@@deriving show, iter, map]

let compare = Stdlib.compare
