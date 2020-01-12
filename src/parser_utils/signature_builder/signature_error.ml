(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc t =
  | ExpectedSort of Signature_builder_kind.Sort.t * string * 'loc
  | ExpectedAnnotation of 'loc * Expected_annotation_sort.t
  | InvalidTypeParamUse of 'loc
  | UnexpectedObjectKey of 'loc * 'loc
  | UnexpectedObjectSpread of 'loc * 'loc
  | UnexpectedArraySpread of 'loc * 'loc
  | UnexpectedArrayHole of 'loc
  | EmptyArray of 'loc
  | EmptyObject of 'loc
  | UnexpectedExpression of 'loc * Flow_ast_utils.ExpressionSort.t
  | SketchyToplevelDef of 'loc
  | UnsupportedPredicateExpression of 'loc
  | TODO of string * 'loc

let compare = Pervasives.compare

let map_locs ~f = function
  | ExpectedSort (sort, str, loc) -> ExpectedSort (sort, str, f loc)
  | ExpectedAnnotation (loc, sort) -> ExpectedAnnotation (f loc, sort)
  | InvalidTypeParamUse loc -> InvalidTypeParamUse (f loc)
  | UnexpectedObjectKey (loc, key_loc) -> UnexpectedObjectKey (f loc, f key_loc)
  | UnexpectedObjectSpread (loc, spread_loc) -> UnexpectedObjectSpread (f loc, f spread_loc)
  | UnexpectedArraySpread (loc, spread_loc) -> UnexpectedArraySpread (f loc, f spread_loc)
  | UnexpectedArrayHole loc -> UnexpectedArrayHole (f loc)
  | EmptyArray loc -> EmptyArray (f loc)
  | EmptyObject loc -> EmptyObject (f loc)
  | UnexpectedExpression (loc, sort) -> UnexpectedExpression (f loc, sort)
  | SketchyToplevelDef loc -> SketchyToplevelDef (f loc)
  | UnsupportedPredicateExpression loc -> UnsupportedPredicateExpression (f loc)
  | TODO (str, loc) -> TODO (str, f loc)
