(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'loc t =
  | ExpectedAnnotation of 'loc * Expected_annotation_sort.t
  | UnexpectedObjectKey of 'loc * 'loc
  | UnexpectedArraySpread of 'loc * 'loc
  | UnexpectedArrayHole of 'loc
  | EmptyArray of 'loc
  | EmptyObject of 'loc
  | UnexpectedExpression of 'loc * Flow_ast_utils.ExpressionSort.t
[@@deriving show, iter, map]

type 'loc binding_validation_t =
  | ModuleOverride of {
      name: string;
      override_binding_loc: 'loc;
      existing_binding_loc: 'loc;
    }
  | NameOverride of {
      name: string;
      override_binding_loc: 'loc;
      existing_binding_loc: 'loc;
    }
  | NamespacedNameAlreadyBound of {
      name: string;
      invalid_binding_loc: 'loc;
      existing_binding_loc: 'loc;
    }
[@@deriving show, iter, map]

let compare = Stdlib.compare
