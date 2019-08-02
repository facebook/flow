(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* NOTE: This is a WIP and should not be used for anything yet *)

type 'loc error =
  { loc: 'loc; desc: Lints.property_assignment_kind }

(* The bulk of the definite instance property assignment analysis is performed
 * by this function. It takes the elements of a class body as input and returns
 * a map from property names to a list of errors that we should emit if that
 * property isn't voidable
 *)
val eval_property_assignment :
  (ALoc.t, ALoc.t) Flow_ast.Class.Body.element list ->
  ALoc.t error list
