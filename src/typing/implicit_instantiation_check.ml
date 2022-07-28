(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type poly_t = ALoc.t * Type.typeparam Nel.t * Type.t

type operation =
  | Call of Type.funcalltype
  | Constructor of Type.call_arg list
  | Jsx of {
      clone: bool;
      component: Type.t;
      config: Type.t;
      children: Type.t list * Type.t option;
    }

type t = {
  lhs: Type.t;
  poly_t: poly_t;
  operation: Type.use_op * Reason.t * operation;
}
