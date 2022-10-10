(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type poly_t = ALoc.t * Type.typeparam Nel.t * Type.t

type operation =
  | Call of Type.funcalltype
  | Constructor of Type.targ list option * Type.call_arg list
  | Jsx of {
      clone: bool;
      component: Type.t;
      config: Type.t;
      targs: Type.targ list option;
      children: Type.t list * Type.t option;
    }

type t = {
  lhs: Type.t;
  poly_t: poly_t;
  operation: Type.use_op * Reason.t * operation;
}

let of_call lhs poly_t use_op reason funcalltype =
  { lhs; poly_t; operation = (use_op, reason, Call funcalltype) }

let of_ctor lhs poly_t use_op reason_op targs args =
  { lhs; poly_t; operation = (use_op, reason_op, Constructor (targs, args)) }

let of_jsx lhs poly_t use_op reason_op clone ~component ~config ~targs children =
  {
    lhs;
    poly_t;
    operation = (use_op, reason_op, Jsx { clone; component; targs; config; children });
  }
