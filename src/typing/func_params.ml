(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines a small data structure that stores function parameters
   before substitution. This is used as part of Func_sig (and Class_sig) to hold
   constraints at bay until substitution can occur.

   Function params serve two purposes: On one hand, they describe the arguments
   that a function expects. On the other, the bindings that exist within the
   body of a function. These may not be the same due to default values and
   destructuring. *)

include Func_params_intf

module Make (C: Config) = struct
  type 'T ast = 'T C.ast
  type 'T param_ast = 'T C.param_ast
  type 'T rest_ast = 'T C.rest_ast

  type param = C.param
  type rest = C.rest

  type reconstruct =
    (ALoc.t * Type.t) param_ast list ->
    (ALoc.t * Type.t) rest_ast option ->
    (ALoc.t * Type.t) ast option

  type t = {
    params_rev: param list;
    rest: rest option;
    reconstruct: reconstruct;
  }

  let empty reconstruct = {
    params_rev = [];
    rest = None;
    reconstruct;
  }

  let add_param p x =
    { x with params_rev = p::x.params_rev }

  let add_rest r x =
    { x with rest = Some r }

  let value {params_rev; _} =
    List.fold_left (fun acc p ->
      let t = C.param_type p in
      t::acc
    ) [] params_rev

  let rest {rest; _} =
    Option.map ~f:C.rest_type rest

  let subst cx map { params_rev; rest; reconstruct } = {
    params_rev = Core_list.map ~f:(C.subst_param cx map) params_rev;
    rest = Option.map ~f:(C.subst_rest cx map) rest;
    reconstruct;
  }

  let eval cx { params_rev; rest; reconstruct } =
    let params = List.rev params_rev in
    let param_tasts_rev = List.rev_map (C.eval_param cx) params in
    let rest_tast = Option.map ~f:(C.eval_rest cx) rest in
    reconstruct (List.rev param_tasts_rev) rest_tast
end
