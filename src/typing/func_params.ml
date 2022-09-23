(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

module Make
    (CT : Func_class_sig_types.Config.S)
    (C : Config with module Types := CT)
    (T : Func_class_sig_types.Param.S with module Config := CT) :
  S with module Config_types := CT and module Config := C and module Types = T = struct
  module Types = T
  open T

  let empty reconstruct = { params_rev = []; rest = None; this_ = None; reconstruct }

  let add_param p x = { x with params_rev = p :: x.params_rev }

  let add_rest r x = { x with rest = Some r }

  let add_this t x = { x with this_ = Some t }

  let all_params_annotated { params_rev; rest; _ } =
    Base.List.for_all params_rev ~f:C.is_param_type_annotated
    && Base.Option.value_map rest ~default:true ~f:C.is_rest_type_annotated

  let value { params_rev; _ } =
    List.fold_left
      (fun acc p ->
        let t = C.param_type p in
        t :: acc)
      []
      params_rev

  let rest { rest; _ } = Base.Option.map ~f:C.rest_type rest

  let this { this_; _ } = Base.Option.map ~f:C.this_type this_

  let eval cx { params_rev; rest; this_; reconstruct } =
    let params = List.rev params_rev in
    let param_tasts_rev = List.rev_map (C.eval_param cx) params in
    let rest_tast = Base.Option.map ~f:(C.eval_rest cx) rest in
    let this_tast = Base.Option.map ~f:(C.eval_this cx) this_ in
    reconstruct (List.rev param_tasts_rev) rest_tast this_tast
end
