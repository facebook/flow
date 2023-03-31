(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
module SpeculationKit = Speculation_kit.Make (Flow_js.FlowJs)

let flow_t cx reason ~upper_unresolved (l, u) =
  SpeculationKit.try_singleton_throw_on_failure
    cx
    Trace.dummy_trace
    ~upper_unresolved
    reason
    l
    (UseT (unknown_use, u))

(* Returns a list of concrete types after breaking up unions, maybe types, etc *)
let possible_concrete_types cx reason t =
  let id = Tvar.mk_no_wrap cx reason in
  Flow_js.flow cx (t, PreprocessKitT (reason, ConcretizeTypes (ConcretizeHintT id)));
  Flow_js_utils.possible_types cx id

let try_singleton_no_throws cx reason ~upper_unresolved t u =
  try
    SpeculationKit.try_singleton_throw_on_failure cx Trace.dummy_trace reason ~upper_unresolved t u;
    true
  with
  | Flow_js_utils.SpeculationSingletonError -> false

let resolved_lower_flow cx r (l, u) =
  match possible_concrete_types cx r l with
  | [] -> ()
  | [l] -> Flow_js.flow cx (l, u)
  | ls ->
    if
      not
        (Base.List.fold ls ~init:false ~f:(fun acc l ->
             let r = try_singleton_no_throws cx r ~upper_unresolved:true l u in
             acc || r
         )
        )
    then
      raise Flow_js_utils.SpeculationSingletonError

let resolved_lower_flow_t cx r (l, u) = resolved_lower_flow cx r (l, UseT (unknown_use, u))

let resolved_upper_flow_t cx r (l, u) =
  match possible_concrete_types cx r u with
  | [] -> ()
  | [u] -> Flow_js.flow_t cx (l, u)
  | us ->
    if
      not
        (Base.List.fold us ~init:false ~f:(fun acc u ->
             let r =
               try_singleton_no_throws cx r ~upper_unresolved:false l (UseT (unknown_use, u))
             in
             acc || r
         )
        )
    then
      raise Flow_js_utils.SpeculationSingletonError

let get_method_type cx t reason propref =
  Tvar.mk_where cx reason (fun prop_t ->
      let use_t = MethodT (unknown_use, reason, reason, propref, NoMethodAction, prop_t) in
      resolved_lower_flow cx reason (t, use_t)
  )

let get_method_type_no_throw cx t reason propref =
  try
    Tvar.mk_where cx reason (fun prop_t ->
        let use_t = MethodT (unknown_use, reason, reason, propref, NoMethodAction, prop_t) in
        resolved_lower_flow cx reason (t, use_t)
    )
  with
  | Flow_js_utils.SpeculationSingletonError -> AnyT.untyped reason
