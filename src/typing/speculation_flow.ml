(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
module SpeculationKit = Speculation_kit.Make (Flow_js.FlowJs)

let flow_t_unsafe cx reason ~upper_unresolved (l, u) =
  SpeculationKit.try_singleton_throw_on_failure
    cx
    Trace.dummy_trace
    ~upper_unresolved
    reason
    l
    (UseT (unknown_use, u))

let is_flow_successful cx reason ~upper_unresolved t u =
  match
    SpeculationKit.try_singleton_throw_on_failure cx Trace.dummy_trace reason ~upper_unresolved t u
  with
  | exception Flow_js_utils.SpeculationSingletonError -> false
  | () -> true

let resolved_lower_flow_unsafe cx r (l, u) =
  match Flow_js.possible_concrete_types_for_inspection cx r l with
  | [] -> ()
  | [l] -> Flow_js.flow cx (l, u)
  | ls ->
    if
      not
        (Base.List.fold ls ~init:false ~f:(fun acc l ->
             let r = is_flow_successful cx r ~upper_unresolved:true l u in
             acc || r
         )
        )
    then
      raise Flow_js_utils.SpeculationSingletonError

let resolved_lower_flow_t_unsafe cx r (l, u) =
  resolved_lower_flow_unsafe cx r (l, UseT (unknown_use, u))

let resolved_upper_flow_t_unsafe cx r (l, u) =
  match Flow_js.possible_concrete_types_for_inspection cx r u with
  | [] -> ()
  | [u] -> Flow_js.flow_t cx (l, u)
  | us ->
    if
      not
        (Base.List.fold us ~init:false ~f:(fun acc u ->
             let r = is_flow_successful cx r ~upper_unresolved:false l (UseT (unknown_use, u)) in
             acc || r
         )
        )
    then
      raise Flow_js_utils.SpeculationSingletonError

let get_method_type_unsafe cx t reason propref =
  Tvar.mk_where cx reason (fun prop_t ->
      let use_t = MethodT (unknown_use, reason, reason, propref, NoMethodAction prop_t) in
      resolved_lower_flow_unsafe cx reason (t, use_t)
  )

let get_method_type_opt cx t reason propref =
  match
    Tvar.mk_where cx reason (fun prop_t ->
        let use_t = MethodT (unknown_use, reason, reason, propref, NoMethodAction prop_t) in
        resolved_lower_flow_unsafe cx reason (t, use_t)
    )
  with
  | exception Flow_js_utils.SpeculationSingletonError -> None
  | t -> Some t
