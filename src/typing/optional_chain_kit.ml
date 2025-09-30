(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason

module Make (Flow : Flow_common.S) : sig
  val run :
    Context.t ->
    DepthTrace.t ->
    Type.t ->
    reason:reason ->
    lhs_reason:reason ->
    upper:Type.use_t ->
    voided_out_collector:TypeCollector.t option ->
    unit
end = struct
  module SpeculationKit = Speculation_kit.Make (Flow)

  let rec run_on_concretized cx trace l ~reason ~lhs_reason ~upper ~voided_out_collector =
    match l with
    | DefT (_, VoidT) ->
      Flow_js_utils.CalleeRecorder.add_callee_use cx Flow_js_utils.CalleeRecorder.Tast l upper;
      Context.mark_optional_chain cx (loc_of_reason reason) lhs_reason ~useful:true;
      Base.Option.iter voided_out_collector ~f:(fun c -> TypeCollector.add c l)
    | DefT (r, NullT) ->
      Flow_js_utils.CalleeRecorder.add_callee_use cx Flow_js_utils.CalleeRecorder.Tast l upper;
      let void =
        match desc_of_reason r with
        | RNull ->
          (* to avoid error messages like "null is incompatible with null",
             give VoidT that arise from `null` annotations a new description
             explaining why it is void and not null *)
          DefT (replace_desc_reason RVoidedNull r, VoidT)
        | _ -> DefT (r, VoidT)
      in
      Context.mark_optional_chain cx (loc_of_reason reason) lhs_reason ~useful:true;
      Base.Option.iter voided_out_collector ~f:(fun c -> TypeCollector.add c void)
    | IntersectionT (r, rep) ->
      let upper =
        match upper with
        | GetPropT { use_op; reason; id = Some _; from_annot; skip_optional; propref; tout; hint }
          ->
          GetPropT { use_op; reason; id = None; from_annot; skip_optional; propref; tout; hint }
        | TestPropT { use_op; reason; id = _; propref; tout; hint } ->
          GetPropT
            {
              use_op;
              reason;
              id = None;
              from_annot = false;
              skip_optional = false;
              propref;
              tout;
              hint;
            }
        | _ -> upper
      in
      (* We only call CalleeRecorder here for sig-help information. As far as
       * the typed AST is concerned when dealing with intersections we record
       * the specific branch that was selected. Therefore, we do not record
       * intersections when they hit a CallT constraint. The only time when an
       * intersection is allowed is when we have exhausted the branches of a
       * speculation job (this is a Flow error) and fall back to the
       * intersection as the type for the callee node. (This happens in
       * Default_resolver.) *)
      Flow_js_utils.CalleeRecorder.add_callee_use cx Flow_js_utils.CalleeRecorder.SigHelp l upper;
      SpeculationKit.try_custom
        cx
        ~no_match_error_loc:(loc_of_reason r)
        ~use_t:upper
        ~default_resolve:(fun () ->
          Default_resolve.default_resolve_touts ~flow:(Flow.flow_t cx) cx (loc_of_reason r) upper)
        (Base.List.map (InterRep.members rep) ~f:(fun t () ->
             run cx trace t ~reason ~lhs_reason ~upper ~voided_out_collector
         )
        )
    | _ ->
      Context.mark_optional_chain
        cx
        (loc_of_reason reason)
        lhs_reason
        ~useful:
          (match l with
          | AnyT (_, AnyError _) -> false
          | DefT (_, MixedT _)
          | AnyT _ ->
            true
          | _ -> false);
      Flow.flow cx (l, upper)

  and run cx trace lhs ~reason ~lhs_reason ~upper ~voided_out_collector =
    Flow.possible_concrete_types_for_optional_chain cx lhs_reason lhs
    |> Base.List.iter
         ~f:(run_on_concretized cx trace ~reason ~lhs_reason ~upper ~voided_out_collector)
end
