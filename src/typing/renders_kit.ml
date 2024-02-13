(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

module type INPUT = sig
  include Flow_common.BASE

  include Flow_common.BUILTINS

  include Flow_common.SUBTYPING

  include Flow_common.REACT
end

module Make (Flow : INPUT) = struct
  open Flow

  let reconstruct_render_type reason form = DefT (reason, RendersT form)

  let rec rec_renders cx trace ~use_op ((reasonl, l), (reasonu, u)) =
    match (l, u) with
    | ( NominalRenders { renders_id = id1; renders_name = _; renders_super },
        NominalRenders { renders_id = id2; renders_name = _; renders_super = _ }
      ) ->
      if ALoc.equal_id id1 id2 then
        ()
      else
        (* We reposition the super using l's reason for better error messages *)
        let super = reposition_reason cx ~trace reasonl ~use_desc:true renders_super in
        rec_flow_t
          cx
          trace
          ~use_op:(Frame (RendersCompatibility, use_op))
          (super, reconstruct_render_type reasonu u)
    | ( NominalRenders { renders_id = _; renders_name = _; renders_super },
        StructuralRenders
          {
            renders_variant = RendersNormal | RendersMaybe | RendersStar;
            renders_structural_type = t;
          }
      ) ->
      if not (speculative_subtyping_succeeds cx (reconstruct_render_type reasonl l) t) then
        let u_type = reconstruct_render_type reasonu u in
        let super = reposition_reason cx ~trace reasonl ~use_desc:true renders_super in
        rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (super, u_type)
    | ( StructuralRenders
          { renders_variant = RendersMaybe | RendersStar; renders_structural_type = _ },
        NominalRenders _
      ) ->
      Flow_js_utils.add_output
        cx
        ~trace
        (Error_message.EIncompatibleWithUseOp
           {
             reason_lower = reasonl;
             reason_upper = reasonu;
             use_op = Frame (RendersCompatibility, use_op);
           }
        )
    | (StructuralRenders { renders_variant = RendersNormal; renders_structural_type }, _) ->
      rec_flow_t cx trace ~use_op (renders_structural_type, reconstruct_render_type reasonu u)
    | ( StructuralRenders { renders_variant = RendersMaybe; renders_structural_type },
        StructuralRenders { renders_variant; renders_structural_type = _ }
      ) ->
      (match renders_variant with
      | RendersNormal ->
        let u_type = reconstruct_render_type reasonu u in
        let reason = Reason.(replace_desc_reason RRendersNothing reasonl) in
        let null_t = DefT (reason, NullT) in
        rec_flow_t cx trace ~use_op (null_t, u_type);
        let void_t = DefT (reason, VoidT) in
        rec_flow_t cx trace ~use_op (void_t, u_type);
        let false_t = DefT (reason, BoolT (Some false)) in
        rec_flow_t cx trace ~use_op (false_t, u_type)
      | RendersMaybe -> ()
      | RendersStar -> ());
      rec_renders
        cx
        trace
        ~use_op
        ( (reasonl, StructuralRenders { renders_variant = RendersNormal; renders_structural_type }),
          (reasonu, u)
        )
    | ( StructuralRenders { renders_variant = RendersStar; renders_structural_type = t },
        StructuralRenders { renders_variant; renders_structural_type = _ }
      ) ->
      (match renders_variant with
      | RendersNormal
      | RendersMaybe ->
        let renders_star = reconstruct_render_type reasonl l in
        let roa = Flow.get_builtin_typeapp cx reasonl "$ReadOnlyArray" [renders_star] in
        rec_flow_t cx trace ~use_op (roa, reconstruct_render_type reasonu u)
      | RendersStar -> ());
      rec_renders
        cx
        trace
        ~use_op
        ( ( reasonl,
            StructuralRenders { renders_variant = RendersMaybe; renders_structural_type = t }
          ),
          (reasonu, u)
        )
end
