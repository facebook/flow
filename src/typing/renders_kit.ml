(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason

module type INPUT = sig
  include Flow_common.BASE

  include Flow_common.BUILTINS

  include Flow_common.SUBTYPING

  include Flow_common.REACT
end

module Make (Flow : INPUT) = struct
  open Flow

  let reconstruct_render_type reason form = DefT (reason, bogus_trust (), RendersT form)

  let rec_renders cx trace ~use_op ((reasonl, l), (reasonu, u)) =
    match (l, u) with
    | ( NominalRenders { renders_id = id1; renders_super },
        NominalRenders { renders_id = id2; renders_super = _ }
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
    | (NominalRenders { renders_id = _; renders_super }, StructuralRenders t) ->
      if not (speculative_subtyping_succeeds cx (reconstruct_render_type reasonl l) t) then
        let mixed_element =
          get_builtin_type cx ~trace reasonl (OrdinaryName "React$MixedElement")
        in
        let u_type = reconstruct_render_type reasonu u in
        if not (speculative_subtyping_succeeds cx mixed_element u_type) then
          let super = reposition_reason cx ~trace reasonl ~use_desc:true renders_super in
          rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (super, u_type)
    | (StructuralRenders t, NominalRenders _) ->
      rec_flow_t
        cx
        trace
        ~use_op:(Frame (RendersCompatibility, use_op))
        (t, reconstruct_render_type reasonu u)
    | (StructuralRenders t, StructuralRenders _) ->
      rec_flow_t
        cx
        trace
        ~use_op:(Frame (RendersCompatibility, use_op))
        (t, reconstruct_render_type reasonu u)
end
