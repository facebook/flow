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
    | (NominalRenders { renders_id; renders_super }, StructuralRenders t) ->
      (* Similar to the RendersT ~> UnionT case, this one is tricky. There are three ways to satisfy
       * this constraint:
       * 1. structural is a union containing a RendersT with a matching id
       * 2. mixed element is a subtype of u
       * 3. super is a subtype of u
       *
       * To perform this check, we preprocess structural to figure out if it has a RendersT with a
       * matching id. If not, we continue with React.MixedElement ~> u, and if that fails then
       * super ~> u
       *)
      let has_id_in_possible_types =
        let possible_types = Flow.possible_concrete_types_for_inspection cx reasonl t in
        possible_types
        |> List.exists (fun t ->
               match t with
               | DefT
                   (_, _, RendersT (NominalRenders { renders_id = component_id; renders_super = _ }))
                 when component_id = renders_id ->
                 true
               | _ -> false
           )
      in
      if not has_id_in_possible_types then
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
