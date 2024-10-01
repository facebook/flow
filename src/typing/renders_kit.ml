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

module type S = sig
  val rec_renders_to_renders :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    (Reason.reason * Type.canonical_renders_form) * (Reason.reason * Type.canonical_renders_form) ->
    unit

  val non_renders_to_renders :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    Type.t ->
    Reason.reason * Type.canonical_renders_form ->
    unit

  val try_synthesize_render_type :
    Context.t -> drop_renders_any:bool -> Type.t -> (Type.renders_variant * Type.t list) option
end

module Make (Flow : INPUT) : S = struct
  open Flow

  let reconstruct_render_type reason form = DefT (reason, RendersT form)

  let rec rec_renders_to_renders cx trace ~use_op ((reasonl, l), (reasonu, u)) =
    match (l, u) with
    | (InstrinsicRenders n1, InstrinsicRenders n2) ->
      if n1 = n2 then
        ()
      else
        Flow_js_utils.add_output
          cx
          (Error_message.EIncompatibleWithUseOp
             {
               reason_lower = reasonl;
               reason_upper = reasonu;
               use_op = Frame (RendersCompatibility, use_op);
               explanation = None;
             }
          )
    | (InstrinsicRenders _, StructuralRenders { renders_variant = _; renders_structural_type = t })
      ->
      if not (speculative_subtyping_succeeds cx (reconstruct_render_type reasonl l) t) then
        Flow_js_utils.add_output
          cx
          (Error_message.EIncompatibleWithUseOp
             {
               reason_lower = reasonl;
               reason_upper = reasonu;
               use_op = Frame (RendersCompatibility, use_op);
               explanation = None;
             }
          )
    | (InstrinsicRenders _, NominalRenders _)
    | (_, InstrinsicRenders _) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EIncompatibleWithUseOp
           {
             reason_lower = reasonl;
             reason_upper = reasonu;
             use_op = Frame (RendersCompatibility, use_op);
             explanation = None;
           }
        )
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
    | (DefaultRenders, StructuralRenders { renders_variant = _; renders_structural_type = t }) ->
      (* This is not necessary for correctness, since it will eventually fail at the end.
       * However, this is helpful for generating a more detailed error if t is a union. *)
      rec_flow_t
        cx
        trace
        ~use_op:(Frame (RendersCompatibility, use_op))
        (reconstruct_render_type reasonl l, t)
    | ( StructuralRenders
          { renders_variant = RendersMaybe | RendersStar; renders_structural_type = _ },
        NominalRenders _
      ) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EIncompatibleWithUseOp
           {
             reason_lower = reasonl;
             reason_upper = reasonu;
             use_op = Frame (RendersCompatibility, use_op);
             explanation = None;
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
      rec_renders_to_renders
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
      rec_renders_to_renders
        cx
        trace
        ~use_op
        ( ( reasonl,
            StructuralRenders { renders_variant = RendersMaybe; renders_structural_type = t }
          ),
          (reasonu, u)
        )
    | ( (InstrinsicRenders _ | NominalRenders _ | StructuralRenders _ | DefaultRenders),
        DefaultRenders
      ) ->
      ()
    | (DefaultRenders, _) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EIncompatibleWithUseOp
           {
             reason_lower = reasonl;
             reason_upper = reasonu;
             use_op = Frame (RendersCompatibility, use_op);
             explanation = None;
           }
        )

  let possibly_promoted_render_types_of_react_element_type cx (elem_reason, opq) =
    let on_concretized_types ts =
      Base.List.fold ts ~init:([], false) ~f:(fun (ts, has_failed) -> function
        | AnyT _ as t -> (t :: ts, has_failed)
        | DefT (_, RendersT _) as t -> (t :: ts, has_failed)
        | _ -> (ts, true)
      )
    in
    let concretize_component_renders_and_check component_t =
      get_builtin_typeapp cx elem_reason "React$ComponentRenders" [component_t]
      |> possible_concrete_types_for_inspection cx elem_reason
      |> on_concretized_types
    in
    match opq with
    | {
     super_t = Some (DefT (_, ObjT { props_tmap; _ }));
     opaque_type_args = (_, _, component_t, _) :: (_ as _targs);
     _;
    } ->
      (match Context.find_monomorphized_component cx props_tmap with
      | Some mono_component -> concretize_component_renders_and_check mono_component
      | None ->
        (* We only want to promote if this is actually a React of a component, otherwise we want
         * to flow the original object to the tout.
         *
         * We perform a speculative subtyping check and then use ComponentRenders to
         * extract the render type of the component. This type gets concretized, and we continue
         * with renders subtyping if we get a RendersT from ComponentRenders, otherwise we error,
         * as we've already checked for structural compatibility in subtyping kit. *)
        let top_abstract_component =
          let config = EmptyT.why elem_reason in
          let instance = ComponentInstanceAvailableAsInstanceType (MixedT.why elem_reason) in
          let renders = get_builtin_type cx elem_reason "React$Node" in
          DefT
            ( elem_reason,
              ReactAbstractComponentT { config; instance; renders; component_kind = Structural }
            )
        in
        if speculative_subtyping_succeeds cx component_t top_abstract_component then
          concretize_component_renders_and_check component_t
        else if
          speculative_subtyping_succeeds
            cx
            component_t
            (DefT (elem_reason, SingletonStrT (Reason.OrdinaryName "svg")))
        then
          ([DefT (elem_reason, RendersT (InstrinsicRenders "svg"))], false)
        else
          ([], true))
    | _ -> ([], true)

  let try_promote_render_type_from_react_element_type
      cx trace ~use_op (elem_reason, opq) (renders_r, upper_renders) =
    let (promoted_ts, has_failed) =
      possibly_promoted_render_types_of_react_element_type cx (elem_reason, opq)
    in
    let use_op = Frame (RendersCompatibility, use_op) in
    (* We did not successfully promote the React$Element and we have a RendersT on the RHS.
     * so this is an error *)
    if has_failed then
      Flow_js_utils.add_output
        cx
        (Error_message.EIncompatibleWithUseOp
           { reason_lower = elem_reason; reason_upper = renders_r; use_op; explanation = None }
        );
    Base.List.iter promoted_ts ~f:(fun l ->
        rec_flow_t
          cx
          trace
          ~use_op
          ( reposition_reason cx ~trace elem_reason ~use_desc:true l,
            DefT (renders_r, RendersT upper_renders)
          )
    )

  let non_renders_to_renders cx trace ~use_op l (renders_r, upper_renders) =
    match (l, upper_renders) with
    | ( DefT (_, (NullT | VoidT | BoolT (Some false))),
        ( StructuralRenders
            { renders_variant = RendersMaybe | RendersStar; renders_structural_type = _ }
        | DefaultRenders )
      ) ->
      ()
    | ( DefT (_, ArrT (ArrayAT { elem_t = t; _ } | TupleAT { elem_t = t; _ } | ROArrayAT (t, _))),
        ( StructuralRenders { renders_variant = RendersStar; renders_structural_type = _ }
        | DefaultRenders )
      ) ->
      rec_flow_t cx trace ~use_op (t, reconstruct_render_type renders_r upper_renders)
    (* Try to do structural subtyping. If that fails promote to a render type *)
    | (OpaqueT (reason_opaque, ({ opaque_id; _ } as opq)), (InstrinsicRenders _ | NominalRenders _))
      when Some opaque_id = Flow_js_utils.builtin_react_element_opaque_id cx ->
      try_promote_render_type_from_react_element_type
        cx
        trace
        ~use_op
        (reason_opaque, opq)
        (renders_r, upper_renders)
    | ( OpaqueT (reason_opaque, ({ opaque_id; _ } as opq)),
        StructuralRenders { renders_variant = _; renders_structural_type = t }
      )
      when Some opaque_id = Flow_js_utils.builtin_react_element_opaque_id cx ->
      if not (speculative_subtyping_succeeds cx l t) then
        try_promote_render_type_from_react_element_type
          cx
          trace
          ~use_op
          (reason_opaque, opq)
          (renders_r, upper_renders)
    (* given x <: y, x <: renders y. The only case in which this is not true is when `x` is a component reference,
     * Foo <: renders Foo fails in that case. Since the RHS is in its canonical form we know that we're safe
     * to Flow the LHS to the structural type on the RHS *)
    | (l, StructuralRenders { renders_variant = _; renders_structural_type = t }) ->
      rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (l, t)
    | (l, DefaultRenders) ->
      rec_flow_t
        cx
        trace
        ~use_op:(Frame (RendersCompatibility, use_op))
        (l, get_builtin_type cx renders_r ~use_desc:true "React$Node")
    | (l, _) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EIncompatibleWithUseOp
           {
             reason_lower = TypeUtil.reason_of_t l;
             reason_upper = renders_r;
             use_op = Frame (RendersCompatibility, use_op);
             explanation = None;
           }
        )

  type render_type_synthesis_state =
    | IntermediateSynthesisState of {
        normalized_render_type_collector: TypeCollector.t;
        renders_variant: renders_variant;
      }
      (* If we encounter anything that we can't turn into render type.
       * e.g. arbitrary stuff in React.Node *)
    | FailedSynthesisState

  let try_synthesize_render_type =
    let merge_renders_variant = function
      | (RendersNormal, RendersNormal) -> RendersNormal
      | (RendersNormal, RendersMaybe) -> RendersMaybe
      | (RendersNormal, RendersStar) -> RendersStar
      | (RendersMaybe, RendersNormal) -> RendersMaybe
      | (RendersStar, RendersNormal) -> RendersStar
      | (RendersMaybe, RendersMaybe) -> RendersMaybe
      | (RendersMaybe, RendersStar) -> RendersStar
      | (RendersStar, RendersMaybe) -> RendersStar
      | (RendersStar, RendersStar) -> RendersStar
    in
    let rec on_concretized_react_node_types cx ~drop_renders_any ~state =
      let f state t =
        match state with
        | FailedSynthesisState -> FailedSynthesisState
        | IntermediateSynthesisState { normalized_render_type_collector; renders_variant } ->
          (match t with
          | AnyT _ ->
            if not drop_renders_any then TypeCollector.add normalized_render_type_collector t;
            state
          | DefT (_, RendersT renders) ->
            (match renders with
            | InstrinsicRenders _
            | NominalRenders _ ->
              TypeCollector.add normalized_render_type_collector t;
              state
            | DefaultRenders -> FailedSynthesisState
            | StructuralRenders { renders_variant = renders_variant'; renders_structural_type } ->
              on_concretized_react_node_types
                cx
                (possible_concrete_types_for_inspection
                   cx
                   (TypeUtil.reason_of_t renders_structural_type)
                   renders_structural_type
                )
                ~drop_renders_any
                ~state:
                  (IntermediateSynthesisState
                     {
                       normalized_render_type_collector;
                       renders_variant = merge_renders_variant (renders_variant, renders_variant');
                     }
                  ))
          | OpaqueT (elem_reason, ({ opaque_id; _ } as opq))
            when Some opaque_id = Flow_js_utils.builtin_react_element_opaque_id cx ->
            let (ts, has_failed) =
              possibly_promoted_render_types_of_react_element_type cx (elem_reason, opq)
            in
            if has_failed then
              FailedSynthesisState
            else
              on_concretized_react_node_types cx ~drop_renders_any ~state ts
          | DefT (_, (NullT | VoidT | BoolT (Some false))) ->
            let renders_variant = merge_renders_variant (renders_variant, RendersMaybe) in
            IntermediateSynthesisState { normalized_render_type_collector; renders_variant }
          | DefT (_, ArrT (ArrayAT { elem_t = t; _ } | TupleAT { elem_t = t; _ } | ROArrayAT (t, _)))
            ->
            on_concretized_react_node_types
              cx
              (possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t)
              ~drop_renders_any
              ~state:
                (IntermediateSynthesisState
                   {
                     normalized_render_type_collector;
                     renders_variant = merge_renders_variant (renders_variant, RendersStar);
                   }
                )
          | _ -> failwith "")
      in
      Base.List.fold ~init:state ~f
    in
    fun cx ~drop_renders_any t ->
      let state =
        IntermediateSynthesisState
          {
            normalized_render_type_collector = TypeCollector.create ();
            renders_variant = RendersNormal;
          }
      in
      match
        on_concretized_react_node_types
          cx
          (possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t)
          ~drop_renders_any
          ~state
      with
      | FailedSynthesisState -> None
      | IntermediateSynthesisState { normalized_render_type_collector; renders_variant } ->
        Some (renders_variant, TypeCollector.collect normalized_render_type_collector)
end
