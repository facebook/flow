(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Flow = Flow_js

module FlowJS : Type_annotation_sig.ConsGen = struct
  include Flow
  open Type
  open TypeUtil

  let map_on_resolved_type = Type_operation_utils.TvarUtils.map_on_resolved_type

  let specialize cx c use_op reason_op reason_tapp targs =
    let open Type in
    let open TypeUtil in
    let reason = reason_of_t c in
    let f c =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tvar ->
          Flow.flow cx (c, SpecializeT (use_op, reason_op, reason_tapp, false, targs, tvar))
      )
    in
    map_on_resolved_type cx reason c f

  let mixin cx reason i =
    let f i =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
          Flow.flow cx (i, Type.MixinT (reason, tout))
      )
    in
    map_on_resolved_type cx reason i f

  let obj_test_proto cx reason t =
    let f t =
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
          Flow.flow cx (t, ObjTestProtoT (reason, tout))
      )
    in
    map_on_resolved_type cx reason t f

  let get_prop cx use_op reason ?(op_reason = reason) name l =
    let f l =
      Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx op_reason (fun tout ->
          Flow.flow
            cx
            ( l,
              GetPropT
                {
                  use_op;
                  reason = op_reason;
                  id = None;
                  from_annot = false;
                  propref = mk_named_prop ~reason name;
                  tout;
                  hint = hint_unavailable;
                }
            )
      )
    in
    map_on_resolved_type cx op_reason l f

  let qualify_type cx use_op reason ~op_reason prop_name l =
    let f l =
      Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx op_reason (fun tout ->
          Flow.flow
            cx
            ( l,
              GetTypeFromNamespaceT
                { use_op; reason = op_reason; prop_ref = (reason, prop_name); tout }
            )
      )
    in
    map_on_resolved_type cx op_reason l f

  let mk_instance cx ?(type_t_kind = InstanceKind) instance_reason ?(use_desc = false) l =
    let f t =
      t
      |> Flow.singleton_concrete_type_for_inspection cx instance_reason
      |> Flow_js_utils.ValueToTypeReferenceTransform.run_on_concrete_type
           cx
           ~trace:Trace.dummy_trace
           ~use_op:unknown_use
           instance_reason
           type_t_kind
    in
    AnnotT (instance_reason, map_on_resolved_type cx instance_reason l f, use_desc)
end

module Annot : Type_annotation_sig.ConsGen = Annotation_inference.ConsGen
