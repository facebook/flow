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

  let specialize cx c use_op reason_op reason_tapp targs =
    let open Type in
    let open TypeUtil in
    let reason = reason_of_t c in
    Tvar.mk_where cx reason (fun tvar ->
        Flow.flow cx (c, SpecializeT (use_op, reason_op, reason_tapp, false, targs, tvar))
    )

  let mixin cx reason i =
    Tvar.mk_where cx reason (fun tout -> Flow.flow cx (i, Type.MixinT (reason, tout)))

  let obj_test_proto cx reason t =
    Tvar.mk_where cx reason (fun tout -> Flow.flow cx (t, ObjTestProtoT (reason, tout)))

  let get_prop cx use_op reason ?(op_reason = reason) name l =
    Tvar.mk_no_wrap_where cx op_reason (fun tout ->
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

  let qualify_type cx use_op reason ~op_reason prop_name l =
    Tvar.mk_no_wrap_where cx op_reason (fun tout ->
        Flow.flow
          cx
          ( l,
            GetTypeFromNamespaceT
              { use_op; reason = op_reason; prop_ref = (reason, prop_name); tout }
          )
    )
end

module Annot : Type_annotation_sig.ConsGen = Annotation_inference.ConsGen
