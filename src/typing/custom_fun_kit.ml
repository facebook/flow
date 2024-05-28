(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_js_utils
open Reason
open Type

module type CUSTOM_FUN = sig
  val run :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    return_hint:Type.lazy_hint_t ->
    Reason.t ->
    Type.custom_fun_kind ->
    Type.targ list option ->
    Type.t list ->
    Type.t option ->
    Type.t ->
    unit
end

module Kit (Flow : Flow_common.S) : CUSTOM_FUN = struct
  include Flow

  let run cx trace ~use_op ~return_hint reason_op kind targs args spread_arg tout =
    match kind with
    | ReactCreateElement ->
      (match args with
      (* React.createElement(component) *)
      | [component] ->
        let config =
          let r = replace_desc_reason RReactProps reason_op in
          Obj_type.mk_with_proto cx r ~obj_kind:Exact ~frozen:true (ObjProtoT r)
        in
        rec_flow
          cx
          trace
          ( component,
            ReactKitT
              ( use_op,
                reason_op,
                React.CreateElement
                  {
                    targs;
                    config;
                    children = ([], None);
                    tout;
                    return_hint;
                    component;
                    record_monomorphized_result = false;
                  }
              )
          )
      (* React.createElement(component, config, ...children) *)
      | component :: config :: children ->
        rec_flow
          cx
          trace
          ( component,
            ReactKitT
              ( use_op,
                reason_op,
                React.CreateElement
                  {
                    targs;
                    config;
                    children = (children, spread_arg);
                    tout;
                    return_hint;
                    component;
                    record_monomorphized_result = false;
                  }
              )
          )
      (* React.createElement() *)
      | _ ->
        (* If we don't have the arguments we need, add an arity error. *)
        add_output cx (Error_message.EReactElementFunArity (reason_op, "createElement", 1)))
    | ObjectAssign
    | ObjectGetPrototypeOf
    | ObjectSetPrototypeOf
    | DebugPrint
    | DebugThrow
    | DebugSleep ->
      failwith "implemented elsewhere"
end
