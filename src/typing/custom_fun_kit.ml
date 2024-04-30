(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_js_utils
open Reason
open Type
open TypeUtil

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
                React.CreateElement0
                  { clone = false; targs; config; children = ([], None); tout; return_hint }
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
                React.CreateElement0
                  {
                    clone = false;
                    targs;
                    config;
                    children = (children, spread_arg);
                    tout;
                    return_hint;
                  }
              )
          )
      (* React.createElement() *)
      | _ ->
        (* If we don't have the arguments we need, add an arity error. *)
        add_output cx (Error_message.EReactElementFunArity (reason_op, "createElement", 1)))
    | ReactCloneElement ->
      (match args with
      (* React.cloneElement(element) *)
      | [element] ->
        let f elt =
          (* Create the expected type for our element with a fresh tvar in the
           * component position. *)
          let expected_element =
            get_builtin_typeapp cx (reason_of_t element) "React$Element" [Tvar.mk cx reason_op]
          in
          (* Flow the element arg to our expected element. *)
          rec_flow_t ~use_op cx trace (elt, expected_element);
          expected_element
        in
        let expected_element =
          match Flow.singleton_concrete_type_for_inspection cx (reason_of_t element) element with
          | UnionT (reason, rep) -> UnionT (reason, UnionRep.ident_map f rep)
          | _ -> f element
        in
        (* Flow our expected element to the return type. *)
        rec_flow_t ~use_op:unknown_use cx trace (expected_element, tout)
      (* React.cloneElement(element, config, ...children) *)
      | element :: config :: children ->
        let f element =
          (* Create a tvar for our component. *)
          Tvar.mk_where cx reason_op (fun component ->
              let expected_element = get_builtin_typeapp cx reason_op "React$Element" [component] in
              (* Flow the element arg to the element type we expect. *)
              rec_flow_t ~use_op cx trace (element, expected_element)
          )
        in
        let component =
          match Flow.singleton_concrete_type_for_inspection cx (reason_of_t element) element with
          | UnionT (reason, rep) -> UnionT (reason, UnionRep.ident_map f rep)
          | _ -> f element
        in
        (* Create a React element using the config and children. *)
        rec_flow
          cx
          trace
          ( component,
            ReactKitT
              ( use_op,
                reason_op,
                React.CreateElement0
                  {
                    clone = true;
                    targs;
                    config;
                    children = (children, spread_arg);
                    tout;
                    return_hint;
                  }
              )
          )
      (* React.cloneElement() *)
      | _ ->
        (* If we don't have the arguments we need, add an arity error. *)
        add_output cx (Error_message.EReactElementFunArity (reason_op, "cloneElement", 1)))
    | ObjectAssign
    | ObjectGetPrototypeOf
    | ObjectSetPrototypeOf
    | DebugPrint
    | DebugThrow
    | DebugSleep ->
      failwith "implemented elsewhere"
end
