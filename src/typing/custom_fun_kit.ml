(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil
module FlowError = Flow_error

module type CUSTOM_FUN = sig
  val run :
    Context.t ->
    Trace.t ->
    use_op:Type.use_op ->
    Reason.t ->
    Type.custom_fun_kind ->
    Type.t list ->
    Type.t option ->
    Type.t ->
    unit
end

module Kit (Flow : Flow_common.S) = struct
  include Flow

  (* Creates the appropriate constraints for the compose() function and its
   * reversed variant. *)
  let rec run_compose cx trace ~use_op reason_op reverse fns spread_fn tin tout =
    match (reverse, fns, spread_fn) with
    (* Call the tail functions in our array first and call our head function
     * last after that. *)
    | (false, fn :: fns, _) ->
      let reason = replace_desc_reason (RCustom "compose intermediate value") (reason_of_t fn) in
      let tvar =
        Tvar.mk_no_wrap_where cx reason (fun tvar ->
            run_compose cx trace ~use_op reason_op reverse fns spread_fn tin tvar)
      in
      rec_flow
        cx
        trace
        (fn, CallT (use_op, reason, mk_functioncalltype reason_op None [Arg tvar] tout))
    (* If the compose function is reversed then we want to call the tail
     * functions in our array after we call the head function. *)
    | (true, fn :: fns, _) ->
      let reason = replace_desc_reason (RCustom "compose intermediate value") (reason_of_t fn) in
      let tvar = Tvar.mk_no_wrap cx reason in
      rec_flow
        cx
        trace
        ( fn,
          CallT (use_op, reason, mk_functioncalltype reason_op None [Arg (OpenT tin)] (reason, tvar))
        );
      run_compose cx trace ~use_op reason_op reverse fns spread_fn (reason, tvar) tout
    (* If there are no functions and no spread function then we are an identity
     * function. *)
    | (_, [], None) -> rec_flow_t ~use_op:unknown_use cx trace (OpenT tin, OpenT tout)
    (* Correctly implementing spreads of unknown arity for the compose function
     * is a little tricky. Let's look at a couple of cases.
     *
     *     const fn = (x: number): string => x.toString();
     *     declare var fns: Array<typeof fn>;
     *     const x = 42;
     *     compose(...fns)(x);
     *
     * This would be invalid. We could have 0 or 1 fn in our fns array, but 2 fn
     * would be wrong because string is incompatible with number. It breaks down
     * as such:
     *
     * 1. x = 42
     * 2. fn(x) = '42'
     * 3. fn(fn(x)) is an error because '42' is not a number.
     *
     * To get an error in this case we would only need to call the spread
     * argument twice. Now let's look at a case where things get recursive:
     *
     *     type Fn = <O>(O) => $PropertyType<O, 'p'>;
     *     declare var fns: Array<Fn>;
     *     const x = { p: { p: 42 } };
     *     compose(...fns)(x);
     *
     * 1. x = { p: { p: 42 } }
     * 2. fn(x) = { p: 42 }
     * 3. fn(fn(x)) = 42
     * 4. fn(fn(fn(x))) throws an error because the p property is not in 42.
     *
     * Here we would need to call fn 3 times before getting an error. Now
     * consider:
     *
     *     type Fn = <O>(O) => $PropertyType<O, 'p'>;
     *     declare var fns: Array<Fn>;
     *     type X = { p: X };
     *     declare var x: X;
     *     compose(...fns)(x);
     *
     * This is valid.
     *
     * To implement spreads in compose functions we first add a constraint based
     * on tin and tout assuming that the spread is empty. Then we emit recursive
     * constraints:
     *
     *     spread_fn(tin) ~> tout
     *     spread_fn(tout) ~> tin
     *
     * The implementation of Flow should be able to terminate these recursive
     * constraints. If it doesn't then we have a bug. *)
    | (_, [], Some spread_fn) ->
      run_compose cx trace ~use_op reason_op reverse [] None tin tout;
      run_compose cx trace ~use_op reason_op reverse [spread_fn] None tin tout;
      run_compose cx trace ~use_op reason_op reverse [spread_fn] None tout tin

  let run cx trace ~use_op reason_op kind args spread_arg tout =
    match kind with
    | Compose reverse ->
      (* Drop the specific argument reasons since run_compose will emit CallTs
       * with completely unrelated argument reasons. *)
      let use_op =
        match use_op with
        | Op (FunCall { op; fn; args = _; local }) -> Op (FunCall { op; fn; args = []; local })
        | Op (FunCallMethod { op; fn; prop; args = _; local }) ->
          Op (FunCallMethod { op; fn; prop; args = []; local })
        | _ -> use_op
      in
      let tin = (reason_op, Tvar.mk_no_wrap cx reason_op) in
      let tvar = (reason_op, Tvar.mk_no_wrap cx reason_op) in
      run_compose cx trace ~use_op reason_op reverse args spread_arg tin tvar;
      let funt =
        FunT
          ( dummy_static reason_op,
            dummy_prototype,
            mk_functiontype
              reason_op
              [OpenT tin]
              ~rest_param:None
              ~def_reason:reason_op
              (OpenT tvar) )
      in
      rec_flow_t ~use_op:unknown_use cx trace (DefT (reason_op, bogus_trust (), funt), tout)
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
            ReactKitT (use_op, reason_op, React.CreateElement0 (false, config, ([], None), tout)) )
      (* React.createElement(component, config, ...children) *)
      | component :: config :: children ->
        rec_flow
          cx
          trace
          ( component,
            ReactKitT
              (use_op, reason_op, React.CreateElement0 (false, config, (children, spread_arg), tout))
          )
      (* React.createElement() *)
      | _ ->
        (* If we don't have the arguments we need, add an arity error. *)
        add_output cx ~trace (Error_message.EReactElementFunArity (reason_op, "createElement", 1)))
    | ReactCloneElement ->
      (match args with
      (* React.cloneElement(element) *)
      | [element] ->
        (* Create the expected type for our element with a fresh tvar in the
         * component position. *)
        let expected_element =
          get_builtin_typeapp cx ~trace (reason_of_t element) "React$Element" [Tvar.mk cx reason_op]
        in
        (* Flow the element arg to our expected element. *)
        rec_flow_t ~use_op:unknown_use cx trace (element, expected_element);

        (* Flow our expected element to the return type. *)
        rec_flow_t ~use_op:unknown_use cx trace (expected_element, tout)
      (* React.cloneElement(element, config, ...children) *)
      | element :: config :: children ->
        (* Create a tvar for our component. *)
        let component = Tvar.mk cx reason_op in
        (* Flow the element arg to the element type we expect. *)
        rec_flow_t
          ~use_op:unknown_use
          cx
          trace
          (element, get_builtin_typeapp cx ~trace reason_op "React$Element" [component]);

        (* Create a React element using the config and children. *)
        rec_flow
          cx
          trace
          ( component,
            ReactKitT
              (use_op, reason_op, React.CreateElement0 (true, config, (children, spread_arg), tout))
          )
      (* React.cloneElement() *)
      | _ ->
        (* If we don't have the arguments we need, add an arity error. *)
        add_output cx ~trace (Error_message.EReactElementFunArity (reason_op, "cloneElement", 1)))
    | ReactElementFactory component ->
      (match args with
      (* React.createFactory(component)() *)
      | [] ->
        let config =
          let r = replace_desc_reason RReactProps reason_op in
          Obj_type.mk_with_proto cx r ~obj_kind:Exact ~frozen:true (ObjProtoT r)
        in
        rec_flow
          cx
          trace
          ( component,
            ReactKitT (use_op, reason_op, React.CreateElement0 (false, config, ([], None), tout)) )
      (* React.createFactory(component)(config, ...children) *)
      | config :: children ->
        rec_flow
          cx
          trace
          ( component,
            ReactKitT
              (use_op, reason_op, React.CreateElement0 (false, config, (children, spread_arg), tout))
          ))
    | ObjectAssign
    | ObjectGetPrototypeOf
    | ObjectSetPrototypeOf
    | ReactPropType _
    | ReactCreateClass
    | Idx
    | TypeAssertIs
    | TypeAssertThrows
    | TypeAssertWraps
    | DebugPrint
    | DebugThrow
    | DebugSleep ->
      failwith "implemented elsewhere"
end
