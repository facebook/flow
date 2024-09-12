(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil
open React

module type REACT = sig
  val run :
    Context.t -> Type.DepthTrace.t -> use_op:use_op -> reason -> Type.t -> Type.React.tool -> unit

  val component_class : Context.t -> Reason.reason -> Type.t -> Type.t

  val subtype_class_component_render :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    Type.t ->
    reason_op:reason ->
    Type.t ->
    unit

  val get_config :
    Context.t ->
    Type.DepthTrace.t ->
    Type.t ->
    use_op:use_op ->
    reason_op:reason ->
    Type.React.tool ->
    Polarity.t ->
    Type.t ->
    unit

  val err_incompatible : Context.t -> use_op:use_op -> reason -> Type.React.tool -> unit
end

module Kit (Flow : Flow_common.S) : REACT = struct
  include Flow

  let err_incompatible cx ~use_op reason tool =
    let err =
      match tool with
      | GetProps _
      | GetConfig _
      | GetRef _
      | CreateElement _
      | ConfigCheck _ ->
        Error_message.ENotAReactComponent { reason; use_op }
      | GetConfigType _ -> Error_message.EInvalidReactConfigType { reason; use_op }
    in
    Flow_js_utils.add_output cx err

  let component_class cx reason props =
    DefT
      ( reason,
        ClassT (Flow.get_builtin_typeapp cx reason "React$Component" [props; Tvar.mk cx reason])
      )

  let get_intrinsic cx trace component ~reason_op artifact literal prop =
    let reason = reason_of_t component in
    (* Get the internal $JSXIntrinsics map. *)
    let intrinsics =
      let reason = mk_reason (RType (OrdinaryName "$JSXIntrinsics")) (loc_of_t component) in
      get_builtin_type cx reason "$JSXIntrinsics"
    in
    (* Create a use_op for the upcoming operations. *)
    let use_op =
      Op
        (ReactGetIntrinsic
           {
             literal =
               (match literal with
               | Literal (_, name) -> replace_desc_reason (RIdentifier name) reason
               | _ -> reason);
           }
        )
    in
    (* GetPropT with a non-literal when there is not a dictionary will propagate
     * any. Run the HasOwnPropT check to give the user an error if they use a
     * non-literal without a dictionary. *)
    (match literal with
    | Literal _ -> ()
    | _ -> rec_flow cx trace (intrinsics, HasOwnPropT (use_op, reason, DefT (reason, StrT literal))));

    (* Create a type variable which will represent the specific intrinsic we
     * find in the intrinsics map. *)
    let intrinsic = Tvar.mk_no_wrap cx reason in
    (* Get the intrinsic from the map. *)
    rec_flow
      cx
      trace
      ( intrinsics,
        GetPropT
          {
            use_op;
            reason;
            id = None;
            from_annot = false;
            propref =
              (match literal with
              | Literal (_, name) ->
                let reason =
                  replace_desc_reason
                    (RReactElement { name_opt = Some name; from_component_syntax = false })
                    reason
                in
                mk_named_prop ~reason name
              | _ -> Computed component);
            tout = (reason, intrinsic);
            hint = hint_unavailable;
          }
      );

    (* Get the artifact from the intrinsic. *)
    let propref =
      let name =
        match artifact with
        | `Props -> "props"
        | `Instance -> "instance"
      in
      let reason = replace_desc_reason (RCustom name) reason_op in
      mk_named_prop ~reason (OrdinaryName name)
    in
    (* TODO: if intrinsic is null, we will treat it like prototype termination,
     * but we should error like a GetPropT would instead. *)
    rec_flow
      cx
      trace
      ( OpenT (reason, intrinsic),
        LookupT
          {
            reason = reason_op;
            lookup_kind = Strict reason_op;
            try_ts_on_failure = [];
            propref;
            lookup_action = LookupProp (unknown_use, prop);
            method_accessible = true;
            ids = Some Properties.Set.empty;
            ignore_dicts = false;
          }
      )

  let subtype_class_component_render
      cx trace ~use_op class_component_instance ~reason_op upper_render =
    let name = "render" in
    let reason_prop = replace_desc_reason (RMethod (Some name)) reason_op in
    let propref = mk_named_prop ~reason:reason_prop (OrdinaryName name) in
    let tvar = Tvar.mk_no_wrap cx reason_op in
    let action =
      CallM
        {
          methodcalltype =
            {
              meth_generic_this = None;
              meth_targs = None;
              meth_args_tlist = [];
              meth_tout = (reason_op, tvar);
              meth_strict_arity = true;
            };
          return_hint = Type.hint_unavailable;
          specialized_callee = None;
        }
    in
    (* Call the `render` method. *)
    rec_flow
      cx
      trace
      (class_component_instance, MethodT (unknown_use, reason_op, reason_op, propref, action));
    rec_flow_t cx trace ~use_op (OpenT (reason_op, tvar), upper_render)

  (* Lookup the defaultProps of a component and flow with upper depending
   * on the given polarity.
   *)
  let lookup_defaults cx trace component ~reason_op upper pole =
    let name = OrdinaryName "defaultProps" in
    let reason_missing = replace_desc_reason RReactDefaultProps (reason_of_t component) in
    let reason_prop = replace_desc_reason (RProperty (Some name)) reason_op in
    let lookup_kind = NonstrictReturning (Some (DefT (reason_missing, VoidT), upper), None) in
    let propref = mk_named_prop ~reason:reason_prop name in
    let action = LookupProp (unknown_use, OrdinaryField { type_ = upper; polarity = pole }) in
    (* Lookup the `defaultProps` property. *)
    rec_flow
      cx
      trace
      ( component,
        LookupT
          {
            reason = reason_op;
            lookup_kind;
            try_ts_on_failure = [];
            propref;
            lookup_action = action;
            method_accessible = true;
            ids = Some Properties.Set.empty;
            ignore_dicts = false;
          }
      )

  (* Get a type for the default props of a component. If a component has no
   * default props then either the type will be Some {||} or we will
   * return None. *)
  let get_defaults cx trace component ~reason_op =
    match drop_generic component with
    | DefT (_, ClassT _) ->
      let tvar = Tvar.mk cx reason_op in
      lookup_defaults cx trace component ~reason_op tvar Polarity.Positive;
      Some tvar
    | DefT (_, FunT _)
    | DefT (_, ObjT _) ->
      if Context.react_disable_function_components_default_props cx then
        Some (VoidT.make reason_op)
      else
        let tvar = Tvar.mk cx reason_op in
        lookup_defaults cx trace component ~reason_op tvar Polarity.Positive;
        Some tvar
    | DefT (_, ReactAbstractComponentT _) -> None
    (* Everything else will not have default props we should diff out. *)
    | _ -> None

  let props_to_tout cx trace component ~use_op ~reason_op u tout =
    match drop_generic component with
    (* Class components or legacy components. *)
    | DefT (_, ClassT _) ->
      let props = Tvar.mk cx reason_op in
      rec_flow_t ~use_op:unknown_use cx trace (props, tout);
      rec_flow cx trace (component, ReactPropsToOut (reason_op, props))
    (* Stateless functional components. *)
    | DefT (_, FunT _)
    | DefT (_, ObjT { call_t = Some _; _ }) ->
      rec_flow cx trace (component, ReactPropsToOut (reason_op, tout))
    (* Special case for intrinsic components. *)
    | DefT (_, StrT lit) ->
      get_intrinsic
        cx
        trace
        component
        ~reason_op
        `Props
        lit
        (OrdinaryField { type_ = tout; polarity = Polarity.Positive })
    (* any and any specializations *)
    | AnyT (reason, src) -> rec_flow_t ~use_op:unknown_use cx trace (AnyT.why src reason, tout)
    | DefT (reason, ReactAbstractComponentT { config; _ }) ->
      rec_flow cx trace (config, ConvertEmptyPropsToMixedT (reason, tout))
    (* ...otherwise, error. *)
    | _ ->
      err_incompatible cx ~use_op (reason_of_t component) u;
      rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, tout)

  (* Creates the type that we expect for a React config by diffing out default
   * props with ObjKitT(Rest). The config does not include types for `key`
   * or `ref`.
   *
   * There is some duplication between the logic used here to get a config type
   * and ObjKitT(ReactConfig). In create_element, we want to produce a props
   * object from the config object and the defaultProps object. This way we can
   * add a lower bound to components who have a type variable for props. e.g.
   *
   *     const MyComponent = props => null;
   *     <MyComponent foo={42} />;
   *
   * Here, MyComponent has no annotation for props so Flow must infer a type.
   * However, get_config must produce a valid type from only the component type.
   *
   * This approach may stall if props never gets a lower bound. Using the result
   * of get_config as an upper bound won't give props a lower bound. However,
   * the places in which this approach stalls are the same places as other type
   * destructor annotations. Like object spread, $Diff, and $Rest. *)
  let get_config cx trace component ~use_op ~reason_op u pole tout =
    match drop_generic component with
    | DefT (_, ReactAbstractComponentT { config; _ }) ->
      let use_op = Frame (ReactGetConfig { polarity = pole }, use_op) in
      begin
        match pole with
        | Polarity.Positive -> rec_flow_t ~use_op cx trace (config, tout)
        | Polarity.Negative -> rec_flow_t ~use_op cx trace (tout, config)
        | Polarity.Neutral -> rec_unify cx trace ~use_op tout config
      end
    | _ ->
      let reason_component = reason_of_t component in
      let props =
        Tvar.mk_where
          cx
          (replace_desc_reason RReactProps reason_component)
          (props_to_tout cx trace component ~use_op ~reason_op:reason_component u)
      in
      let defaults = get_defaults cx trace component ~reason_op in
      (match defaults with
      | None -> rec_flow cx trace (props, UseT (use_op, tout))
      | Some defaults ->
        Object.(
          Object.Rest.(
            let tool = Resolve Next in
            let state = One defaults in
            rec_flow
              cx
              trace
              (props, ObjKitT (use_op, reason_op, tool, Rest (ReactConfigMerge pole, state), tout))
          )
        ))

  let run cx trace ~use_op reason_op l u =
    let err_incompatible reason = err_incompatible cx ~use_op reason u in
    let get_intrinsic = get_intrinsic cx trace l ~reason_op in
    (* This function creates a constraint *from* tin *to* props so that props is
     * an upper bound on tin. This is important because when the type of a
     * component's props is inferred (such as when a stateless functional
     * component has an unannotated props argument) we want to create a constraint
     * *from* the props input *to* tin which should then be propagated to the
     * inferred props type. *)
    let tin_to_props tin =
      let component = l in
      match drop_generic component with
      (* Class components or legacy components. *)
      | DefT (_, ClassT _) ->
        (* The Props type parameter is invariant, but we only want to create a
         * constraint tin <: props. *)
        let props = Tvar.mk cx reason_op in
        rec_flow_t ~use_op:unknown_use cx trace (tin, props);
        rec_flow cx trace (component, ReactInToProps (reason_op, props))
      (* Stateless functional components. *)
      | DefT (_, FunT _)
      (* Stateless functional components, again. This time for callable `ObjT`s. *)
      | DefT (_, ObjT { call_t = Some _; _ }) ->
        rec_flow cx trace (component, ReactInToProps (reason_op, tin))
      (* Abstract components. *)
      | DefT (reason, ReactAbstractComponentT _) ->
        rec_flow_t ~use_op:unknown_use cx trace (tin, MixedT.why reason)
      (* Intrinsic components. *)
      | DefT (_, StrT lit) ->
        get_intrinsic `Props lit (OrdinaryField { type_ = tin; polarity = Polarity.Negative })
      | AnyT (reason, source) ->
        rec_flow_t ~use_op:unknown_use cx trace (tin, AnyT.why source reason)
      (* ...otherwise, error. *)
      | _ ->
        let reason = reason_of_t component in
        err_incompatible reason;
        rec_flow_t ~use_op:unknown_use cx trace (tin, AnyT.error reason)
    in
    let props_to_tout = props_to_tout cx trace l ~use_op ~reason_op u in
    let config_check use_op ~jsx_props =
      (* Create a type variable for our props. *)
      let (component_props, component_default_props) =
        match drop_generic l with
        | DefT (_, ReactAbstractComponentT { config; _ }) -> (config, None)
        | _ ->
          ( Tvar.mk_where cx reason_op tin_to_props,
            (* For class components and function components we want to lookup the
             * static default props property so that we may add it to our config input. *)
            get_defaults cx trace l ~reason_op
          )
      in
      (* Use object spread to add children to config (if we have children)
       * and remove key and ref since we already checked key and ref. Finally in
       * this block we will flow the final config to our props type.
       *
       * NOTE: We don't eagerly run this check so that create_element can constrain the
       * ref and key pseudoprops before we run the config check.
       *)
      Object.(
        Object.ReactConfig.(
          (* We need to treat jsx props input as a literal here so we ensure it has the
           * RReactProps reason description. *)
          let reason = replace_desc_new_reason RReactProps (reason_of_t jsx_props) in
          (* Create the final config object using the ReactConfig object kit tool
           * and flow it to our type for props.
           *
           * We wrap our use_op in a ReactConfigCheck frame to increment the
           * speculation error message score. Usually we will already have a
           * ReactCreateElementCall use_op, but we want errors after this point to
           * win when picking the best errors speculation discovered. *)
          let use_op = Frame (ReactConfigCheck, use_op) in
          rec_flow
            cx
            trace
            ( jsx_props,
              ObjKitT
                ( use_op,
                  reason,
                  Resolve Next,
                  ReactConfig (Config { component_default_props }),
                  component_props
                )
            )
        )
      )
    in
    let create_element
        component jsx_props record_monomorphized_result inferred_targs specialized_component tout =
      let use_op =
        (* Why do we try to remove the OpaqueTypeBound frame here?
         * The frame will be added when we unwrap the opaque type bound of `React$CreateElement`.
         * The error printing logic will unconditionally use the reason of `React$CreateElement`
         * tracked here to replace the actual lower bound.
         * TODO: generate reasons in a more principled way everywhere, so we don't need this hack. *)
        let rec unwrap = function
          | Frame (OpaqueTypeBound _, use_op) -> unwrap use_op
          | Frame (f, use_op) -> Frame (f, unwrap use_op)
          | Op _ as use_op -> use_op
        in
        unwrap use_op
      in
      config_check use_op ~jsx_props;

      (* If our jsx props is void or null then we want to replace it with an
       * empty object.
       *
       * NOTE: We only need the normalized config to look up the key
       * and ref.
       *)
      let normalized_jsx_props =
        Tvar.mk_where cx (reason_of_t jsx_props) (fun normalized_config ->
            Object.(
              let reason = reason_of_t jsx_props in
              rec_flow
                cx
                trace
                (jsx_props, ObjKitT (use_op, reason, Resolve Next, ObjectRep, normalized_config))
            )
        )
      in
      (* Check the type of React keys in the config input.
       *
       * NOTE: We are intentionally being unsound here. If config is inexact
       * and we can't find a key prop in config then the sound thing to do
       * would be to assume that the type of key is mixed. Instead we are unsound
       * and don't check a type for key. Otherwise we would cause a lot of issues
       * in existing React code. *)
      let () =
        let reason_key =
          replace_desc_reason (RCustom "React key") (reason_of_t normalized_jsx_props)
        in
        (* Create the key type. *)
        let key_t = optional (maybe (get_builtin_type cx reason_key "React$Key")) in
        (* Flow the config input key type to the key type. *)
        let lookup_kind = NonstrictReturning (None, None) in
        let prop_name = OrdinaryName "key" in
        let propref = mk_named_prop ~reason:reason_key prop_name in
        let use_op =
          Frame
            ( PropertyCompatibility
                {
                  prop = Some prop_name;
                  lower = reason_of_t normalized_jsx_props;
                  upper = reason_key;
                },
              use_op
            )
        in
        let action =
          LookupProp (use_op, OrdinaryField { type_ = key_t; polarity = Polarity.Positive })
        in
        rec_flow
          cx
          trace
          ( normalized_jsx_props,
            LookupT
              {
                reason = reason_key;
                lookup_kind;
                try_ts_on_failure = [];
                propref;
                lookup_action = action;
                method_accessible = true;
                ids = Some Properties.Set.empty;
                ignore_dicts = false;
              }
          )
      in
      (* Check the type of React refs in the config input.
       *
       * NOTE: We are intentionally being unsound here. If config is inexact
       * and we can't find a ref prop in config then the sound thing to do
       * would be to assume that the type of ref is mixed. Instead we are unsound
       * and don't check a type for key. Otherwise we would cause a lot of issues
       * in existing React code. *)
      let () =
        let reason_ref =
          replace_desc_reason (RCustom "React ref") (reason_of_t normalized_jsx_props)
        in
        (* Create the ref type. *)
        let ref_t = optional (maybe (get_builtin_typeapp cx reason_ref "React$Ref" [l])) in
        (* Flow the config input ref type to the ref type. *)
        let lookup_kind = NonstrictReturning (None, None) in
        let prop_name = OrdinaryName "ref" in
        let propref = mk_named_prop ~reason:reason_ref prop_name in
        let use_op =
          Frame
            ( PropertyCompatibility
                {
                  prop = Some prop_name;
                  lower = reason_of_t normalized_jsx_props;
                  upper = reason_ref;
                },
              use_op
            )
        in
        let action =
          LookupProp (use_op, OrdinaryField { type_ = ref_t; polarity = Polarity.Positive })
        in
        rec_flow
          cx
          trace
          ( normalized_jsx_props,
            LookupT
              {
                reason = reason_ref;
                lookup_kind;
                try_ts_on_failure = [];
                propref;
                lookup_action = action;
                method_accessible = true;
                ids = Some Properties.Set.empty;
                ignore_dicts = false;
              }
          )
      in
      let annot_loc = loc_of_reason reason_op in
      let elem_reason =
        let desc = react_element_desc_of_component_reason (reason_of_t l) in
        annot_reason ~annot_loc (replace_desc_reason desc reason_op)
      in
      let elem =
        get_builtin_typeapp
          cx
          elem_reason
          ~use_desc:true
          "React$Element"
          [component; Tvar.mk_where cx reason_op props_to_tout]
      in
      (* Concretize to an ObjT so that we can asssociate the monomorphized component with the props id *)
      let elem =
        let result = Flow.singleton_concrete_type_for_inspection cx elem_reason elem in
        match result with
        | OpaqueT
            ( _,
              ( {
                  super_t =
                    Some
                      (DefT (super_r, ObjT { props_tmap; flags; proto_t; call_t; reachable_targs }));
                  _;
                } as opaque_t
              )
            ) ->
          if record_monomorphized_result then (
            let props_tmap = Context.generate_property_map cx (Context.find_props cx props_tmap) in
            let t =
              OpaqueT
                ( elem_reason,
                  {
                    opaque_t with
                    super_t =
                      Some
                        (DefT (super_r, ObjT { props_tmap; flags; proto_t; call_t; reachable_targs })
                        );
                  }
                )
            in
            Context.add_monomorphized_component cx props_tmap l;
            t
          ) else
            elem
        | _ ->
          (*TODO(jmbrown): Internal Error *)
          elem
      in
      (* Record the instantiated type for hover types. *)
      let () =
        let component = l in
        match (drop_generic component, inferred_targs) with
        | ( DefT
              ( r,
                ReactAbstractComponentT
                  { config; instance; renders; component_kind = Nominal (loc, name, _) }
              ),
            Some inferred_targs
          ) ->
          let ts = Some (Base.List.map ~f:fst inferred_targs) in
          let inst_component =
            DefT
              ( r,
                ReactAbstractComponentT
                  { config; instance; renders; component_kind = Nominal (loc, name, ts) }
              )
          in
          Flow_js_utils.CalleeRecorder.add_callee
            cx
            Flow_js_utils.CalleeRecorder.Tast
            inst_component
            specialized_component
        | ((DefT (_, FunT _) as fn), _) ->
          Flow_js_utils.CalleeRecorder.add_callee
            cx
            Flow_js_utils.CalleeRecorder.Tast
            fn
            specialized_component
        | _ -> ()
      in
      rec_flow_t ~use_op:unknown_use cx trace (elem, tout)
    in
    let get_config = get_config cx trace l ~use_op ~reason_op u Polarity.Positive in
    let get_config_with_props_and_defaults default_props tout =
      Object.(
        Object.Rest.(
          let props = l in
          let tool = Resolve Next in
          let state = One default_props in
          rec_flow
            cx
            trace
            ( props,
              ObjKitT
                ( Op UnknownUse,
                  reason_op,
                  tool,
                  Rest (ReactConfigMerge Polarity.Neutral, state),
                  tout
                )
            )
        )
      )
    in
    let get_instance tout =
      let component = l in
      match drop_generic component with
      (* Class components or legacy components. *)
      | DefT (_, ClassT component) -> rec_flow_t ~use_op:unknown_use cx trace (component, tout)
      (* Stateless functional components. *)
      | DefT (r, FunT _) ->
        rec_flow_t ~use_op:unknown_use cx trace (VoidT.make (replace_desc_reason RVoid r), tout)
      (* Stateless functional components, again. This time for callable `ObjT`s. *)
      | DefT (r, ObjT { call_t = Some _; _ }) ->
        rec_flow_t ~use_op:unknown_use cx trace (VoidT.make (replace_desc_reason RVoid r), tout)
      (* Abstract components. *)
      | DefT (_, ReactAbstractComponentT { instance = ComponentInstanceAvailable instance; _ }) ->
        rec_flow_t ~use_op:unknown_use cx trace (instance, tout)
      | DefT (r, ReactAbstractComponentT { instance = ComponentInstanceOmitted _; _ }) ->
        rec_flow_t ~use_op:unknown_use cx trace (VoidT.make (replace_desc_reason RVoid r), tout)
      (* Intrinsic components. *)
      | DefT (_, StrT lit) ->
        get_intrinsic `Instance lit (OrdinaryField { type_ = tout; polarity = Polarity.Positive })
      | AnyT (reason, source) ->
        rec_flow_t ~use_op:unknown_use cx trace (AnyT.why source reason, tout)
      (* ...otherwise, error. *)
      | _ ->
        let reason = reason_of_t component in
        err_incompatible reason;
        rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason, tout)
    in
    match u with
    | CreateElement
        {
          component;
          jsx_props;
          tout;
          targs = _;
          return_hint = _;
          record_monomorphized_result;
          inferred_targs;
          specialized_component;
        } ->
      create_element
        component
        jsx_props
        record_monomorphized_result
        inferred_targs
        specialized_component
        tout
    | ConfigCheck jsx_props -> config_check use_op ~jsx_props
    | GetProps tout -> props_to_tout tout
    | GetConfig tout -> get_config tout
    | GetConfigType (default_props, tout) -> get_config_with_props_and_defaults default_props tout
    | GetRef tout -> get_instance tout
end
