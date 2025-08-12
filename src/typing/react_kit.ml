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
    in
    Flow_js_utils.add_output cx err

  let component_class cx reason props =
    DefT
      ( reason,
        ClassT
          (Flow.get_builtin_react_typeapp
             cx
             reason
             Flow_intermediate_error_types.ReactModuleForReactClassComponent
             [props; Tvar.mk cx reason]
          )
      )

  let get_intrinsic cx trace ~component_reason ~reason_op ~artifact ~literal ~prop_polarity tout =
    let reason = component_reason in
    (* Get the internal $JSXIntrinsics map. *)
    let intrinsics =
      let reason =
        mk_reason (RType (OrdinaryName "$JSXIntrinsics")) (loc_of_reason component_reason)
      in
      get_builtin_type cx reason "$JSXIntrinsics"
    in
    (* Create a use_op for the upcoming operations. *)
    let use_op =
      Op
        (ReactGetIntrinsic
           {
             literal =
               (match literal with
               | `Literal name -> replace_desc_reason (RIdentifier name) reason
               | `General _ -> reason);
           }
        )
    in
    (* GetPropT with a non-literal when there is not a dictionary will propagate
     * any. Run the HasOwnPropT check to give the user an error if they use a
     * non-literal without a dictionary. *)
    (match literal with
    | `Literal _ -> ()
    | `General gen ->
      rec_flow cx trace (intrinsics, HasOwnPropT (use_op, reason, DefT (reason, StrGeneralT gen))));

    (* Get the intrinsic from the map. *)
    let intrinsic =
      Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun tout ->
          let propref =
            match literal with
            | `Literal name ->
              let reason =
                replace_desc_reason
                  (RReactElement { name_opt = Some name; from_component_syntax = false })
                  reason
              in
              mk_named_prop ~reason name
            | `General _ -> Computed (DefT (reason, StrGeneralT AnyLiteral))
          in
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
                  skip_optional = false;
                  propref;
                  tout;
                  hint = hint_unavailable;
                }
            )
      )
    in

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
      ( intrinsic,
        LookupT
          {
            reason = reason_op;
            lookup_kind = Strict reason_op;
            try_ts_on_failure = [];
            propref;
            lookup_action = LookupPropForTvarPopulation { tout; polarity = prop_polarity };
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
    let action = LookupPropForTvarPopulation { tout = upper; polarity = pole } in
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
      None
    | DefT (_, ReactAbstractComponentT _) -> None
    (* Everything else will not have default props we should diff out. *)
    | _ -> None

  let get_expected_ref cx reason_ref component =
    match component with
    | DefT (_, ReactAbstractComponentT { instance = ComponentInstanceAvailableAsRefSetterProp t; _ })
      ->
      Some t
    | DefT (_, ClassT instance) ->
      get_builtin_react_typeapp
        cx
        (update_desc_new_reason (fun desc -> RTypeAppImplicit desc) reason_ref)
        Flow_intermediate_error_types.ReactModuleForReactRefSetterType
        [instance]
      |> Option.some
    | DefT (_, FunT _)
    | DefT (_, ObjT _)
    | DefT (_, ReactAbstractComponentT { instance = ComponentInstanceOmitted _; _ }) ->
      None
    | DefT (_, SingletonStrT { value = name; _ }) ->
      let instance =
        Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason_ref (fun tout ->
            get_intrinsic
              cx
              DepthTrace.unit_trace
              ~component_reason:(reason_of_t component)
              ~reason_op:reason_ref
              ~artifact:`Instance
              ~literal:(`Literal name)
              ~prop_polarity:Polarity.Positive
              tout
        )
      in
      get_builtin_react_typeapp
        cx
        (update_desc_new_reason (fun desc -> RTypeAppImplicit desc) reason_ref)
        Flow_intermediate_error_types.ReactModuleForReactRefSetterType
        [instance]
      |> Option.some
    | _ -> None

  let rec props_to_tout cx trace component ~use_op ~reason_op u tout =
    match drop_generic component with
    (* Class components or legacy components. *)
    | DefT (r, ClassT _) ->
      let props = Tvar.mk cx reason_op in
      rec_flow_t ~use_op:unknown_use cx trace (props, tout);
      rec_flow_t ~use_op:unknown_use cx trace (component, component_class cx r props)
    (* Functional components. *)
    | DefT (r, FunT (_, fun_t)) ->
      (match fun_t with
      | {
       params;
       rest_param = None;
       type_guard = None | Some (TypeGuard { inferred = true; _ });
       effect_ = ArbitraryEffect | AnyEffect;
       _;
      } ->
        (* Contravariance *)
        Base.List.hd params
        |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx r)
        |> fun t -> rec_flow_t ~use_op:unknown_use cx trace (t, tout)
      | _ ->
        err_incompatible cx ~use_op:unknown_use r (React.GetProps tout);
        rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, tout))
    | DefT (r, ObjT { call_t = Some id; _ }) ->
      (match Context.find_call cx id with
      | DefT
          ( _,
            FunT
              ( _,
                {
                  rest_param = None;
                  type_guard = None | Some (TypeGuard { inferred = true; _ });
                  _;
                }
              )
          ) as fun_t ->
        (* Keep the object's reason for better error reporting *)
        props_to_tout cx trace (mod_reason_of_t (Fun.const r) fun_t) ~use_op ~reason_op u tout
      | _ ->
        err_incompatible cx ~use_op:unknown_use r (React.GetProps tout);
        rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, tout))
    (* Special case for intrinsic components. *)
    | DefT (_, SingletonStrT { value = name; _ }) ->
      get_intrinsic
        cx
        trace
        ~component_reason:(reason_of_t component)
        ~reason_op
        ~artifact:`Props
        ~literal:(`Literal name)
        ~prop_polarity:Polarity.Positive
        tout
    | DefT (_, StrGeneralT gen) ->
      get_intrinsic
        cx
        trace
        ~component_reason:(reason_of_t component)
        ~reason_op
        ~artifact:`Props
        ~literal:(`General gen)
        ~prop_polarity:Polarity.Positive
        tout
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
    let err_incompatible ?(use_op = use_op) reason = err_incompatible cx ~use_op reason u in
    let get_intrinsic = get_intrinsic cx trace ~component_reason:(reason_of_t l) ~reason_op in
    (* This function creates a constraint *from* tin *to* props so that props is
     * an upper bound on tin. This is important because when the type of a
     * component's props is inferred (such as when a stateless functional
     * component has an unannotated props argument) we want to create a constraint
     * *from* the props input *to* tin which should then be propagated to the
     * inferred props type. *)
    let rec tin_to_props l tin =
      let component = l in
      match drop_generic component with
      (* Class components or legacy components. *)
      | DefT (r, ClassT _) ->
        (* The Props type parameter is invariant, but we only want to create a
         * constraint tin <: props. *)
        let props = Tvar.mk cx reason_op in
        rec_flow_t ~use_op:unknown_use cx trace (tin, props);
        rec_flow_t ~use_op:unknown_use cx trace (component, component_class cx r props)
        (* Stateless functional components. *)
      | DefT (r, FunT (_, fun_t)) ->
        (match fun_t with
        | {
         params;
         return_t;
         rest_param = None;
         type_guard = _;
         effect_ = ArbitraryEffect | AnyEffect;
         _;
        } ->
          (* Contravariance *)
          Base.List.hd params
          |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx r)
          |> fun t ->
          rec_flow_t ~use_op:unknown_use cx trace (tin, t);
          if not (Context.in_implicit_instantiation cx) then
            rec_flow_t
              ~use_op:unknown_use
              cx
              trace
              ( return_t,
                get_builtin_react_type
                  cx
                  reason_op
                  Flow_intermediate_error_types.ReactModuleForReactNodeType
              )
        | _ ->
          err_incompatible ~use_op:unknown_use r;
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, tin))
      (* Functional components, again. This time for callable `ObjT`s. *)
      | DefT (r, ObjT { call_t = Some id; _ }) ->
        (match Context.find_call cx id with
        | DefT
            ( _,
              FunT
                ( _,
                  {
                    rest_param = None;
                    type_guard = None | Some (TypeGuard { inferred = true; _ });
                    _;
                  }
                )
            ) as fun_t ->
          (* Keep the object's reason for better error reporting *)
          tin_to_props (mod_reason_of_t (Fun.const r) fun_t) tin
        | _ ->
          err_incompatible ~use_op:unknown_use r;
          rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, tin))
      (* Abstract components. *)
      | DefT (reason, ReactAbstractComponentT _) ->
        rec_flow_t ~use_op:unknown_use cx trace (tin, MixedT.why reason) (* Intrinsic components. *)
      | DefT (_, SingletonStrT { value = name; _ }) ->
        get_intrinsic ~artifact:`Props ~literal:(`Literal name) ~prop_polarity:Polarity.Negative tin
      | DefT (_, StrGeneralT gen) ->
        get_intrinsic ~artifact:`Props ~literal:(`General gen) ~prop_polarity:Polarity.Negative tin
      | AnyT (reason, source) ->
        rec_flow_t ~use_op:unknown_use cx trace (tin, AnyT.why source reason)
      (* ...otherwise, error. *)
      | _ ->
        let reason = reason_of_t component in
        err_incompatible reason;
        rec_flow_t ~use_op:unknown_use cx trace (tin, AnyT.error reason)
    in
    let props_to_tout = props_to_tout cx trace l ~use_op ~reason_op u in
    let config_check use_op ~instance ~jsx_props =
      let props_of_fn_component l =
        match drop_generic l with
        | DefT (r, FunT (_, { params; _ })) ->
          Some
            (Base.List.hd params
            |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx r)
            )
        | DefT (r, ObjT { call_t = Some id; _ }) ->
          (match Context.find_call cx id with
          | DefT (_, FunT (_, { params; _ })) ->
            Some
              (Base.List.hd params
              |> Base.Option.value_map ~f:snd ~default:(Obj_type.mk ~obj_kind:Exact cx r)
              )
          | _ -> None)
        | DefT (_, ReactAbstractComponentT { config; instance = ComponentInstanceOmitted _; _ }) ->
          Some config
        | _ -> None
      in
      let definitely_has_ref_in_props cx r props =
        (* If props is unresolved, then the following check will succeed,
         * but it doesn't tell us anything. In this case, we decide to intentionally assume
         * that the inferred props should not contain a ref prop. *)
        (not (Flow_js_utils.TvarVisitors.has_unresolved_tvars cx props))
        && speculative_subtyping_succeeds
             cx
             (DefT (r, SingletonStrT { from_annot = false; value = OrdinaryName "ref" }))
             (KeysT (r, props))
      in
      (* Create a type variable for our props. *)
      let (component_props, component_default_props) =
        match drop_generic l with
        | DefT (_, ReactAbstractComponentT { config; _ }) -> (config, None)
        | _ ->
          ( Tvar.mk_where cx reason_op (tin_to_props l),
            (* For class components and function components we want to lookup the
             * static default props property so that we may add it to our config input. *)
            get_defaults cx trace l ~reason_op
          )
      in
      let ref_manipulation =
        match props_of_fn_component l with
        | None -> Object.ReactConfig.FilterRef
        | Some props ->
          (match instance with
          | None -> Object.ReactConfig.KeepRef
          | Some (ComponentInstanceOmitted _) ->
            (* e.g. `component(foo: string)`, which doesn't have ref prop,
             * so we just do `{foo: string} ~> function component props` *)
            Object.ReactConfig.KeepRef
          | Some (ComponentInstanceAvailableAsRefSetterProp ref_t) ->
            let r = reason_of_t ref_t in
            if definitely_has_ref_in_props cx r props then (
              ( if Context.in_implicit_instantiation cx then
                (* Why do we need to do this when ref_t is added below anyways?
                 * In implicit instantiation, we might have `fn_component ~> component(ref: infer I, ...infer Props)`
                 * The ref type will be underconstrained in implicit instantiation,
                 * so we need the extra flow to constrain it. *)
                let fn_component_ref =
                  EvalT
                    ( props,
                      TypeDestructorT (use_op, r, PropertyType { name = OrdinaryName "ref" }),
                      Eval.generate_id ()
                    )
                in
                rec_flow_t
                  cx
                  trace
                  ~use_op:(Frame (ReactConfigCheck, use_op))
                  (ref_t, fn_component_ref)
              );
              (* If we see that the function component has a ref prop,
               * then given `component(ref: R, ...Props)`,
               * and `(fn_props_has_ref) => React.Node`
               * we will do something equivalent to
               * {...Props, ref: R} ~> fn_props_has_ref *)
              Object.ReactConfig.AddRef ref_t
            ) else
              (* If function component doesn't have a ref prop, technically
               * we should do the same thing as above, but it will fail
               * ({}) => React.Node ~> component(ref: React.RefSetter<mixed>)
               * which previously passes. Therefore, during the transition phase,
               * we add this logic to keep the old behavior, where a function
               * component is treated as `component(ref: React.RefSetter<void>)` *)
              let fn_component_ref =
                get_builtin_react_typeapp
                  cx
                  (update_desc_new_reason (fun desc -> RTypeAppImplicit desc) r)
                  Flow_intermediate_error_types.ReactModuleForReactRefSetterType
                  [VoidT.why r]
              in
              rec_flow_t
                cx
                trace
                ~use_op:(Frame (ReactConfigCheck, use_op))
                (ref_t, fn_component_ref);
              Object.ReactConfig.FilterRef)
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
                  ReactConfig { state = Config { component_default_props }; ref_manipulation },
                  component_props
                )
            )
        )
      )
    in
    let create_element
        component jsx_props record_monomorphized_result inferred_targs specialized_component tout =
      let use_op =
        (* Why do we try to remove the OpaqueTypeUpperBound frame here?
         * The frame will be added when we unwrap the opaque type bound of `React$CreateElement`.
         * The error printing logic will unconditionally use the reason of `React$CreateElement`
         * tracked here to replace the actual lower bound.
         * TODO: generate reasons in a more principled way everywhere, so we don't need this hack. *)
        let rec unwrap = function
          | Frame (OpaqueTypeUpperBound _, use_op) -> unwrap use_op
          | Frame (f, use_op) -> Frame (f, unwrap use_op)
          | Op _ as use_op -> use_op
        in
        unwrap use_op
      in
      config_check use_op ~instance:None ~jsx_props;

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
        let action =
          LookupPropForSubtyping
            {
              use_op;
              prop = OrdinaryField { type_ = key_t; polarity = Polarity.Positive };
              prop_name;
              reason_lower = reason_of_t normalized_jsx_props;
              reason_upper = reason_key;
            }
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
        match get_expected_ref cx reason_ref l with
        | None -> ()
        | Some ref_t ->
          (* Flow the config input ref type to the ref type. *)
          let lookup_kind = NonstrictReturning (None, None) in
          let prop_name = OrdinaryName "ref" in
          let propref = mk_named_prop ~reason:reason_ref prop_name in
          let action =
            LookupPropForSubtyping
              {
                use_op;
                prop = OrdinaryField { type_ = ref_t; polarity = Polarity.Positive };
                prop_name;
                reason_lower = reason_of_t normalized_jsx_props;
                reason_upper = reason_ref;
              }
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
      (* Concretize to an ObjT so that we can associate the monomorphized component with the props id *)
      let elem =
        let result = Flow.singleton_concrete_type_for_inspection cx elem_reason elem in
        match result with
        | OpaqueT
            ( _,
              ( {
                  upper_t =
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
                    upper_t =
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
      | DefT
          ( _,
            ReactAbstractComponentT
              { instance = ComponentInstanceAvailableAsRefSetterProp ref_prop; _ }
          ) ->
        rec_flow cx trace (ref_prop, ExtractReactRefT (reason_of_t tout, tout))
      | DefT (r, ReactAbstractComponentT { instance = ComponentInstanceOmitted _; _ }) ->
        rec_flow_t ~use_op:unknown_use cx trace (VoidT.make (replace_desc_reason RVoid r), tout)
      (* Intrinsic components. *)
      | DefT (_, SingletonStrT { value = name; _ }) ->
        get_intrinsic
          ~artifact:`Instance
          ~literal:(`Literal name)
          ~prop_polarity:Polarity.Positive
          tout
      | DefT (_, StrGeneralT gen) ->
        get_intrinsic
          ~artifact:`Instance
          ~literal:(`General gen)
          ~prop_polarity:Polarity.Positive
          tout
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
    | ConfigCheck { props = jsx_props; instance } ->
      config_check use_op ~instance:(Some instance) ~jsx_props
    | GetProps tout -> props_to_tout tout
    | GetConfig tout -> get_config tout
    | GetRef tout -> get_instance tout
end
