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
    from_userland_react_element_config:bool ->
    Type.React.tool ->
    Polarity.t ->
    Type.t ->
    unit

  val err_incompatible : Context.t -> use_op:use_op -> reason -> Type.React.tool -> unit
end

module Kit (Flow : Flow_common.S) : REACT = struct
  include Flow
  module RendersKit = Renders_kit.Make (Flow)

  let err_incompatible cx ~use_op reason tool =
    let err =
      match tool with
      | GetProps _
      | GetConfig _
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

  let add_optional_ref_prop_to_props cx trace props reason_op instance tout =
    rec_flow
      cx
      trace
      ( props,
        ObjKitT
          ( unknown_use,
            reason_op,
            Object.(Resolve Next),
            Object.ReactConfig
              {
                state = Object.ReactConfig.Config { component_default_props = None };
                ref_manipulation =
                  Object.ReactConfig.AddRef
                    (OptionalT
                       {
                         reason = reason_op;
                         use_desc = false;
                         type_ =
                           get_builtin_react_typeapp
                             cx
                             reason_op
                             Flow_intermediate_error_types.ReactModuleForReactRefSetterType
                             [instance];
                       }
                    );
              },
            tout
          )
      )

  let rec props_to_tout
      cx trace component ~use_op ~reason_op ~from_userland_react_element_config u tout =
    match drop_generic component with
    (* Class components or legacy components. *)
    | DefT (r, ClassT i) ->
      let props = Tvar.mk cx reason_op in
      (match Context.react_ref_as_prop cx with
      | Options.ReactRefAsProp.StoreRefInPropsButRemoveRefInReactElementConfig
        when from_userland_react_element_config ->
        rec_flow_t ~use_op:unknown_use cx trace (props, tout);
        rec_flow_t ~use_op:unknown_use cx trace (component, component_class cx r props)
      | Options.ReactRefAsProp.StoreRefInPropsButRemoveRefInReactElementConfig
      | Options.ReactRefAsProp.StoreRefInPropsNoSpecialCase
      | Options.ReactRefAsProp.FullSupport ->
        rec_flow_t ~use_op:unknown_use cx trace (component, component_class cx r props);
        add_optional_ref_prop_to_props cx trace props reason_op i tout)
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
        props_to_tout
          cx
          trace
          (mod_reason_of_t (Fun.const r) fun_t)
          ~use_op
          ~reason_op
          ~from_userland_react_element_config
          u
          tout
      | _ ->
        err_incompatible cx ~use_op:unknown_use r (React.GetProps tout);
        rec_flow_t ~use_op:unknown_use cx trace (AnyT.error reason_op, tout))
    (* Special case for intrinsic components. *)
    | DefT (_, SingletonStrT { value = name; _ }) ->
      (match Context.react_ref_as_prop cx with
      | Options.ReactRefAsProp.StoreRefInPropsButRemoveRefInReactElementConfig
        when from_userland_react_element_config ->
        get_intrinsic
          cx
          trace
          ~component_reason:(reason_of_t component)
          ~reason_op
          ~artifact:`Props
          ~literal:(`Literal name)
          ~prop_polarity:Polarity.Positive
          tout
      | Options.ReactRefAsProp.StoreRefInPropsButRemoveRefInReactElementConfig
      | Options.ReactRefAsProp.StoreRefInPropsNoSpecialCase
      | Options.ReactRefAsProp.FullSupport ->
        let props = Tvar.mk cx reason_op in
        get_intrinsic
          cx
          trace
          ~component_reason:(reason_of_t component)
          ~reason_op
          ~artifact:`Props
          ~literal:(`Literal name)
          ~prop_polarity:Polarity.Positive
          props;
        let i =
          Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason_op (fun tout ->
              get_intrinsic
                cx
                trace
                ~component_reason:(reason_of_t component)
                ~reason_op
                ~artifact:`Instance
                ~literal:(`Literal name)
                ~prop_polarity:Polarity.Positive
                tout
          )
        in
        add_optional_ref_prop_to_props cx trace props reason_op i tout)
    | DefT (_, StrGeneralT gen) ->
      (match Context.react_ref_as_prop cx with
      | Options.ReactRefAsProp.StoreRefInPropsButRemoveRefInReactElementConfig
        when from_userland_react_element_config ->
        get_intrinsic
          cx
          trace
          ~component_reason:(reason_of_t component)
          ~reason_op
          ~artifact:`Props
          ~literal:(`General gen)
          ~prop_polarity:Polarity.Positive
          tout
      | Options.ReactRefAsProp.StoreRefInPropsButRemoveRefInReactElementConfig
      | Options.ReactRefAsProp.StoreRefInPropsNoSpecialCase
      | Options.ReactRefAsProp.FullSupport ->
        let props = Tvar.mk cx reason_op in
        let i =
          Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason_op (fun tout ->
              get_intrinsic
                cx
                trace
                ~component_reason:(reason_of_t component)
                ~reason_op
                ~artifact:`Instance
                ~literal:(`General gen)
                ~prop_polarity:Polarity.Positive
                tout
          )
        in
        get_intrinsic
          cx
          trace
          ~component_reason:(reason_of_t component)
          ~reason_op
          ~artifact:`Props
          ~literal:(`General gen)
          ~prop_polarity:Polarity.Positive
          tout;
        add_optional_ref_prop_to_props cx trace props reason_op i tout)
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
  let get_config
      cx trace component ~use_op ~reason_op ~from_userland_react_element_config u pole tout =
    match drop_generic component with
    | DefT (_, ReactAbstractComponentT { config; _ }) ->
      let use_op = Frame (ReactGetConfig { polarity = pole }, use_op) in
      if
        Context.react_ref_as_prop cx
        = Options.ReactRefAsProp.StoreRefInPropsButRemoveRefInReactElementConfig
        && from_userland_react_element_config
      then
        match pole with
        | Polarity.Negative
        | Polarity.Neutral ->
          failwith "When from_userland_react_element_config=true, pole must be positive"
        | Polarity.Positive ->
          let reason = reason_of_t config in
          let open Object in
          let t_ref_removed =
            Tvar.mk_where cx reason (fun tout ->
                rec_flow
                  cx
                  trace
                  ( config,
                    ObjKitT
                      ( use_op,
                        reason,
                        Resolve Next,
                        ReactConfig
                          {
                            state = ReactConfig.Config { component_default_props = None };
                            ref_manipulation = ReactConfig.FilterRef;
                          },
                        tout
                      )
                  )
            )
          in
          rec_flow cx trace (t_ref_removed, ObjKitT (use_op, reason, Resolve Next, ReadOnly, tout))
      else begin
        match pole with
        | Polarity.Positive -> rec_flow_t ~use_op cx trace (config, tout)
        | Polarity.Negative -> rec_flow_t ~use_op cx trace (tout, config)
        | Polarity.Neutral ->
          rec_unify cx trace ~use_op ~unify_cause:UnifyCause.Uncategorized tout config
      end
    | _ ->
      let reason_component = reason_of_t component in
      let props =
        let reason = update_desc_reason (fun desc -> RPropsOfComponent desc) reason_component in
        Tvar.mk_where
          cx
          reason
          (props_to_tout
             cx
             trace
             component
             ~use_op
             ~reason_op:reason
             ~from_userland_react_element_config
             u
          )
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
      | DefT (_, ClassT _) as c ->
        (* The Props type parameter is invariant, but we only want to create a
         * constraint tin <: props. *)
        let props =
          EvalT
            ( c,
              TypeDestructorT
                (unknown_use, reason_op, ReactElementConfigType { from_userland = false }),
              Eval.generate_id ()
            )
        in
        rec_flow_t ~use_op:unknown_use cx trace (tin, props)
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
      | DefT (_, SingletonStrT _) as c ->
        let props =
          EvalT
            ( c,
              TypeDestructorT
                (unknown_use, reason_op, ReactElementConfigType { from_userland = false }),
              Eval.generate_id ()
            )
        in
        rec_flow_t ~use_op:unknown_use cx trace (tin, props)
      | DefT (_, StrGeneralT _) as c ->
        let props =
          EvalT
            ( c,
              TypeDestructorT
                (unknown_use, reason_op, ReactElementConfigType { from_userland = false }),
              Eval.generate_id ()
            )
        in
        rec_flow_t ~use_op:unknown_use cx trace (tin, props)
      | AnyT (reason, source) ->
        rec_flow_t ~use_op:unknown_use cx trace (tin, AnyT.why source reason)
      (* ...otherwise, error. *)
      | _ ->
        let reason = reason_of_t component in
        err_incompatible reason;
        rec_flow_t ~use_op:unknown_use cx trace (tin, AnyT.error reason)
    in
    let props_to_tout =
      props_to_tout cx trace l ~use_op ~reason_op ~from_userland_react_element_config:false u
    in
    let config_check use_op ~jsx_props =
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
      let ref_manipulation = Object.ReactConfig.KeepRef in
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
        component
        jsx_props
        should_generalize
        record_monomorphized_result
        inferred_targs
        specialized_component
        tout =
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
          "ExactReactElement_DEPRECATED"
          [component; Tvar.mk_where cx reason_op props_to_tout]
      in
      (* Concretize to an ObjT so that we can asssociate the monomorphized component with the props id *)
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
                ReactAbstractComponentT { config; renders; component_kind = Nominal (loc, name, _) }
              ),
            Some inferred_targs
          ) ->
          let ts = Some (Base.List.map ~f:fst inferred_targs) in
          let inst_component =
            DefT
              ( r,
                ReactAbstractComponentT
                  { config; renders; component_kind = Nominal (loc, name, ts) }
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
      let elem =
        if should_generalize then
          match RendersKit.try_synthesize_render_type cx ~drop_renders_any:false elem with
          | None ->
            get_builtin_react_type
              cx
              ~trace
              elem_reason
              Flow_intermediate_error_types.ReactModuleForReactMixedElementType
          | Some (renders_variant, ts) ->
            DefT
              ( elem_reason,
                RendersT
                  (StructuralRenders
                     {
                       renders_variant;
                       renders_structural_type = TypeUtil.union_of_ts elem_reason ts;
                     }
                  )
              )
        else
          elem
      in
      rec_flow_t ~use_op:unknown_use cx trace (elem, tout)
    in
    let get_config ~from_userland_react_element_config =
      get_config
        cx
        trace
        l
        ~use_op
        ~reason_op
        ~from_userland_react_element_config
        u
        Polarity.Positive
    in
    match u with
    | CreateElement
        {
          component;
          jsx_props;
          tout;
          targs = _;
          should_generalize;
          return_hint = _;
          record_monomorphized_result;
          inferred_targs;
          specialized_component;
        } ->
      create_element
        component
        jsx_props
        should_generalize
        record_monomorphized_result
        inferred_targs
        specialized_component
        tout
    | ConfigCheck { props = jsx_props } -> config_check use_op ~jsx_props
    | GetProps tout -> props_to_tout tout
    | GetConfig { from_userland_react_element_config; tout } ->
      get_config ~from_userland_react_element_config tout
end
