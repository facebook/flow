(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open React

let err_incompatible
  cx
  trace
  ~use_op
  ~(add_output: Context.t -> ?trace:Trace.t -> Error_message.t -> unit)
  reason
  tool
=
  let open React in
  let err = match tool with
  | GetProps _
  | GetConfig _
  | GetRef _
  | CreateElement0 _
  | CreateElement _
  | ConfigCheck _ ->
    Error_message.ENotAReactComponent { reason; use_op; }
  | GetConfigType _ ->
    Error_message.EInvalidReactConfigType { reason; use_op }
  | SimplifyPropType (tool, _) ->
    Error_message.EInvalidReactPropType { reason; use_op; tool }
  | CreateClass (tool, _, _) ->
    Error_message.EInvalidReactCreateClass { reason; use_op; tool }
  in
  add_output cx ~trace err

let component_class
  cx
  reason
  ~(get_builtin_typeapp: Context.t -> ?trace:Trace.t -> reason -> string -> Type.t list -> Type.t)
  props
=
  DefT (reason, bogus_trust (), ClassT (get_builtin_typeapp cx reason "React$Component" [props; Tvar.mk cx reason]))

let get_intrinsic cx trace component ~reason_op artifact literal prop ~rec_flow
  ~(get_builtin_type: Context.t -> ?trace:Trace.t -> reason -> ?use_desc:bool -> string -> Type.t)
=
  let reason = reason_of_t component in
  (* Get the internal $JSXIntrinsics map. *)
  let intrinsics =
    let reason = mk_reason (RType "$JSXIntrinsics") (loc_of_t component) in
    get_builtin_type cx ~trace reason "$JSXIntrinsics"
  in
  (* Create a use_op for the upcoming operations. *)
  let use_op = Op (ReactGetIntrinsic {
    literal = (match literal with
      | Literal (_, name) -> replace_desc_reason (RIdentifier name) reason
      | _ -> reason);
  }) in
  (* GetPropT with a non-literal when there is not a dictionary will propagate
   * any. Run the HasOwnPropT check to give the user an error if they use a
   * non-literal without a dictionary. *)
  (match literal with
    | Literal _ -> ()
    | _ -> rec_flow cx trace (intrinsics, HasOwnPropT (use_op, reason, literal)));
  (* Create a type variable which will represent the specific intrinsic we
   * find in the intrinsics map. *)
  let intrinsic = Tvar.mk cx reason in
  (* Get the intrinsic from the map. *)
  rec_flow cx trace (intrinsics, GetPropT (use_op, reason, (match literal with
    | Literal (_, name) ->
      Named (replace_desc_reason (RReactElement (Some name)) reason, name)
    | _ -> Computed component
  ), intrinsic));
  (* Get the artifact from the intrinsic. *)
  let propref =
    let name = match artifact with
    | `Props -> "props"
    | `Instance -> "instance"
    in
    Named (replace_desc_reason (RCustom name) reason_op, name)
  in
  (* TODO: if intrinsic is null, we will treat it like prototype termination,
   * but we should error like a GetPropT would instead. *)
  rec_flow cx trace (intrinsic, LookupT (
    reason_op,
    Strict reason_op,
    [],
    propref,
    LookupProp (unknown_use, prop)
  ))

(* Lookup the defaultProps of a component and flow with upper depending
 * on the given polarity.
 *)
let lookup_defaults cx trace component ~reason_op ~rec_flow upper pole =
  let name = "defaultProps" in
  let reason_missing =
    replace_desc_reason (RReactDefaultProps) (reason_of_t component) in
  let reason_prop =
    replace_desc_reason (RProperty (Some name)) reason_op in
  (* NOTE: This is intentionally unsound. Function statics are modeled
   * as an unsealed object and so a `GetPropT` would perform a shadow
   * lookup since a write to an unsealed property may happen at any
   * time. If we were to perform a shadow lookup for `defaultProps` and
   * `defaultProps` was never written then our lookup would stall and
   * therefore so would our props analysis. So instead we make the
   * stateful assumption that `defaultProps` was already written to
   * the component statics which may not always be true. *)
  let strict = NonstrictReturning (Some
    (DefT (reason_missing, bogus_trust (), VoidT), upper), None) in
  let propref = Named (reason_prop, name) in
  let action = LookupProp (unknown_use, Field (None, upper, pole)) in
  (* Lookup the `defaultProps` property. *)
  rec_flow cx trace (component,
    LookupT (reason_op, strict, [], propref, action))

(* Get a type for the default props of a component. If a component has no
 * default props then either the type will be Some {||} or we will
 * return None. *)
let get_defaults cx trace component ~reason_op ~rec_flow =
  match component with
  | DefT (_, _, ClassT _)
  | DefT (_, _, FunT _)
  | DefT (_, _, ObjT _) ->
    let tvar = Tvar.mk cx reason_op in
    lookup_defaults cx trace component ~reason_op ~rec_flow tvar Polarity.Positive;
    Some tvar
  | DefT (_, _, ReactAbstractComponentT _) -> None
  (* Everything else will not have default props we should diff out. *)
  | _ -> None

let props_to_tout
  cx
  trace
  component
  ~use_op
  ~reason_op
  ~(rec_flow_t: Context.t -> Trace.t -> ?use_op:Type.use_op -> (Type.t * Type.t) -> unit)
  ~rec_flow
  ~(get_builtin_type: Context.t -> ?trace:Trace.t -> reason -> ?use_desc:bool -> string -> Type.t)
  ~(add_output: Context.t -> ?trace:Trace.t -> Error_message.t -> unit)
  u
  tout
=
  match component with
  (* Class components or legacy components. *)
  | DefT (_, _, ClassT _) ->
    let props = Tvar.mk cx reason_op in
    rec_flow_t cx trace (props, tout);
    rec_flow cx trace (component, ReactPropsToOut (reason_op, props))

  (* Stateless functional components. *)
  | DefT (_, _, FunT _)
  | DefT (_, _, ObjT { call_t = Some _; _ }) ->
    rec_flow cx trace (component, ReactPropsToOut (reason_op, tout))

  (* Special case for intrinsic components. *)
  | DefT (_, _, StrT lit) -> get_intrinsic cx trace component
    ~reason_op
    ~rec_flow
    ~get_builtin_type
    `Props
    lit
    (Field (None, tout, Polarity.Positive))

  (* any and any specializations *)
  | AnyT (reason, src) ->
    rec_flow_t cx trace (AnyT.why src reason, tout)

  | DefT (reason, trust, ReactAbstractComponentT _) ->
    rec_flow_t cx trace (MixedT.why reason trust, tout)

  (* ...otherwise, error. *)
  | _ -> err_incompatible cx trace ~use_op ~add_output (reason_of_t component) u

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
  cx
  trace
  component
  ~use_op
  ~reason_op
  ~(rec_flow_t: Context.t -> Trace.t -> ?use_op:Type.use_op -> (Type.t * Type.t) -> unit)
  ~rec_flow
  ~(rec_unify: Context.t -> Trace.t -> use_op:Type.use_op -> ?unify_any:bool -> Type.t -> Type.t -> unit)
  ~get_builtin_type
  ~(add_output: Context.t -> ?trace:Trace.t -> Error_message.t -> unit)
  u
  pole
  tout
=
  match component with
  | DefT (_, _, ReactAbstractComponentT {config; _}) ->
      let use_op = Frame (ReactGetConfig {polarity = pole}, use_op) in
      begin match pole with
      | Polarity.Positive -> rec_flow_t ~use_op cx trace (config, tout)
      | Polarity.Negative -> rec_flow_t ~use_op cx trace (tout, config)
      | Polarity.Neutral -> rec_unify cx trace ~use_op tout config
      end
  | _ ->
    let reason_component = (reason_of_t component) in
    let props = Tvar.mk_where cx (replace_desc_reason RReactProps reason_component) (props_to_tout
      cx
      trace
      component
      ~use_op
      ~reason_op:reason_component
      ~rec_flow_t
      ~rec_flow
      ~get_builtin_type
      ~add_output
      u
    ) in
    let defaults = get_defaults cx trace component ~reason_op ~rec_flow in
    match defaults with
    | None -> rec_flow cx trace (props, UseT (use_op, tout))
    | Some defaults ->
      let open Object in
      let open Object.Rest in
      let tool = Resolve Next in
      let state = One defaults in
      rec_flow cx trace (props,
        ObjKitT (use_op, reason_op, tool, Rest (ReactConfigMerge pole, state), tout))

module type REACT = sig
  val run: Context.t -> Trace.t -> use_op:use_op -> reason -> Type.t -> Type.React.tool ->
  unit
end

module Kit (Flow: Flow_common.S): REACT = struct
  include Flow
  let sealed_in_op = Obj_type.sealed_in_op

  let run cx trace ~use_op reason_op l u =
    let err_incompatible reason = err_incompatible cx trace ~use_op ~add_output reason u
    in

    (* ReactKit can't stall, so even if `l` is an unexpected type, we must produce
       some outflow, usually some flavor of `any`, along with an error. However,
       not every unexpected inflow should cause an error. For example, `any`
       inflows shouldn't cause additional errors. Also, if we expect an array, but
       we get one without any static information, we should fall back without
       erroring. This is best-effort, after all. *)

    let coerce_object = function
      | DefT (reason, _, ObjT { props_tmap; dict_t; flags; _ }) ->
        Ok (reason, Context.find_props cx props_tmap, dict_t, flags)
      | AnyT (reason, _) ->
        Error reason
      | _ ->
        let reason = reason_of_t l in
        err_incompatible reason;
        Error reason
    in

    let coerce_prop_type = function
      | CustomFunT (reason, ReactPropType (PropType.Primitive (required, t))) ->
        let loc = aloc_of_reason reason in
        Ok (required, reposition cx ~trace loc t)
      | DefT (reason, _, FunT _) as t ->
        rec_flow_t cx trace (t,
          get_builtin_type cx reason_op "ReactPropsCheckType");
        Error reason
      | AnyT (reason, _) ->
        Error reason
      | t ->
        let reason = reason_of_t t in
        err_incompatible reason;
        Error reason
    in

    let coerce_array = function
      | DefT (_, _, ArrT (ArrayAT (_, Some ts) | TupleAT (_, ts))) ->
        Ok ts
      | DefT (reason, _, ArrT _) | AnyT (reason, _) ->
        Error reason
      | t ->
        let reason = reason_of_t t in
        err_incompatible reason;
        Error reason
    in

    (* Unlike other coercions, don't add a Flow error if the incoming type doesn't
       have a singleton type representation. *)
    let coerce_singleton = function
      | DefT (reason, trust, StrT (Literal (_, x))) ->
        let reason = replace_desc_reason (RStringLit x) reason in
        Ok (DefT (reason, trust, SingletonStrT x))

      | DefT (reason, trust, NumT (Literal (_, x))) ->
        let reason = replace_desc_reason (RNumberLit (snd x)) reason in
        Ok (DefT (reason, trust, SingletonNumT x))

      | DefT (reason, trust, BoolT (Some x)) ->
        let reason = replace_desc_reason (RBooleanLit x) reason in
        Ok (DefT (reason, trust, SingletonBoolT x))
      | DefT (_, _, NullT) | DefT (_, _, VoidT) as t ->
        Ok t
      | t ->
        Error (reason_of_t t)
    in

    let get_intrinsic = get_intrinsic cx trace l ~reason_op ~rec_flow ~get_builtin_type
    in

    (* This function creates a constraint *from* tin *to* props so that props is
     * an upper bound on tin. This is important because when the type of a
     * component's props is inferred (such as when a stateless functional
     * component has an unannotated props argument) we want to create a constraint
     * *from* the props input *to* tin which should then be propagated to the
     * inferred props type. *)
    let tin_to_props tin =
      let component = l in
      match component with
      (* Class components or legacy components. *)
      | DefT (_, _, ClassT _) ->
        (* The Props type parameter is invariant, but we only want to create a
         * constraint tin <: props. *)
        let props = Tvar.mk cx reason_op in
        rec_flow_t cx trace (tin, props);
        rec_flow cx trace (component, ReactInToProps (reason_op, props))

      (* Stateless functional components. *)
      | DefT (_, _, FunT _)
      (* Stateless functional components, again. This time for callable `ObjT`s. *)
      | DefT (_, _, ObjT { call_t = Some _; _ }) ->
        rec_flow cx trace (component, ReactInToProps (reason_op, tin))

      (* Abstract components. *)
      | DefT (reason, trust, ReactAbstractComponentT _) ->
        rec_flow_t cx trace (tin, MixedT.why reason trust);

      (* Intrinsic components. *)
      | DefT (_, _, StrT lit) -> get_intrinsic `Props lit (Field (None, tin, Polarity.Negative))

      | AnyT (reason, source) ->
        rec_flow_t cx trace (tin, AnyT.why source reason)

      (* ...otherwise, error. *)
      | _ -> err_incompatible (reason_of_t component)
    in

    let props_to_tout =
      props_to_tout cx trace l ~use_op ~reason_op ~rec_flow_t ~rec_flow
      ~get_builtin_type ~add_output u
    in

    let coerce_children_args (children, children_spread) =
      match children, children_spread with
      (* If we have no children and no variable spread argument then React will
       * not pass in any value for children. *)
      | [], None -> None
      (* If we know that we have exactly one argument and no variable spread
       * argument then React will pass in that single value. Notable we do not
       * wrap the type in an array as React returns the single value. *)
      | t::[], None -> Some t
      (* If we have two or more known arguments and no spread argument then we
       * want to create a tuple array type for our children. *)
      | t::ts, None ->
        (* Create a reason where the location is between our first and last known
         * argument. *)
        let r = mk_reason RReactChildren (match use_op with
        | Op (ReactCreateElementCall {children; _}) -> children
        | _ -> aloc_of_reason reason_op)
        in
        Some (DefT (r, bogus_trust (), ArrT (ArrayAT (union_of_ts r (t::ts), Some (t::ts)))))
      (* If we only have a spread of unknown length then React may not pass in
       * children, React may pass in a single child, or React may pass in an array
       * of children. We need to model all of these possibilities. *)
      | [], Some spread ->
        let r = replace_reason
          (fun desc -> RReactChildrenOrUndefinedOrType desc)
          (reason_of_t spread)
        in
        Some (OptionalT (r,
          union_of_ts r [
            spread;
            (DefT (r, bogus_trust (), ArrT (ArrayAT (spread, None))));
          ]
        ))
      (* If we have one children argument and a spread of unknown length then
       * React may either pass in the unwrapped argument, or an array where the
       * element type is the union of the known argument and the spread type. *)
      | t::[], Some spread ->
        (* Create a reason between our known argument and the spread argument. *)
        let r = mk_reason
          (RReactChildrenOrType (t |> reason_of_t |> desc_of_reason))
          (match use_op with
          | Op (ReactCreateElementCall {children; _}) -> children
          | _ -> aloc_of_reason reason_op)
        in
        Some (union_of_ts r [
          t;
          (DefT (r, bogus_trust (), ArrT (ArrayAT (union_of_ts r [spread; t], Some [t]))))
        ])
      (* If we have two or more arguments and a spread argument of unknown length
       * then we want to return an array type where the element type is the union
       * of all argument types and the spread argument type. *)
      | t::ts, Some spread ->
        (* Create a reason between our known argument and the spread argument. *)
        let r = mk_reason RReactChildren (match use_op with
        | Op (ReactCreateElementCall {children; _}) -> children
        | _ -> aloc_of_reason reason_op)
        in
        Some (DefT (r, bogus_trust (), ArrT (ArrayAT (union_of_ts r (spread::t::ts), Some (t::ts)))))
    in

    let config_check clone config children_args =
      (* Create the optional children input type from the children arguments. *)
      let children = coerce_children_args children_args in
      (* Create a type variable for our props. *)
      (* If we are cloning an existing element, the config does not need to
       * provide the entire props type. *)
      let props, defaults  =
        match l with
        | DefT (_, _, ReactAbstractComponentT {config; _}) ->
            (* This is a bit of a hack. We will be passing these props and
             * default props to react_config in flow_js.ml to calculate the
             * config and check the passed config against it. Since our config is
             * already calculated, we can pretend the props type is the config
             * type and that we have no defaultProps for identical behavior.
             *
             * This hack is necessary because we (by design) do not calculate
             * props from Config and DefaultProps. Even if we did do that, we would
             * just introduce unnecessary work here-- we would calculate the props from
             * the config and defaultProps just so that we could re-calculate the config
             * down the line.
             *
             * Additionally, this hack enables us to not have to explicitly handle
             * AbstractComponent past this point. *)
            (if clone
            then ShapeT (config)
            else config), None
        | _ ->
          (if clone
          then ShapeT (Tvar.mk_where cx reason_op props_to_tout)
          else Tvar.mk_where cx reason_op tin_to_props),
          (* For class components and function components we want to lookup the
           * static default props property so that we may add it to our config input. *)
          get_defaults cx trace l ~reason_op ~rec_flow
      in
      (* Use object spread to add children to config (if we have children)
       * and remove key and ref since we already checked key and ref. Finally in
       * this block we will flow the final config to our props type.
       *
       * NOTE: We don't eagerly run this check so that create_element can constrain the
       * ref and key pseudoprops before we run the config check.
       *)
      let open Object in
      let open Object.ReactConfig in
      (* We need to treat config input as a literal here so we ensure it has the
       * RReactProps reason description. *)
      let reason = replace_desc_new_reason RReactProps (reason_of_t config) in
      (* Create the final config object using the ReactConfig object kit tool
       * and flow it to our type for props.
       *
       * We wrap our use_op in a ReactConfigCheck frame to increment the
       * speculation error message score. Usually we will already have a
       * ReactCreateElementCall use_op, but we want errors after this point to
       * win when picking the best errors speculation discovered. *)
      let use_op = Frame (ReactConfigCheck, use_op) in
      rec_flow cx trace (config,
        ObjKitT (use_op, reason, Resolve Next,
          ReactConfig (Config { defaults; children }), props))
    in

    let create_element clone component config children_args tout =
      config_check clone config children_args;

      (* If our config is void or null then we want to replace it with an
       * empty object.
       *
       * NOTE: We only need the normalized config to look up the key
       * and ref.
       *)
      let normalized_config = Tvar.mk_where cx (reason_of_t config) (fun normalized_config ->
        let open Object in
        let reason = (reason_of_t config) in
        rec_flow cx trace (config,
          ObjKitT (use_op, reason, Resolve Next, ObjectRep, normalized_config))
      ) in

      (* Check the type of React keys in the config input.
       *
       * NOTE: We are intentionally being unsound here. If config is inexact
       * and we can't find a key prop in config then the sound thing to do
       * would be to assume that the type of key is mixed. Instead we are unsound
       * and don't check a type for key. Otherwise we would cause a lot of issues
       * in existing React code. *)
      let () =
        let reason_key =
          (replace_desc_reason (RCustom "React key") (reason_of_t normalized_config)) in
        (* Create the key type. *)
        let key_t = optional (maybe (get_builtin_type cx reason_key "React$Key")) in
        (* Flow the config input key type to the key type. *)
        let kind = NonstrictReturning (None, None) in
        let propref = Named (reason_key, "key") in
        let use_op = Frame (PropertyCompatibility {
          prop = Some "key";
          lower = reason_of_t normalized_config;
          upper = reason_key;
        }, use_op) in
        let action = LookupProp (use_op, Field (None, key_t, Polarity.Positive)) in
        rec_flow cx trace (normalized_config,
          LookupT (reason_key, kind, [], propref, action))
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
          (replace_desc_reason (RCustom "React ref") (reason_of_t normalized_config)) in
        (* Create the ref type. *)
        let ref_t = optional (maybe (get_builtin_typeapp cx reason_ref "React$Ref" [l])) in
        (* Flow the config input ref type to the ref type. *)
        let kind = NonstrictReturning (None, None) in
        let propref = Named (reason_ref, "ref") in
        let use_op = Frame (PropertyCompatibility {
          prop = Some "ref";
          lower = reason_of_t normalized_config;
          upper = reason_ref;
        }, use_op) in
        let action = LookupProp (use_op, Field (None, ref_t, Polarity.Positive)) in
        rec_flow cx trace (normalized_config,
          LookupT (reason_ref, kind, [], propref, action))
      in

      let elem_reason = annot_reason (replace_desc_reason (RType "React$Element") reason_op) in
      rec_flow_t cx trace (
        get_builtin_typeapp cx ~trace elem_reason "React$Element" [component],
        tout
      )
    in

    let get_config = get_config cx trace l ~use_op ~reason_op ~rec_flow ~rec_flow_t ~rec_unify
      ~get_builtin_type ~add_output u Polarity.Positive
    in

    let get_config_with_props_and_defaults default_props tout =
      let open Object in
      let open Object.Rest in
      let props = l in
      let tool = Resolve Next in
      let state = One default_props in
      rec_flow cx trace (props, ObjKitT (
        Op UnknownUse,
        reason_op,
        tool,
        Rest (ReactConfigMerge Polarity.Neutral, state),
        tout
      ))
    in


    let get_instance tout =
      let component = l in
      match component with
      (* Class components or legacy components. *)
      | DefT (_, _, ClassT component) -> rec_flow_t cx trace (component, tout)

      (* Stateless functional components. *)
      | DefT (r, trust, FunT _) ->
        rec_flow_t cx trace (VoidT.make (replace_desc_reason RVoid r) trust, tout)

      (* Stateless functional components, again. This time for callable `ObjT`s. *)
      | DefT (r, trust, ObjT { call_t = Some _; _ }) ->
        rec_flow_t cx trace (VoidT.make (replace_desc_reason RVoid r) trust, tout)

      (* Abstract components. *)
      | DefT (_, _, ReactAbstractComponentT {instance; _}) ->
        rec_flow_t cx trace (instance, tout);

      (* Intrinsic components. *)
      | DefT (_, _, StrT lit) -> get_intrinsic `Instance lit (Field (None, tout, Polarity.Positive))

      | AnyT (reason, source) ->
        rec_flow_t cx trace (AnyT.why source reason, tout)

      (* ...otherwise, error. *)
      | _ -> err_incompatible (reason_of_t component)
    in

    (* In order to create a useful type from the `propTypes` property of a React
       class specification, Flow needs the ReactPropType CustomFunT type. This
       tool evaluates a complex prop type such that a specific CustomFunT is
       returned when there is enough static information. *)

    let simplify_prop_type tout =
      let resolve t = rec_flow_t cx trace (
        CustomFunT (reason_op, ReactPropType (PropType.Primitive (false, t))),
        tout
      ) in

      let mk_union reason = function
        | [] -> DefT (replace_desc_reason REmpty reason, bogus_trust (), EmptyT Bottom)
        | [t] -> t
        | t0::t1::ts ->
          let reason = replace_desc_reason RUnionType reason in
          UnionT (reason, UnionRep.make t0 t1 ts)
      in

      let open SimplifyPropType in
      function
      | ArrayOf ->
        (* TODO: Don't ignore the required flag. *)
        let elem_t = match coerce_prop_type l with
          | Ok (_required, t) -> t
          | Error reason -> AnyT.make AnyError reason
        in
        let reason = replace_desc_reason RArrayType reason_op in
        let t = DefT (reason, bogus_trust (), ArrT (ArrayAT (elem_t, None))) in
        resolve t

      | InstanceOf ->
        let t = mk_instance cx (annot_reason reason_op) l in
        resolve t

      | ObjectOf ->
        (* TODO: Don't ignore the required flag. *)
        let value = match coerce_prop_type l with
          | Ok (_required, t) -> t
          | Error reason -> AnyT.make AnyError reason
        in
        let props = SMap.empty in
        let dict = {
          dict_name = None;
          key = tout |> reason_of_t |> StrT.why |> with_trust bogus_trust;
          value;
          dict_polarity = Polarity.Neutral;
        } in
        let proto = ObjProtoT (locationless_reason RObjectClassName) in
        let reason = replace_desc_reason RObjectType reason_op in
        let t = Obj_type.mk_with_proto cx reason ~props proto
          ~dict ~sealed:true ~exact:false in
        resolve t

      | OneOf tool ->
        let next todo done_rev = match todo with
          | [] ->
            let t = mk_union reason_op (List.rev done_rev) in
            resolve t
          | t::todo ->
            rec_flow cx trace (t, ReactKitT (unknown_use, reason_op,
              SimplifyPropType (OneOf
                (ResolveElem (todo, done_rev)), tout)))
        in
        (match tool with
        | ResolveArray ->
          (match coerce_array l with
          | Ok todo -> next todo []
          | Error _ -> AnyT.make AnyError reason_op |> resolve)
        | ResolveElem (todo, done_rev) ->
          (match coerce_singleton l with
          | Ok t -> next todo (t::done_rev)
          | Error _ -> AnyT.make AnyError reason_op |> resolve))

      | OneOfType tool ->
        (* TODO: This is _very_ similar to `one_of` above. *)
        let next todo done_rev = match todo with
          | [] ->
            let t = mk_union reason_op (List.rev done_rev) in
            resolve t
          | t::todo ->
            rec_flow cx trace (t, ReactKitT (unknown_use, reason_op,
              SimplifyPropType (OneOfType
                (ResolveElem (todo, done_rev)), tout)))
        in
        (match tool with
        | ResolveArray ->
          (match coerce_array l with
          | Ok todo -> next todo []
          | Error _ -> AnyT.make AnyError reason_op |> resolve)
        | ResolveElem (todo, done_rev) ->
          (* TODO: Don't ignore the required flag. *)
          (match coerce_prop_type l with
          | Ok (_required, t) -> next todo (t::done_rev)
          | Error _ -> AnyT.make AnyError reason_op |> resolve))

      | Shape tool ->
        (* TODO: This is _very_ similar to `CreateClass.PropTypes` below, except
           for reasons descriptions/locations, recursive ReactKit constraints, and
           `resolve` behavior. *)
        let add_prop k t (reason, props, dict, flags) =
          let props = SMap.add k (Field (None, t, Polarity.Neutral)) props in
          reason, props, dict, flags
        in
        let add_dict dict (reason, props, _, flags) =
          reason, props, Some dict, flags
        in
        let rec next todo shape =
          match SMap.choose todo with
          | None ->
            let reason = replace_desc_reason RObjectType reason_op in
            let proto = ObjProtoT (locationless_reason RObjectClassName) in
            let _, props, dict, _ = shape in
            let t = Obj_type.mk_with_proto cx reason ~props proto
              ?dict ~sealed:true ~exact:false
            in
            resolve t
          | Some (k, p) ->
            let todo = SMap.remove k todo in
            match Property.read_t p with
            | None -> next todo shape
            | Some t ->
              rec_flow cx trace (t, ReactKitT (unknown_use, reason_op,
                SimplifyPropType (Shape
                  (ResolveProp (k, todo, shape)), tout)))
        in
        (match tool with
        | ResolveObject ->
          (match coerce_object l with
          (* TODO: If the resolved object is not exact and sealed, or if it does
           * not have a dictionary -- that is, it may be wider in an unknown way,
           * we should error and resolve to any. However, since all object spreads
           * are currently unsealed, we must wait for precise spread support.
           * Otherwise, we will cause too many spurious errors. *)
          | Ok (reason, todo, dict, flags) ->
            let shape = reason, SMap.empty, None, flags in
            (match dict with
            | None -> next todo shape
            | Some dicttype ->
              rec_flow cx trace (dicttype.value, ReactKitT (unknown_use, reason_op,
                SimplifyPropType (Shape
                  (ResolveDict (dicttype, todo, shape)), tout))))
          | Error _ -> AnyT.make AnyError reason_op |> resolve)
        | ResolveDict (dicttype, todo, shape) ->
          let dict = match coerce_prop_type l with
          | Ok (_, t) -> {dicttype with value = t}
          | Error reason -> {dicttype with value = AnyT.make AnyError reason}
          in
          next todo (add_dict dict shape)
        | ResolveProp (k, todo, shape) ->
          let t = match coerce_prop_type l with
          | Ok (required, t) -> if required then t else Type.optional t
          | Error _ -> AnyT.make AnyError reason_op |> Type.optional
          in
          next todo (add_prop k t shape))
    in

    let create_class knot tout =
      let open CreateClass in

      let maybe_known_of_result = function
        | Ok x -> Known x
        | Error e -> Unknown e
      in

      let map_known f = function
        | Known x -> Known (f x)
        | Unknown e -> Unknown e
      in

      let get_prop x (_, props, dict, _) =
        match SMap.get x props with
        | Some _ as p -> p
        | None ->
          Option.map dict (fun { key; value; dict_polarity; _ } ->
            rec_flow_t cx trace (string_key x reason_op, key);
            Field (None, value, dict_polarity))
      in

      let read_prop x obj = Option.bind (get_prop x obj) Property.read_t in

      let read_stack x ((obj, _), _) = read_prop x obj in

      let map_spec f ((obj, spec), tail) = ((obj, f spec), tail) in

      (* This tool recursively resolves types until the spec is resolved enough to
       * compute the instance type. `resolve` and `resolve_call` actually emit the
       * recursive constraints. The latter is for `getInitialState` and
       * `getDefaultProps`, where the type we want to resolve is the return type
       * of the bound function call *)

      let resolve tool t =
        rec_flow cx trace (t, ReactKitT (unknown_use, reason_op,
          CreateClass (tool, knot, tout)))
      in

      let resolve_call this tool t =
        let reason = reason_of_t t in
        let return_t = Tvar.mk cx reason in
        let funcall = mk_methodcalltype this None [] return_t in
        rec_flow cx trace (t, CallT (unknown_use, reason, funcall));
        resolve tool return_t
      in

      let merge_nullable f a b =
        match a, b with
        | NotNull a, NotNull b -> NotNull (f a b)
        | Null _, x | x, Null _ -> x
      in

      let merge_unknown f a b =
        match a, b with
        | Known a, Known b -> Known (f a b)
        | Unknown r, _ | _, Unknown r -> Unknown r
      in

      let merge_flags a b =
        let { frozen = f1; sealed = s1; exact = e1 } = a in
        let { frozen = f2; sealed = s2; exact = e2 } = b in
        let frozen = f1 && f2 in
        let exact = e1 && e2 in
        let sealed =
          let s1 = sealed_in_op reason_op s1 in
          let s2 = sealed_in_op reason_op s2 in
          if exact && not (s1 || s2)
          then UnsealedInFile (ALoc.source (aloc_of_reason reason_op))
          else Sealed
        in
        { frozen; exact; sealed }
      in

      let merge_objs (r1, ps1, dict1, flags1) (_, ps2, dict2, flags2) =
        let props = SMap.union ps1 ps2 in
        let dict = Option.first_some dict1 dict2 in
        let flags = merge_flags flags1 flags2 in
        (r1, props, dict, flags)
      in

      (* When a type is resolved, we move on to the next field. If the field is
       * not found on the spec, we skip ahead. Otherwise we emit a constraint to
       * resolve the property type. *)

      let rec on_resolve_spec stack =
        match read_stack "mixins" stack with
        | None -> on_resolve_mixins stack
        | Some t -> resolve (Mixins stack) t

      and on_resolve_mixins stack =
        match read_stack "statics" stack with
        | None -> on_resolve_statics stack
        | Some t -> resolve (Statics stack) t

      and on_resolve_statics stack =
        match read_stack "propTypes" stack with
        | None -> on_resolve_prop_types stack
        | Some t -> resolve (PropTypes (stack, ResolveObject)) t

      and on_resolve_prop_types stack =
        match stack with
        | (_, spec), [] ->
          (* Done resolving class spec and mixin specs. *)
          on_resolve_default_props None spec.get_default_props;
          on_resolve_initial_state None spec.get_initial_state;
          mk_class spec
        | (_, mixin), ((obj, spec), todo, mixins_rev)::stack' ->
          (* Done resolving a mixin *)
          let mixins_rev = Known mixin :: mixins_rev in
          match todo with
          | [] ->
            (* No more mixins, resume parent stack with accumulated mixins *)
            let stack = (obj, flatten_mixins mixins_rev spec), stack' in
            on_resolve_mixins stack
          | t::todo ->
            (* Resolve next mixin in parent's mixin list *)
            let stack' = ((obj, spec), todo, mixins_rev)::stack' in
            resolve (Spec stack') t

      and on_resolve_default_props acc = function
        | [] ->
          let t = match acc with
          | None ->
            let reason = replace_desc_reason RReactDefaultProps reason_op in
            VoidT.make reason (bogus_trust ())
          | Some (Unknown reason) -> AnyT.make Untyped reason
          | Some (Known (reason, props, dict, _)) ->
            Obj_type.mk_with_proto cx reason ~props (ObjProtoT reason)
              ?dict ~sealed:true ~exact:false
          in
          rec_flow_t cx trace (t, knot.default_t)
        | t::todo ->
          let tool = DefaultProps (todo, acc) in
          resolve_call knot.static tool t

      and on_resolve_initial_state acc = function
        | [] ->
          let t = match acc with
          | None ->
            let reason = replace_desc_reason RReactState reason_op in
            Obj_type.mk cx reason
          | Some (Unknown reason) -> AnyT.make Untyped reason
          | Some (Known (Null reason)) -> DefT (reason, bogus_trust (), NullT)
          | Some (Known (NotNull (reason, props, dict, { exact; sealed; _ }))) ->
            let sealed = not (exact && sealed_in_op reason_op sealed) in
            Obj_type.mk_with_proto cx reason ~props (ObjProtoT reason)
              ?dict ~sealed ~exact
          in
          rec_flow_t cx trace (t, knot.state_t)
        | t::todo ->
          let tool = InitialState (todo, acc) in
          resolve_call knot.this tool t

      and flatten_mixins mixins_rev spec =
        List.fold_right (fun mixin acc ->
          match mixin with
          | Known spec ->
            merge_specs acc spec
          | Unknown reason ->
            {acc with unknown_mixins = reason::acc.unknown_mixins}
        ) mixins_rev spec

      and merge_statics =
        Option.merge ~f:(merge_unknown merge_objs)

      and merge_prop_types =
        Option.merge ~f:(merge_unknown merge_objs)

      and merge_default_props =
        Option.merge ~f:(merge_unknown merge_objs)

      and merge_initial_state =
        Option.merge ~f:(merge_unknown (merge_nullable merge_objs))

      and merge_specs a b = {
        obj = merge_objs a.obj b.obj;
        statics = merge_statics a.statics b.statics;
        prop_types = merge_prop_types a.prop_types b.prop_types;
        get_default_props = a.get_default_props @ b.get_default_props;
        get_initial_state = a.get_initial_state @ b.get_initial_state;
        unknown_mixins = a.unknown_mixins @ b.unknown_mixins;
      }

      and mk_class spec =
        (* If the component doesn't specify propTypes, allow anything. To be
           stricter, we could use an empty object type, but that would require all
           components to specify propTypes *)
        let props_t = match spec.prop_types with
        | None -> AnyT.make Untyped reason_op
        | Some (Unknown reason) -> AnyT.make Untyped reason
        | Some (Known (reason, props, dict, _)) ->
          Obj_type.mk_with_proto cx reason ~props (ObjProtoT reason)
            ?dict ~sealed:true ~exact:false
        in
        let props_t =
          mod_reason_of_t (replace_desc_reason RReactPropTypes) props_t
        in

        let props =
          SMap.empty
          |> SMap.add "props" (Field (None, props_t, Polarity.Neutral))
          |> SMap.add "state" (Field (None, knot.state_t, Polarity.Neutral))
        in

        (* Some spec fields are used to create the instance type, but are not
           present on the resulting prototype or statics. Other spec fields should
           become static props. Everything else should be on the prototype. *)
        let _, spec_props, _, _ = spec.obj in
        let props, static_props = SMap.fold (fun k v (props, static_props) ->
          match k with
          | "autobind"
          | "mixins"
          | "statics" ->
            props, static_props

          | "childContextTypes"
          | "contextTypes"
          | "displayName"
          | "getDefaultProps"
          | "propTypes" ->
            let v = match Property.read_t v with
            | None -> v
            | Some t ->
              let loc = Property.read_loc v in
              Field (loc, t, Polarity.Positive)
            in
            props, SMap.add k v static_props

          (* Don't autobind ReactClassInterface props, like getInitialState.
             Instead, call with the correct this when resolving types. *)
          | "getInitialState"
          | "getChildContext"
          | "render"
          | "componentWillMount"
          | "componentDidMount"
          | "componentWillReceiveProps"
          | "shouldComponentUpdate"
          | "componentWillUpdate"
          | "componentDidUpdate"
          | "componentWillUnmount"
          | "updateComponent" ->
            let loc = Property.read_loc v in
            let v = match Property.read_t v with
            | None -> v
            | Some t ->
              (* Tie the `this` knot with BindT *)
              let dummy_return = MixedT.make reason_op |> with_trust bogus_trust in
              let calltype = mk_methodcalltype knot.this None [] dummy_return in
              rec_flow cx trace (t, BindT (unknown_use, reason_op, calltype, true));
              (* Because we are creating an instance type, which can be used as an
                 upper bound (e.g., as a super class), it's more flexible to
                 create covariant methods. Otherwise, a subclass could not
                 override the `render` method, say. *)
              Method (loc, t)
            in
            SMap.add k v props, static_props

          | _ ->
            let bound_v = Property.map_t (fun t ->
              let use_op = unknown_use in
              let destructor = Bind knot.this in
              let id = mk_id () in
              ignore (mk_type_destructor cx ~trace use_op reason_op t destructor id);
              EvalT (t, TypeDestructorT (use_op, reason_op, destructor), id)
            ) v in
            SMap.add k bound_v props, static_props
        ) spec_props (props, SMap.empty) in

        let static_props = static_props
          |> SMap.add "defaultProps" (Field (None, knot.default_t, Polarity.Neutral))
        in

        let reason_component = replace_desc_reason RReactComponent reason_op in

        let super =
          let reason = replace_reason (fun x -> RSuperOf x) reason_component in
          let c = get_builtin cx "LegacyReactComponent" reason in
          this_typeapp c knot.this (Some [props_t; knot.state_t])
        in

        let static =
          let reason, props, dict, exact, sealed = match spec.statics with
          | None ->
            reason_op, static_props, None, true, false
          | Some (Unknown reason) ->
            let dict = Some {
              dict_name = None;
              key = StrT.why reason (bogus_trust ());
              value = EmptyT.why reason (bogus_trust ());
              dict_polarity = Polarity.Neutral;
            } in
            reason, static_props, dict, false, true
          | Some (Known (reason, props, dict, { exact; sealed; _ })) ->
            let static_props = SMap.union props static_props in
            let sealed = not (exact && sealed_in_op reason_op sealed) in
            reason, static_props, dict, exact, sealed
          in
          let reason = replace_desc_reason RReactStatics reason in
          Obj_type.mk_with_proto cx reason ~props (class_type super)
            ?dict ~exact ~sealed
        in

        let insttype = {
          class_id = ALoc.none;
          type_args = [];
          (* TODO: props are actually installed on the prototype *)
          own_props = Context.generate_property_map cx props;
          proto_props = Context.generate_property_map cx SMap.empty;
          initialized_fields = SSet.empty;
          initialized_static_fields = SSet.singleton "propTypes";
          inst_call_t = None;
          has_unknown_react_mixins = spec.unknown_mixins <> [];
          inst_kind = ClassKind;
        } in
        rec_flow cx trace (super, SuperT (use_op, reason_op, Derived {
          own = props;
          proto = SMap.empty;
          (* TODO: check static signature against base class *)
          static = SMap.empty;
        }));

        let instance = DefT (reason_component, bogus_trust (), InstanceT (static, super, [], insttype)) in
        rec_flow_t cx trace (instance, knot.this);
        rec_flow_t cx trace (static, knot.static);
        rec_flow_t cx trace (class_type instance, tout)
      in

      let empty_spec obj = {
        obj;
        statics = None;
        prop_types = None;
        get_default_props = Option.to_list (read_prop "getDefaultProps" obj);
        get_initial_state = Option.to_list (read_prop "getInitialState" obj);
        unknown_mixins = [];
      } in

      function
      | Spec stack' ->
        let result = match coerce_object l with
        | Ok (reason, _, _, { exact; sealed; _ })
          when not (exact && sealed_in_op reason_op sealed) ->
          err_incompatible reason;
          Error reason
        | result -> result
        in
        (match result with
        | Ok obj ->
          on_resolve_spec ((obj, empty_spec obj), stack')
        | Error reason ->
          (match stack' with
          | [] ->
            (* The root spec is unknown *)
            rec_flow_t cx trace (AnyT.error reason_op, tout)
          | ((obj, spec), todo, mixins_rev)::stack' ->
            (* A mixin is unknown *)
            let mixins_rev = Unknown reason :: mixins_rev in
            (match todo with
            | [] ->
              (* No more mixins, resume parent stack with accumulated mixin *)
              let stack = (obj, flatten_mixins mixins_rev spec), stack' in
              on_resolve_mixins stack
            | t::todo ->
              (* Resolve next mixin in parent's mixin list *)
              let stack' = ((obj, spec), todo, mixins_rev)::stack' in
              resolve (Spec stack') t)))

      | Mixins stack ->
        (match coerce_array l with
        | Error reason ->
          let stack = map_spec (fun spec -> {
            spec with
            unknown_mixins = reason::spec.unknown_mixins
          }) stack in
          on_resolve_mixins stack
        | Ok [] -> on_resolve_mixins stack
        | Ok (t::todo) ->
          (* We need to resolve every mixin before we can continue resolving this
           * spec. Push the stack and start resolving the first mixin. Once the
           * mixins are done, we'll pop the stack and continue. *)
          let head, tail = stack in
          let tail = (head, todo, [])::tail in
          resolve (Spec tail) t)

      | Statics stack ->
        let statics = Some (maybe_known_of_result (coerce_object l)) in
        map_spec (fun spec -> {
          spec with
          statics = merge_statics statics spec.statics
        }) stack |> on_resolve_statics

      | PropTypes (stack, tool) ->
        let add_prop k t (reason, props, dict, flags) =
          let props = SMap.add k (Field (None, t, Polarity.Neutral)) props in
          reason, props, dict, flags
        in
        let add_dict dict (reason, props, _, flags) =
          reason, props, Some dict, flags
        in
        let rec next todo prop_types =
          match SMap.choose todo with
          | None ->
            let prop_types = Some (Known prop_types) in
            map_spec (fun spec -> {
              spec with
              prop_types = merge_prop_types prop_types spec.prop_types
            }) stack |> on_resolve_prop_types
          | Some (k, p) ->
            let todo = SMap.remove k todo in
            match Property.read_t p with
            | None -> next todo prop_types
            | Some t ->
              let tool = PropTypes (stack,
                ResolveProp (k, todo, prop_types)) in
              resolve tool t
        in
        (match tool with
        | ResolveObject ->
          (match coerce_object l with
          (* TODO: If the resolved object is not exact and sealed, or if it does
           * not have a dictionary -- that is, it may be wider in an unknown way,
           * we should error and resolve to any. However, since all object spreads
           * are currently unsealed, we must wait for precise spread support.
           * Otherwise, we will cause too many spurious errors. *)
          | Ok (reason, todo, dict, flags) ->
            let prop_types = reason, SMap.empty, None, flags in
            (match dict with
            | None -> next todo prop_types
            | Some dicttype ->
              let tool = PropTypes (stack,
                ResolveDict (dicttype, todo, prop_types)) in
              resolve tool dicttype.value)
          | Error reason ->
            let prop_types = Some (Unknown reason) in
            map_spec (fun spec -> {
              spec with
              prop_types = merge_prop_types prop_types spec.prop_types
            }) stack |> on_resolve_prop_types)
        | ResolveDict (dicttype, todo, prop_types) ->
          let dict = match coerce_prop_type l with
          | Ok (_, t) -> {dicttype with value = t}
          | Error reason -> {dicttype with value = AnyT.make AnyError reason}
          in
          next todo (add_dict dict prop_types)
        | ResolveProp (k, todo, prop_types) ->
          let t = match coerce_prop_type l with
          | Ok (required, t) -> if required then t else Type.optional t
          | Error reason -> AnyT.make AnyError reason |> Type.optional
          in
          next todo (add_prop k t prop_types))

      | DefaultProps (todo, acc) ->
        let default_props = Some (maybe_known_of_result (coerce_object l)) in
        let acc = merge_default_props default_props acc in
        on_resolve_default_props acc todo

      | InitialState (todo, acc) ->
        let initial_state = Some (match l with
        | DefT (reason, _, NullT) -> Known (Null reason)
        | _ ->
          coerce_object l
          |> maybe_known_of_result
          |> map_known (fun x -> NotNull x)
        ) in
        let acc = merge_initial_state initial_state acc in
        on_resolve_initial_state acc todo
    in

    match u with
    | CreateElement0 _ -> failwith "handled elsewhere"
    | CreateElement (clone, component, config, children, tout) ->
      create_element clone component config children tout
    | ConfigCheck config -> config_check false config ([], None)
    | GetProps tout -> props_to_tout tout
    | GetConfig tout -> get_config tout
    | GetConfigType (default_props, tout) -> get_config_with_props_and_defaults default_props tout
    | GetRef tout -> get_instance tout
    | SimplifyPropType (tool, tout) -> simplify_prop_type tout tool
    | CreateClass (tool, knot, tout) -> create_class knot tout tool
end
