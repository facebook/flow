(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js
open Reason
open Type
open React

let run cx trace reason_op l u
  ~(add_output: Context.t -> ?trace:Trace.t -> Flow_error.error_message -> unit)
  ~(reposition: Context.t -> ?trace:Trace.t -> Loc.t -> Type.t -> Type.t)
  ~(rec_flow: Context.t -> Trace.t -> (Type.t * Type.use_t) -> unit)
  ~(rec_flow_t: Context.t -> Trace.t -> ?use_op:Type.use_op -> (Type.t * Type.t) -> unit)
  ~(get_builtin_type: Context.t -> ?trace:Trace.t -> reason -> string -> Type.t)
  ~(get_builtin_typeapp: Context.t -> ?trace:Trace.t -> reason -> string -> Type.t list -> Type.t)
  ~(mk_functioncalltype: Type.call_arg list -> ?frame:int -> ?call_strict_arity:bool -> Type.t -> Type.funcalltype)
  ~(mk_methodcalltype: Type.t -> Type.call_arg list -> ?frame:int -> ?call_strict_arity:bool -> Type.t -> Type.funcalltype)
  ~(mk_instance: Context.t -> ?trace:Trace.t -> reason -> ?for_type:bool -> Type.t -> Type.t)
  ~(mk_object: Context.t -> reason -> Type.t)
  ~(mk_object_with_map_proto: Context.t -> reason -> ?sealed:bool -> ?exact:bool -> ?frozen:bool -> ?dict:Type.dicttype -> Type.Properties.t -> Type.t -> Type.t)
  ~(string_key: string -> reason -> Type.t)
  ~(mk_tvar: Context.t -> reason -> Type.t)
  ~(eval_destructor: Context.t -> trace:Trace.t -> reason -> t -> Type.destructor -> int -> Type.t)
  ~(sealed_in_op: reason -> Type.sealtype -> bool)
  =

  let err_incompatible reason =
    add_output cx ~trace (Flow_error.EReactKit
      ((reason_op, reason), u))
  in

  (* ReactKit can't stall, so even if `l` is an unexpected type, we must produce
     some outflow, usually some flavor of `any`, along with an error. However,
     not every unexpected inflow should cause an error. For example, `any`
     inflows shouldn't cause additional errors. Also, if we expect an array, but
     we get one without any static information, we should fall back without
     erroring. This is best-effort, after all. *)

  let coerce_object = function
    | DefT (reason, ObjT { props_tmap; dict_t; flags; _ }) ->
      OK (reason, Context.find_props cx props_tmap, dict_t, flags)
    | DefT (reason, AnyT) | DefT (reason, AnyObjT) ->
      Err reason
    | _ ->
      let reason = reason_of_t l in
      err_incompatible reason;
      Err reason
  in

  let coerce_prop_type = function
    | CustomFunT (reason, ReactPropType (PropType.Primitive (required, t))) ->
      let loc = loc_of_reason reason in
      OK (required, reposition cx ~trace loc t)
    | DefT (reason, FunT _) as t ->
      rec_flow_t cx trace (t,
        get_builtin_type cx reason_op "ReactPropsCheckType");
      Err reason
    | DefT (reason, AnyT) | DefT (reason, AnyFunT) ->
      Err reason
    | t ->
      let reason = reason_of_t t in
      err_incompatible reason;
      Err reason
  in

  let coerce_array = function
    | DefT (_, ArrT (ArrayAT (_, Some ts) | TupleAT (_, ts))) ->
      OK ts
    | DefT (reason, ArrT _) | DefT (reason, AnyT) ->
      Err reason
    | t ->
      let reason = reason_of_t t in
      err_incompatible reason;
      Err reason
  in

  (* Unlike other coercions, don't add a Flow error if the incoming type doesn't
     have a singleton type representation. *)
  let coerce_singleton = function
    | DefT (reason, StrT (Literal (_, x))) ->
      let reason = replace_reason_const (RStringLit x) reason in
      OK (DefT (reason, SingletonStrT x))

    | DefT (reason, NumT (Literal (_, x))) ->
      let reason = replace_reason_const (RNumberLit (snd x)) reason in
      OK (DefT (reason, SingletonNumT x))

    | DefT (reason, BoolT (Some x)) ->
      let reason = replace_reason_const (RBooleanLit x) reason in
      OK (DefT (reason, SingletonBoolT x))
    | DefT (_, NullT) | DefT (_, VoidT) as t ->
      OK t
    | t ->
      Err (reason_of_t t)
  in

  let create_element config tout =
    let elem_reason = replace_reason_const (RReactElement None) reason_op in
    (match l with
    | DefT (_, ClassT _) ->
      let react_class =
        get_builtin_typeapp cx ~trace reason_op "ReactClass" [config]
      in
      rec_flow_t cx trace (l, react_class)
    | DefT (_, FunT _) ->
      let return_t =
        get_builtin_typeapp cx ~trace elem_reason "React$Element"
          [Locationless.AnyT.t]
      in
      let return_t = DefT (elem_reason, MaybeT return_t) in
      let context_t =
        DefT (replace_reason_const (RCustom "context") elem_reason, AnyT) in
      let args = [Arg config; Arg context_t] in
      let funcalltype ={ (mk_functioncalltype args return_t) with
        call_strict_arity = false;
      } in
      let call_t = CallT (reason_op, funcalltype) in
      rec_flow cx trace (l, call_t)
    | DefT (_, StrT _)
    | DefT (_, SingletonStrT _) ->
      let jsx_intrinsics =
        get_builtin_type cx ~trace reason_op "$JSXIntrinsics" in
      rec_flow_t cx trace (l, KeysT (reason_op, jsx_intrinsics))
    | DefT (_, (AnyT | AnyFunT | AnyObjT)) -> ()
    | _ -> err_incompatible (reason_of_t l);
    );
    rec_flow_t cx trace (
      get_builtin_typeapp cx ~trace elem_reason "React$Element" [config],
      tout
    )
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
      | [] -> DefT (replace_reason_const REmpty reason, EmptyT)
      | [t] -> t
      | t0::t1::ts ->
        let reason = replace_reason_const RUnionType reason in
        DefT (reason, UnionT (UnionRep.make t0 t1 ts))
    in

    let open SimplifyPropType in
    function
    | ArrayOf ->
      (* TODO: Don't ignore the required flag. *)
      let elem_t = match coerce_prop_type l with
        | OK (_required, t) -> t
        | Err reason -> DefT (reason, AnyT)
      in
      let reason = replace_reason_const RArrayType reason_op in
      let t = DefT (reason, ArrT (ArrayAT (elem_t, None))) in
      resolve t

    | InstanceOf ->
      let t = mk_instance cx reason_op l in
      resolve t

    | ObjectOf ->
      (* TODO: Don't ignore the required flag. *)
      let value = match coerce_prop_type l with
        | OK (_required, t) -> t
        | Err reason -> DefT (reason, AnyT)
      in
      let props = SMap.empty in
      let dict = {
        dict_name = None;
        key = Locationless.AnyT.t;
        value;
        dict_polarity = Neutral;
      } in
      let proto = ObjProtoT (locationless_reason RObjectClassName) in
      let reason = replace_reason_const RObjectType reason_op in
      let t = mk_object_with_map_proto cx reason props proto
        ~dict ~sealed:true ~exact:false in
      resolve t

    | OneOf tool ->
      let next todo done_rev = match todo with
        | [] ->
          let t = mk_union reason_op (List.rev done_rev) in
          resolve t
        | t::todo ->
          rec_flow cx trace (t, ReactKitT (reason_op,
            SimplifyPropType (OneOf
              (ResolveElem (todo, done_rev)), tout)))
      in
      (match tool with
      | ResolveArray ->
        (match coerce_array l with
        | OK todo -> next todo []
        | Err _ -> resolve (DefT (reason_op, AnyT)))
      | ResolveElem (todo, done_rev) ->
        (match coerce_singleton l with
        | OK t -> next todo (t::done_rev)
        | Err _ -> resolve (DefT (reason_op, AnyT))))

    | OneOfType tool ->
      (* TODO: This is _very_ similar to `one_of` above. *)
      let next todo done_rev = match todo with
        | [] ->
          let t = mk_union reason_op (List.rev done_rev) in
          resolve t
        | t::todo ->
          rec_flow cx trace (t, ReactKitT (reason_op,
            SimplifyPropType (OneOfType
              (ResolveElem (todo, done_rev)), tout)))
      in
      (match tool with
      | ResolveArray ->
        (match coerce_array l with
        | OK todo -> next todo []
        | Err _ -> resolve (DefT (reason_op, AnyT)))
      | ResolveElem (todo, done_rev) ->
        (* TODO: Don't ignore the required flag. *)
        (match coerce_prop_type l with
        | OK (_required, t) -> next todo (t::done_rev)
        | Err _ -> resolve (DefT (reason_op, AnyT))))

    | Shape tool ->
      (* TODO: This is _very_ similar to `CreateClass.PropTypes` below, except
         for reasons descriptions/locations, recursive ReactKit constraints, and
         `resolve` behavior. *)
      let add_prop k t (reason, props, dict, flags) =
        let props = SMap.add k (Field (t, Neutral)) props in
        reason, props, dict, flags
      in
      let add_dict dict (reason, props, _, flags) =
        reason, props, Some dict, flags
      in
      let rec next todo shape =
        match SMap.choose todo with
        | None ->
          let reason = replace_reason_const RObjectType reason_op in
          let proto = ObjProtoT (locationless_reason RObjectClassName) in
          let _, props, dict, _ = shape in
          let t = mk_object_with_map_proto cx reason props proto
            ?dict ~sealed:true ~exact:false
          in
          resolve t
        | Some (k, p) ->
          let todo = SMap.remove k todo in
          match Property.read_t p with
          | None -> next todo shape
          | Some t ->
            rec_flow cx trace (t, ReactKitT (reason_op,
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
        | OK (reason, todo, dict, flags) ->
          let shape = reason, SMap.empty, None, flags in
          (match dict with
          | None -> next todo shape
          | Some dicttype ->
            rec_flow cx trace (dicttype.value, ReactKitT (reason_op,
              SimplifyPropType (Shape
                (ResolveDict (dicttype, todo, shape)), tout))))
        | Err _ -> resolve (DefT (reason_op, AnyT)))
      | ResolveDict (dicttype, todo, shape) ->
        let dict = match coerce_prop_type l with
        | OK (_, t) -> {dicttype with value = t}
        | Err reason -> {dicttype with value = DefT (reason, AnyT)}
        in
        next todo (add_dict dict shape)
      | ResolveProp (k, todo, shape) ->
        let t = match coerce_prop_type l with
        | OK (required, t) -> if required then t else Type.optional t
        | Err _ -> Type.optional (DefT (reason_op, AnyT))
        in
        next todo (add_prop k t shape))
  in

  let create_class knot tout =
    let open CreateClass in

    let maybe_known_of_result = function
      | OK x -> Known x
      | Err e -> Unknown e
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
          Field (value, dict_polarity))
    in

    let read_prop x obj = Option.bind (get_prop x obj) Property.read_t in

    let read_stack x ((obj, _), _) = read_prop x obj in

    let map_spec f ((obj, spec), tail) = ((obj, f spec), tail) in

    (* This tool recursively resolves types until the spec is resolved enough to
     * compute the instance type. `resolve` and `resolve_call` actually emit the
     * recursive constaints. The latter is for `getInitialState` and
     * `getDefaultProps`, where the type we want to resolve is the return type
     * of the bound function call *)

    let resolve tool t =
      rec_flow cx trace (t, ReactKitT (reason_op,
        CreateClass (tool, knot, tout)))
    in

    let resolve_call this tool t =
      let reason = reason_of_t t in
      let return_t = mk_tvar cx reason in
      let funcall = mk_methodcalltype this [] return_t in
      rec_flow cx trace (t, CallT (reason, funcall));
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
        then UnsealedInFile (Loc.source (loc_of_reason reason_op))
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
          let reason = replace_reason_const RReactDefaultProps reason_op in
          mk_object cx reason
        | Some (Unknown reason) -> DefT (reason, AnyObjT)
        | Some (Known (reason, props, dict, _)) ->
          mk_object_with_map_proto cx reason props (ObjProtoT reason)
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
          let reason = replace_reason_const RReactState reason_op in
          mk_object cx reason
        | Some (Unknown reason) -> DefT (reason, AnyObjT)
        | Some (Known (Null reason)) -> DefT (reason, NullT)
        | Some (Known (NotNull (reason, props, dict, { exact; sealed; _ }))) ->
          let sealed = not (exact && sealed_in_op reason_op sealed) in
          mk_object_with_map_proto cx reason props (ObjProtoT reason)
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
      | None -> DefT (reason_op, AnyObjT)
      | Some (Unknown reason) -> DefT (reason, AnyObjT)
      | Some (Known (reason, props, dict, _)) ->
        mk_object_with_map_proto cx reason props (ObjProtoT reason)
          ?dict ~sealed:true ~exact:false
      in
      let props_t =
        mod_reason_of_t (replace_reason_const RReactPropTypes) props_t
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
          (* Tie the `this` knot with BindT *)
          Property.read_t v |> Option.iter ~f:(fun t ->
            let dummy_return = DefT (reason_op, AnyT) in
            let calltype = mk_methodcalltype knot.this [] dummy_return in
            rec_flow cx trace (t, BindT (reason_op, calltype, true))
          );
          SMap.add k v props, static_props

        | _ ->
          let bound_v = Property.map_t (fun t ->
            let destructor = Bind knot.this in
            let id = mk_id () in
            ignore (eval_destructor cx ~trace reason_op t destructor id);
            EvalT (t, TypeDestructorT (reason_op, destructor), id)
          ) v in
          SMap.add k bound_v props, static_props
      ) spec_props (SMap.empty, SMap.empty) in

      let static_props = static_props
        |> SMap.add "defaultProps" (Field (knot.default_t, Neutral))
      in

      let reason_component = replace_reason_const RReactComponent reason_op in

      let super =
        let reason = replace_reason (fun x -> RSuperOf x) reason_component in
        get_builtin_typeapp cx reason "LegacyReactComponent"
          [knot.default_t; props_t; knot.state_t]
      in

      let static =
        let reason, props, dict, exact, sealed = match spec.statics with
        | None ->
          reason_op, static_props, None, true, false
        | Some (Unknown reason) ->
          let dict = Some {
            dict_name = None;
            key = StrT.why reason;
            value = AnyT.why reason;
            dict_polarity = Neutral;
          } in
          reason, static_props, dict, false, true
        | Some (Known (reason, props, dict, { exact; sealed; _ })) ->
          let static_props = SMap.union props static_props in
          let sealed = not (exact && sealed_in_op reason_op sealed) in
          reason, static_props, dict, exact, sealed
        in
        let reason = replace_reason_const RReactStatics reason in
        mk_object_with_map_proto cx reason props (class_type super)
          ?dict ~exact ~sealed
      in

      let insttype = {
        class_id = 0;
        type_args = SMap.empty;
        arg_polarities = SMap.empty;
        fields_tmap = Context.make_property_map cx props;
        initialized_field_names = SSet.empty;
        methods_tmap = Context.make_property_map cx SMap.empty;
        mixins = spec.unknown_mixins <> [];
        structural = false;
      } in
      rec_flow cx trace (super, SuperT (reason_op, insttype));

      let instance = DefT (reason_component, InstanceT (static, super, [], insttype)) in
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
      | OK (reason, _, _, { exact; sealed; _ })
        when not (exact && sealed_in_op reason_op sealed) ->
        err_incompatible reason;
        Err reason
      | result -> result
      in
      (match result with
      | OK obj ->
        on_resolve_spec ((obj, empty_spec obj), stack')
      | Err reason ->
        (match stack' with
        | [] ->
          (* The root spec is unknown *)
          rec_flow_t cx trace (AnyT.why reason_op, tout)
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
      | Err reason ->
        let stack = map_spec (fun spec -> {
          spec with
          unknown_mixins = reason::spec.unknown_mixins
        }) stack in
        on_resolve_mixins stack
      | OK [] -> on_resolve_mixins stack
      | OK (t::todo) ->
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
        let props = SMap.add k (Field (t, Neutral)) props in
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
        | OK (reason, todo, dict, flags) ->
          let prop_types = reason, SMap.empty, None, flags in
          (match dict with
          | None -> next todo prop_types
          | Some dicttype ->
            let tool = PropTypes (stack,
              ResolveDict (dicttype, todo, prop_types)) in
            resolve tool dicttype.value)
        | Err reason ->
          let prop_types = Some (Unknown reason) in
          map_spec (fun spec -> {
            spec with
            prop_types = merge_prop_types prop_types spec.prop_types
          }) stack |> on_resolve_prop_types)
      | ResolveDict (dicttype, todo, prop_types) ->
        let dict = match coerce_prop_type l with
        | OK (_, t) -> {dicttype with value = t}
        | Err reason -> {dicttype with value = DefT (reason, AnyT)}
        in
        next todo (add_dict dict prop_types)
      | ResolveProp (k, todo, prop_types) ->
        let t = match coerce_prop_type l with
        | OK (required, t) -> if required then t else Type.optional t
        | Err reason -> Type.optional (DefT (reason, AnyT))
        in
        next todo (add_prop k t prop_types))

    | DefaultProps (todo, acc) ->
      let default_props = Some (maybe_known_of_result (coerce_object l)) in
      let acc = merge_default_props default_props acc in
      on_resolve_default_props acc todo

    | InitialState (todo, acc) ->
      let initial_state = Some (match l with
      | DefT (reason, NullT) -> Known (Null reason)
      | _ ->
        coerce_object l
        |> maybe_known_of_result
        |> map_known (fun x -> NotNull x)
      ) in
      let acc = merge_initial_state initial_state acc in
      on_resolve_initial_state acc todo
  in

  match u with
  | CreateElement (config, tout) -> create_element config tout
  | SimplifyPropType (tool, tout) -> simplify_prop_type tout tool
  | CreateClass (tool, knot, tout) -> create_class knot tout tool
