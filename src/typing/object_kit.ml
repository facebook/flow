(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Type
open TypeUtil

module type OBJECT = sig
  val run :
    Type.trace ->
    Context.t ->
    Type.use_op ->
    Reason.t ->
    Type.Object.resolve_tool ->
    Type.Object.tool ->
    Type.t ->
    tout:Type.t ->
    unit

  val widen_obj_type :
    Context.t -> ?trace:Type.trace -> use_op:Type.use_op -> Reason.reason -> Type.t -> Type.t
end

module Kit (Flow : Flow_common.S) : OBJECT = struct
  include Flow

  let widen_obj_type cx ?trace ~use_op reason t =
    match t with
    | OpenT (r, id) ->
      let open Constraint in
      begin
        match Lazy.force (Context.find_graph cx id) with
        | Unresolved _ ->
          let open Object in
          let widened_id = Tvar.mk_no_wrap cx r in
          let tout = OpenT (r, widened_id) in
          flow_opt
            cx
            ?trace
            (t, ObjKitT (use_op, reason, Resolve Next, ObjectWiden widened_id, tout));
          tout
        | Resolved (_, t)
        | FullyResolved (_, (lazy t)) ->
          widen_obj_type cx ?trace ~use_op reason t
      end
    | UnionT (r, rep) ->
      UnionT
        ( r,
          UnionRep.ident_map
            (fun t ->
              if is_proper_def t then
                widen_obj_type cx ?trace ~use_op reason t
              else
                t)
            rep )
    | t -> t

  let run =
    let open Object in
    (*****************)
    (* Object Spread *)
    (*****************)
    let object_spread =
      let dict_check trace cx use_op d1 d2 =
        rec_flow_t cx trace ~use_op (d2.key, d1.key);
        rec_unify cx trace ~use_op d1.value d2.value
      in
      let return trace cx use_op t tout = rec_flow_t cx trace ~use_op (t, tout) in
      let recurse trace cx use_op reason resolve_tool tool t tout =
        rec_flow cx trace (t, ObjKitT (use_op, reason, resolve_tool, tool, tout))
      in
      fun options state cx trace ->
        Slice_utils.object_spread
          ~dict_check:(dict_check trace)
          ~widen_obj_type:(widen_obj_type ~trace)
          ~add_output:(Flow_js_utils.add_output ~trace)
          ~return:(return trace)
          ~recurse:(recurse trace)
          options
          state
          cx
    in

    (***************)
    (* Object Rest *)
    (***************)
    let object_rest =
      let open Object.Rest in
      let return trace cx use_op options t tout =
        match options with
        | ReactConfigMerge Polarity.Neutral ->
          rec_unify cx trace ~use_op:(use_op Polarity.Neutral) t tout
        | ReactConfigMerge Polarity.Negative ->
          rec_flow_t cx trace ~use_op:(use_op Polarity.Negative) (tout, t)
        | ReactConfigMerge Polarity.Positive ->
          rec_flow_t cx trace ~use_op:(use_op Polarity.Positive) (t, tout)
        | _ ->
          (* Intentional UnknownUse here. *)
          rec_flow_t ~use_op:unknown_use cx trace (t, tout)
      in
      let recurse trace cx use_op reason resolve_tool tool t tout =
        rec_flow cx trace (t, ObjKitT (use_op, reason, resolve_tool, tool, tout))
      in
      let subt_check trace ~use_op cx = rec_flow_t ~use_op cx trace in
      fun options state cx trace ->
        Slice_utils.object_rest
          ~add_output:(Flow_js_utils.add_output ~trace)
          ~return:(return trace)
          ~recurse:(recurse trace)
          ~subt_check:(subt_check trace)
          options
          state
          cx
    in
    (********************)
    (* Object Read Only *)
    (********************)
    let object_read_only cx trace _use_op reason x tout =
      rec_flow_t ~use_op:unknown_use cx trace (Slice_utils.object_read_only cx reason x, tout)
    in
    (**************)
    (* Object Rep *)
    (**************)
    let object_rep =
      let mk_object cx reason { Object.reason = r; props; flags; generics; interface = _ } =
        (* TODO(jmbrown): Add polarity information to props *)
        let polarity = Polarity.Neutral in
        let props =
          NameUtils.Map.map
            (fun (t, _, is_method) ->
              if is_method then
                Method (None, t)
              else
                Field (None, t, polarity))
            props
        in
        let flags =
          {
            flags with
            obj_kind =
              Obj_type.map_dict (fun dict -> { dict with dict_polarity = polarity }) flags.obj_kind;
          }
        in
        let call = None in
        let id = Context.generate_property_map cx props in
        let proto = ObjProtoT reason in
        Slice_utils.mk_object_type
          ~def_reason:r
          ~exact_reason:(Some reason)
          ~invalidate_aliases:true
          ~interface:None
          flags
          call
          id
          proto
          generics
      in
      fun cx trace use_op reason x tout ->
        let t =
          match Nel.map (mk_object cx reason) x with
          | (t, []) -> t
          | (t0, t1 :: ts) -> UnionT (reason, UnionRep.make t0 t1 ts)
        in
        rec_flow_t cx trace ~use_op (t, tout)
    in
    (****************)
    (* Object Widen *)
    (****************)
    let object_widen =
      let open Slice_utils in
      let mk_object cx reason { Object.reason = r; props; flags; generics; interface = _ } =
        let polarity = Polarity.Neutral in
        let props =
          NameUtils.Map.map
            (fun (t, _, is_method) ->
              if is_method then
                Method (None, t)
              else
                Field (None, t, polarity))
            props
        in
        let flags =
          {
            flags with
            obj_kind =
              Obj_type.map_dict (fun dict -> { dict with dict_polarity = polarity }) flags.obj_kind;
          }
        in
        let call = None in
        let id = Context.generate_property_map cx props in
        let proto = ObjProtoT reason in
        Slice_utils.mk_object_type
          ~def_reason:r
          ~exact_reason:None
          ~invalidate_aliases:true
          ~interface:None
          flags
          call
          id
          proto
          generics
      in
      let widen cx trace ~use_op ~obj_reason ~slice ~widened_id ~tout =
        let rec is_subset (x, y) =
          match (x, y) with
          | (UnionT (_, rep), u) -> UnionRep.members rep |> List.for_all (fun t -> is_subset (t, u))
          | (t, UnionT (_, rep)) -> UnionRep.members rep |> List.exists (fun u -> is_subset (t, u))
          | (MaybeT (_, t1), MaybeT (_, t2)) -> is_subset (t1, t2)
          | ( OptionalT { reason = _; type_ = t1; use_desc = _ },
              OptionalT { reason = _; type_ = t2; use_desc = _ } ) ->
            is_subset (t1, t2)
          | (DefT (_, _, (NullT | VoidT)), MaybeT _) -> true
          | (DefT (_, _, VoidT), OptionalT _) -> true
          | (t1, MaybeT (_, t2)) -> is_subset (t1, t2)
          | (t1, OptionalT { reason = _; type_ = t2; use_desc = _ }) -> is_subset (t1, t2)
          | (t1, t2) -> quick_subtype false t1 t2
        in
        let widen_type cx trace reason ~use_op t1 t2 =
          match (t1, t2) with
          | (t1, t2) when is_subset (t2, t1) -> (t1, false)
          | (OpenT (r1, _), OpenT (r2, _))
            when Slice_utils.(is_widened_reason_desc r1 && is_widened_reason_desc r2) ->
            rec_unify cx trace ~use_op t1 t2;
            (t1, false)
          | (OpenT (r, _), t2) when Slice_utils.is_widened_reason_desc r ->
            rec_flow_t cx trace ~use_op (t2, t1);
            (t1, false)
          | (t1, t2) ->
            let reason = replace_desc_new_reason (RWidenedObjProp (desc_of_reason reason)) reason in
            ( Tvar.mk_where cx reason (fun t ->
                  rec_flow_t cx trace ~use_op (t1, t);
                  rec_flow_t cx trace ~use_op (t2, t)),
              true )
        in
        let widest = Context.spread_widened_types_get_widest cx widened_id in
        match widest with
        | None ->
          Context.spread_widened_types_add_widest cx widened_id slice;
          rec_flow_t cx trace ~use_op (mk_object cx obj_reason slice, tout)
        | Some widest ->
          let widest_pmap = widest.props in
          let widest_dict = Obj_type.get_dict_opt widest.Object.flags.obj_kind in
          let slice_dict = Obj_type.get_dict_opt slice.Object.flags.obj_kind in
          let new_pmap = slice.props in
          let pmap_and_changed =
            NameUtils.Map.merge
              (fun propname p1 p2 ->
                let p1 = get_prop widest.Object.reason p1 widest_dict in
                let p2 = get_prop slice.Object.reason p2 slice_dict in
                match (p1, p2) with
                | (None, None) -> None
                | (None, Some ((_, _, m) as t)) ->
                  let (t', opt, missing_prop) = type_optionality_and_missing_property t in
                  let t' =
                    if opt && not missing_prop then
                      optional t'
                    else
                      possibly_missing_prop propname widest.Object.reason t'
                  in
                  Some ((t', m), true)
                | (Some ((_, _, m) as t), None) ->
                  let (t', opt, missing_prop) = type_optionality_and_missing_property t in
                  let t' =
                    if opt && not missing_prop then
                      optional t'
                    else
                      possibly_missing_prop propname slice.Object.reason t'
                  in
                  Some ((t', m), not opt)
                | (Some ((_, _, m1) as t1), Some ((_, _, m2) as t2)) ->
                  let (t1', opt1, missing_prop1) = type_optionality_and_missing_property t1 in
                  let (t2', opt2, missing_prop2) = type_optionality_and_missing_property t2 in
                  let (t, changed) = widen_type cx trace obj_reason ~use_op t1' t2' in
                  if opt1 || opt2 then
                    Some
                      ( ( make_optional_with_possible_missing_props
                            propname
                            missing_prop1
                            missing_prop2
                            widest.Object.reason
                            t,
                          m1 || m2 ),
                        changed )
                  else
                    Some ((t, m1 || m2), changed))
              widest_pmap
              new_pmap
          in
          let (pmap', changed) =
            NameUtils.Map.fold
              (fun k (t, changed) (acc_map, acc_changed) ->
                (NameUtils.Map.add k t acc_map, changed || acc_changed))
              pmap_and_changed
              (NameUtils.Map.empty, false)
          in
          (* TODO: (jmbrown) we can be less strict here than unifying. It may be possible to
           * also merge the dictionary types *)
          let (dict, changed) =
            match (widest_dict, slice_dict) with
            | (Some d1, Some d2) ->
              rec_unify cx trace ~use_op d1.key d2.key;
              rec_unify cx trace ~use_op d1.value d2.value;
              (Some d1, changed)
            | (Some _, None) -> (None, true)
            | _ -> (None, changed)
          in
          let (exact, changed) =
            if
              Obj_type.is_legacy_exact_DO_NOT_USE widest.Object.flags.obj_kind
              && not (Obj_type.is_legacy_exact_DO_NOT_USE slice.Object.flags.obj_kind)
            then
              (false, true)
            else
              ( Obj_type.is_legacy_exact_DO_NOT_USE widest.Object.flags.obj_kind
                && Obj_type.is_legacy_exact_DO_NOT_USE slice.Object.flags.obj_kind,
                changed )
          in
          if not changed then
            ()
          else
            let obj_kind =
              match (exact, dict) with
              | (_, Some d) -> Indexed d
              | (true, _) -> Exact
              | _ -> Inexact
            in
            let flags = { obj_kind; frozen = false } in
            let props = NameUtils.Map.map (fun (t, m) -> (t, true, m)) pmap' in
            let slice' =
              {
                Object.reason = slice.Object.reason;
                props;
                flags;
                generics = widest.generics;
                interface = None;
              }
            in
            Context.spread_widened_types_add_widest cx widened_id slice';
            let obj = mk_object cx slice.Object.reason slice' in
            rec_flow_t cx trace ~use_op (obj, tout)
      in
      fun widened_id cx trace use_op reason x tout ->
        Nel.iter (fun slice -> widen cx trace ~use_op ~obj_reason:reason ~slice ~widened_id ~tout) x
    in

    (****************)
    (* React Config *)
    (****************)
    let react_config =
      Object.ReactConfig.(
        (* All props currently have a neutral polarity. However, they should have a
         * positive polarity (or even better, constant) since React.createElement()
         * freezes the type of props. We use a neutral polarity today because the
         * props type we flow the config into is written by users who very rarely
         * add a positive variance annotation. We may consider marking that type as
         * constant in the future as well. *)
        let prop_polarity = Polarity.Neutral in
        let finish cx trace ~use_op reason config defaults children =
          let {
            Object.reason = config_reason;
            props = config_props;
            flags = config_flags;
            generics = config_generics;
            interface = _;
          } =
            config
          in
          (* If we have some type for children then we want to add a children prop
           * to our config props. *)
          let config_props =
            Base.Option.value_map children ~default:config_props ~f:(fun children ->
                NameUtils.Map.add (OrdinaryName "children") (children, true, false) config_props)
          in
          (* Remove the key and ref props from our config. We check key and ref
           * independently of our config. So we must remove them so the user can't
           * see them. *)
          let config_props = NameUtils.Map.remove (OrdinaryName "key") config_props in
          let config_props = NameUtils.Map.remove (OrdinaryName "ref") config_props in

          let config_dict = Obj_type.get_dict_opt config_flags.obj_kind in
          (* Create the final props map and dict.
           *
           * NOTE: React will copy any enumerable prop whether or not it
           * is own to the config. *)
          let (props_map, flags, generics) =
            match defaults with
            (* If we have some default props then we want to add the types for those
             * default props to our final props object. *)
            | Some
                {
                  Object.reason = defaults_reason;
                  props = defaults_props;
                  flags = defaults_flags;
                  generics = defaults_generics;
                  interface = _;
                } ->
              let defaults_dict = Obj_type.get_dict_opt defaults_flags.obj_kind in
              (* Merge our props and default props. *)
              let props =
                NameUtils.Map.merge
                  (fun _ p1 p2 ->
                    let p1 = Slice_utils.get_prop config_reason p1 config_dict in
                    let p2 = Slice_utils.get_prop defaults_reason p2 defaults_dict in
                    match (p1, p2) with
                    | (None, None) -> None
                    | (Some (t, _, m), None) -> Some (t, m)
                    | (None, Some (t, _, m)) -> Some (t, m)
                    (* If a property is defined in both objects, and the first property's
                     * type includes void then we want to replace every occurrence of void
                     * with the second property's type. This is consistent with the behavior
                     * of function default arguments. If you call a function, `f`, like:
                     * `f(undefined)` and there is a default value for the first argument,
                     * then we will ignore the void type and use the type for the default
                     * parameter instead. *)
                    | (Some (t1, _, m1), Some (t2, _, m2)) ->
                      (* Use CondT to replace void with t1. *)
                      let t =
                        Tvar.mk_where cx reason (fun tvar ->
                            rec_flow
                              cx
                              trace
                              ( OpenT (reason, filter_optional cx ~trace reason t1),
                                CondT (reason, None, t2, tvar) ))
                      in
                      Some (t, m1 || m2))
                  config_props
                  defaults_props
              in
              (* Merge the dictionary from our config with the defaults dictionary. *)
              let dict =
                Base.Option.merge config_dict defaults_dict (fun d1 d2 ->
                    {
                      dict_name = None;
                      key = UnionT (reason, UnionRep.make d1.key d2.key []);
                      value =
                        UnionT
                          ( reason,
                            UnionRep.make
                              (Slice_utils.read_dict config_reason d1)
                              (Slice_utils.read_dict defaults_reason d2)
                              [] );
                      dict_polarity = prop_polarity;
                    })
              in
              (* React freezes the config so we set the frozen flag to true. The
               * final object is only exact if both the config and defaults objects
               * are exact. *)
              let obj_kind =
                match dict with
                | Some d -> Indexed d
                | None ->
                  if
                    Obj_type.is_exact_or_sealed reason config_flags.obj_kind
                    && Obj_type.is_exact_or_sealed reason defaults_flags.obj_kind
                  then
                    Exact
                  else
                    Inexact
              in
              let flags = { frozen = true; obj_kind } in
              let generics = Generic.spread_append config_generics defaults_generics in
              (props, flags, generics)
            (* Otherwise turn our slice props map into an object props. *)
            | None ->
              let props =
                NameUtils.Map.map (fun (t, _, is_method) -> (t, is_method)) config_props
              in
              (* Create a new dictionary from our config's dictionary with a
               * positive polarity. *)
              let dict =
                Base.Option.map config_dict (fun d ->
                    {
                      dict_name = None;
                      key = d.key;
                      value = d.value;
                      dict_polarity = prop_polarity;
                    })
              in
              (* React freezes the config so we set the frozen flag to true. The
               * final object is only exact if the config object is exact. *)
              let obj_kind =
                Obj_type.obj_kind_from_optional_dict
                  ~dict
                  ~otherwise:
                    (if Obj_type.is_exact_or_sealed reason config_flags.obj_kind then
                      Exact
                    else
                      Inexact)
              in
              let flags = { frozen = true; obj_kind } in
              (props, flags, config_generics)
          in
          let call = None in
          (* This code should not be here, but it has to be. In a React Server Component file,
           * we need to check all the props against React$TransportValue. The ideal way to do
           * this would be to take the final config object computed here and flow it to
           * React$TransportObject. However, doing that directly can lead to a subtle bug that
           * causes spurious errors. The config object produced here has a literal object
           * reason-- and it should! It's a new fresh object. It's also important that this
           * object is a literal so that it gets the less strict type checking behavior when
           * flowing to the Component's props. Many Props types are not specified as
           * read-only, so this literal reason is required in order to get the
           * covariant property type checking.
           *
           * However, literal objects flowing to object types with optional properties will
           * take on optional properties in the object types. This behavior is spooky, but
           * in place because we do not have a proper aliasing analysis.
           *
           * In order to avoid accidentally comparing optional properties from the Props
           * that may be adopted by the config to React$TransportValue, we flow each property
           * to React$TransportValue directly. If a dictionary is present then we also flow
           * the dict's value type to React$TransportValue. If the object is Inexact then
           * we also flow mixed to React$TransportValue to represent properties that may
           * exist but are not present in the type. This case will always error, but is
           * unlikely to be hit because it requires an inexact object to be spread into
           * the props in jsx.
           *)
          if Context.in_react_server_component_file cx then (
            let reason_transport_value_reason =
              replace_desc_reason (RCustom "React.TransportValue") reason
            in
            let react_transport_value =
              get_builtin_type
                cx
                reason_transport_value_reason
                (OrdinaryName "React$TransportValue")
            in
            NameUtils.Map.iter
              (fun _ (t, _) -> rec_flow_t cx trace ~use_op (t, react_transport_value))
              props_map;
            match flags.obj_kind with
            | UnsealedInFile _ -> failwith "React config should never be unsealed"
            | Exact -> ()
            | Inexact ->
              let r =
                mk_reason
                  (RUnknownUnspecifiedProperty (desc_of_reason reason))
                  (aloc_of_reason reason)
              in
              let mixed = DefT (r, bogus_trust (), MixedT Mixed_everything) in
              rec_flow_t cx trace ~use_op (mixed, react_transport_value)
            | Indexed d -> rec_flow_t cx trace ~use_op (d.value, react_transport_value)
          );
          (* Finish creating our props object. *)
          let props =
            NameUtils.Map.map
              (fun (t, is_method) ->
                if is_method then
                  Method (None, t)
                else
                  Field (None, t, prop_polarity))
              props_map
          in
          let id = Context.generate_property_map cx props in
          let proto = ObjProtoT reason in
          Slice_utils.mk_object_type
            ~def_reason:reason
            ~exact_reason:(Some reason)
            ~invalidate_aliases:false
            ~interface:None
            flags
            call
            id
            proto
            generics
        in
        fun state cx trace use_op reason x tout ->
          match state with
          (* If we have some type for default props then we need to wait for that
           * type to resolve before finishing our props type. *)
          | Config { defaults = Some t; children } ->
            let tool = Resolve Next in
            let state = Defaults { config = x; children } in
            rec_flow cx trace (t, ObjKitT (use_op, reason, tool, ReactConfig state, tout))
          (* If we have no default props then finish our object and flow it to our
           * tout type. *)
          | Config { defaults = None; children } ->
            let ts = Nel.map (fun x -> finish cx trace ~use_op reason x None children) x in
            let t =
              match ts with
              | (t, []) -> t
              | (t0, t1 :: ts) -> UnionT (reason, UnionRep.make t0 t1 ts)
            in
            rec_flow cx trace (t, UseT (use_op, tout))
          (* If we had default props and those defaults resolved then finish our
           * props object with those default props. *)
          | Defaults { config; children } ->
            let ts =
              Nel.map_concat
                (fun c -> Nel.map (fun d -> finish cx trace ~use_op reason c (Some d) children) x)
                config
            in
            let t =
              match ts with
              | (t, []) -> t
              | (t0, t1 :: ts) -> UnionT (reason, UnionRep.make t0 t1 ts)
            in
            rec_flow cx trace (t, UseT (use_op, tout)))
    in
    (*********************)
    (* Object Resolution *)
    (*********************)
    let next = function
      | Spread (options, state) -> object_spread options state
      | Rest (options, state) -> object_rest options state
      | ReactConfig state -> react_config state
      | ReadOnly -> object_read_only
      | ObjectRep -> object_rep
      | ObjectWiden id -> object_widen id
    in
    fun trace ->
      let add_output = Flow_js_utils.add_output ~trace in
      let return cx use_op t ~tout = rec_flow_t cx trace ~use_op (t, tout) in
      let next cx use_op tool reason x ~tout = next tool cx trace use_op reason x tout in
      let recurse cx use_op reason resolve_tool tool t ~tout =
        rec_flow cx trace (t, ObjKitT (use_op, reason, resolve_tool, tool, tout))
      in
      let statics cx r i =
        Tvar.mk_no_wrap_where cx r (fun tvar -> rec_flow cx trace (i, GetStaticsT tvar))
      in
      Slice_utils.run ~add_output ~return ~next ~recurse ~statics
end
