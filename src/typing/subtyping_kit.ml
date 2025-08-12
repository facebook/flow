(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Debug_js.Verbose
open Flow_js_utils
open Instantiation_utils
open Reason
open Type
open TypeUtil
open Utils_js

module type INPUT = sig
  include Flow_common.BASE

  include Flow_common.BUILTINS

  include Flow_common.EVAL

  include Flow_common.SUBTYPING

  include Flow_common.REACT
end

module type OUTPUT = sig
  val rec_sub_t : Context.t -> Type.use_op -> Type.t -> Type.t -> Type.DepthTrace.t -> unit

  val rec_flow_p :
    Context.t ->
    ?trace:Type.DepthTrace.t ->
    use_op:use_op ->
    ?report_polarity:bool ->
    reason ->
    reason ->
    Type.propref ->
    Type.property_type * Type.property_type ->
    unit

  val union_to_union :
    Context.t -> Type.DepthTrace.t -> use_op -> Type.t -> Type.UnionRep.t -> Type.t -> unit
end

module Make (Flow : INPUT) : OUTPUT = struct
  open Flow
  module SpeculationKit = Speculation_kit.Make (Flow)
  module RendersKit = Renders_kit.Make (Flow)

  let flow_all_in_union cx trace rep u =
    iter_union ~f:rec_flow ~init:() ~join:(fun _ _ -> ()) cx trace rep u

  let add_output_prop_polarity_mismatch cx use_op (lreason, ureason) props =
    match Nel.of_list props with
    | None -> ()
    | Some props ->
      add_output cx (Error_message.EPropPolarityMismatch { lreason; ureason; props; use_op })

  let polarity_error_content propref lp up =
    let lpol = Property.polarity_of_property_type lp in
    let upol = Property.polarity_of_property_type up in
    (propref, (lpol, upol))

  let rec_flow_p cx ?trace ~use_op ?(report_polarity = true) propref = function
    (* unification cases *)
    | ( OrdinaryField { type_ = lt; polarity = Polarity.Neutral },
        OrdinaryField { type_ = ut; polarity = Polarity.Neutral }
      ) ->
      unify_opt cx ?trace ~use_op lt ut;
      []
    (* directional cases *)
    | (lp, up) ->
      let propref_error = name_of_propref propref in
      let errs1 =
        match (Property.read_t_of_property_type lp, Property.read_t_of_property_type up) with
        | (Some lt, Some ut) ->
          flow_opt cx ?trace (lt, UseT (use_op, ut));
          []
        | (None, Some _) when report_polarity -> [polarity_error_content propref_error lp up]
        | _ -> []
      in
      let errs2 =
        match (Property.write_t_of_property_type lp, Property.write_t_of_property_type up) with
        | (Some lt, Some ut) ->
          flow_opt cx ?trace (ut, UseT (use_op, lt));
          []
        | (None, Some _) when report_polarity -> [polarity_error_content propref_error lp up]
        | _ -> []
      in
      errs1 @ errs2

  let index_of_param params x =
    Base.List.find_mapi params ~f:(fun i p ->
        match p with
        | (Some x', _) when x = x' -> Some i
        | _ -> None
    )

  let func_type_guard_compat cx trace use_op grd1 grd2 =
    let (reason1, params1, impl1, (loc1, x1), t1) = grd1 in
    let (reason2, params2, impl2, (loc2, x2), t2) = grd2 in
    if impl1 && not impl2 then
      add_output
        cx
        (Error_message.ETypeGuardImpliesMismatch { use_op; reasons = (reason1, reason2) });
    let idx1 = index_of_param params1 x1 in
    let idx2 = index_of_param params2 x2 in
    let use_op = Frame (TypeGuardCompatibility, use_op) in
    ( if idx1 <> idx2 then
      let lower = Reason.mk_reason (RTypeGuardParam x1) loc1 in
      let upper = Reason.mk_reason (RTypeGuardParam x2) loc2 in
      add_output cx (Error_message.ETypeGuardIndexMismatch { use_op; reasons = (lower, upper) })
    );
    if impl2 then
      rec_flow_t cx trace ~use_op (t1, t2)
    else
      rec_unify cx trace ~use_op t1 t2

  let flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj) =
    let {
      flags = { react_dro = ldro; _ } as lflags;
      call_t = lcall;
      props_tmap = lflds;
      proto_t = lproto;
      reachable_targs = _;
    } =
      l_obj
    in
    let {
      flags = { react_dro = udro; _ } as rflags;
      call_t = ucall;
      props_tmap = uflds;
      proto_t = uproto;
      reachable_targs = _;
    } =
      u_obj
    in

    let mod_t name react_dro t =
      if
        Base.Option.value_map ~f:dro_strict ~default:false ldro
        = Base.Option.value_map ~f:dro_strict ~default:false udro
      then
        t
      else
        let t =
          match (react_dro, name) with
          | (Some dro, None) -> Flow.mk_react_dro cx use_op dro t
          | (Some dro, Some name)
            when not
                   (is_exception_to_react_dro
                      (Named { name; reason = reason_of_t t; from_indexed_access = false })
                   ) ->
            Flow.mk_react_dro cx use_op dro t
          | _ -> t
        in
        match name with
        | Some (OrdinaryName name)
          when Context.hook_compatibility cx && Flow_ast_utils.hook_name name ->
          mk_hooklike cx use_op t
        | _ -> t
    in

    (* if inflowing type is literal (thus guaranteed to be
       unaliased), propertywise subtyping is sound *)
    let lit = is_literal_object_reason lreason in
    (* If both are dictionaries, ensure the keys and values are compatible
       with each other. *)
    let ldict = Obj_type.get_dict_opt lflags.obj_kind in
    let udict = Obj_type.get_dict_opt rflags.obj_kind in
    (match (ldict, udict) with
    | ( Some { key = lk; value = lv; dict_polarity = lpolarity; _ },
        Some { key = uk; value = uv; dict_polarity = upolarity; _ }
      ) ->
      let use_op_k = Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op) in
      ( if lit then
        rec_flow_t cx trace ~use_op:use_op_k (mod_t None ldro lk, mod_t None udro uk)
      else
        (* Don't report polarity errors when checking the indexer key. We would
         * report these errors again a second time when checking values. *)
        let errs =
          rec_flow_p
            cx
            ~trace
            ~report_polarity:false
            ~use_op:use_op_k
            (Computed uk)
            ( OrdinaryField { type_ = mod_t None ldro lk; polarity = lpolarity },
              OrdinaryField { type_ = mod_t None udro uk; polarity = upolarity }
            )
        in
        add_output_prop_polarity_mismatch cx use_op_k (lreason, ureason) errs
      );
      let use_op_v =
        Frame (PropertyCompatibility { prop = None; lower = lreason; upper = ureason }, use_op)
      in
      if lit then
        rec_flow_t cx trace ~use_op:use_op_v (mod_t None ldro lv, mod_t None udro uv)
      else
        let errs =
          rec_flow_p
            cx
            ~trace
            ~use_op:use_op_v
            (Computed uv)
            ( OrdinaryField { type_ = mod_t None ldro lv; polarity = lpolarity },
              OrdinaryField { type_ = mod_t None udro uv; polarity = upolarity }
            )
        in
        add_output_prop_polarity_mismatch cx use_op_v (lreason, ureason) errs
    | _ -> ());

    if rflags.obj_kind = Exact && not (is_literal_object_reason ureason) then (
      if not (Obj_type.is_exact lflags.obj_kind) then
        exact_obj_error cx lflags.obj_kind ~use_op ~exact_reason:ureason (DefT (lreason, ObjT l_obj));
      let missing_props =
        Context.fold_real_props
          cx
          lflds
          (fun name _ acc ->
            if Context.has_prop cx uflds name then
              acc
            else
              name :: acc)
          []
        |> List.rev
      in
      (match Nel.of_list missing_props with
      | None -> ()
      | Some missing_props ->
        let err =
          Error_message.EPropsExtraAgainstExactObject
            { prop_names = missing_props; reason_l_obj = lreason; reason_r_obj = ureason; use_op }
        in
        add_output cx err);
      Base.Option.iter lcall ~f:(fun _ ->
          if Base.Option.is_none ucall then
            let prop = Some (OrdinaryName "$call") in
            let err =
              (* Lower and upper are reversed in this case since the lower object
               * is the one requiring the prop. *)
              Error_message.EPropNotFoundInSubtyping
                {
                  prop_name = prop;
                  reason_lower = ureason;
                  reason_upper = lreason;
                  use_op;
                  suggestion = None;
                }
            in
            add_output cx err
      )
    );

    (match ucall with
    | Some ucall ->
      let prop_name = Some (OrdinaryName "$call") in
      (match lcall with
      | Some lcall ->
        rec_flow cx trace (Context.find_call cx lcall, UseT (use_op, Context.find_call cx ucall))
      | None ->
        let error_message =
          Error_message.EPropNotFoundInSubtyping
            { reason_lower = lreason; reason_upper = ureason; prop_name; use_op; suggestion = None }
        in
        add_output cx error_message)
    | None -> ());

    (* Properties in u must either exist in l, or match l's indexer. *)
    let errs =
      Context.fold_real_props
        cx
        uflds
        (fun name up acc ->
          let reason_prop = replace_desc_reason (RProperty (Some name)) ureason in
          let propref = mk_named_prop ~reason:reason_prop name in
          let use_op' = use_op in
          let use_op =
            Frame
              (PropertyCompatibility { prop = Some name; lower = lreason; upper = ureason }, use_op')
          in
          match (Context.get_prop cx lflds name, ldict) with
          | (Some lp, _) ->
            if lit then
              (* prop from unaliased LB: check <: *)
              match (Property.read_t lp, Property.read_t up) with
              | (Some lt, Some ut) ->
                rec_flow
                  cx
                  trace
                  (mod_t (Some name) ldro lt, UseT (use_op, mod_t (Some name) udro ut));
                acc
              | _ -> acc
            else
              (* prop from aliased LB *)
              let errs =
                rec_flow_p
                  cx
                  ~trace
                  ~use_op
                  propref
                  ( Property.type_ lp |> TypeUtil.map_property ~f:(mod_t (Some name) ldro),
                    Property.type_ up |> TypeUtil.map_property ~f:(mod_t (Some name) udro)
                  )
              in
              Base.List.rev_append errs acc
          | (None, Some { key; value; dict_polarity; _ }) when not (is_dictionary_exempt name) ->
            let subtype_against_indexer err_acc () =
              let lp =
                OrdinaryField { type_ = mod_t (Some name) ldro value; polarity = dict_polarity }
              in
              let up =
                match up with
                | Field
                    {
                      preferred_def_locs = _;
                      key_loc = _;
                      type_ = OptionalT { reason = _; type_ = ut; use_desc = _ };
                      polarity;
                    } ->
                  OrdinaryField { type_ = mod_t (Some name) udro ut; polarity }
                | _ -> Property.type_ up |> TypeUtil.map_property ~f:(mod_t (Some name) udro)
              in
              if lit then
                match
                  (Property.read_t_of_property_type lp, Property.read_t_of_property_type up)
                with
                | (Some lt, Some ut) ->
                  rec_flow cx trace (lt, UseT (use_op, ut));
                  err_acc
                | _ -> err_acc
              else
                let errs = rec_flow_p cx ~trace ~use_op propref (lp, up) in
                Base.List.rev_append errs err_acc
            in
            (match up with
            (* If the upper property is optional and readonly (or this is a lit check) then we only
             * need to check the lower indexer type against the upper property type if the upper
             * property key is covered by the lower property's indexer, otherwise we can omit the
             * check. We already check elsewhere that the upper dictionary is compatible with the
             * lower dictionary, so we are not risking any issues with exactness here.
             *
             * We only do this outside of implicit instantiation to avoid accidentally underconstraining tvars by avoiding flows *)
            | Field { type_ = OptionalT _; polarity; _ }
              when (lit || polarity = Polarity.Positive)
                   && not (Context.in_implicit_instantiation cx) ->
              if speculative_subtyping_succeeds cx (type_of_key_name cx name reason_prop) key then
                subtype_against_indexer acc ()
              else
                acc
            | _ ->
              rec_flow
                cx
                trace
                ( type_of_key_name cx name reason_prop,
                  UseT
                    ( Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op'),
                      key
                    )
                );
              subtype_against_indexer acc ())
          | _ ->
            (* property doesn't exist in inflowing type *)
            (match up with
            | Field { type_ = OptionalT _; _ } when lit -> acc
            | Field { type_ = OptionalT _ as type_; polarity = Polarity.Positive; _ }
              when Obj_type.is_exact lflags.obj_kind ->
              rec_flow
                cx
                trace
                ( lproto,
                  LookupT
                    {
                      reason = ureason;
                      lookup_kind = NonstrictReturning (None, None);
                      try_ts_on_failure = [];
                      propref;
                      lookup_action =
                        LookupPropForSubtyping
                          {
                            use_op = use_op';
                            prop = OrdinaryField { type_; polarity = Polarity.Positive };
                            prop_name = name;
                            reason_lower = lreason;
                            reason_upper = ureason;
                          };
                      method_accessible = true;
                      ids = None;
                      ignore_dicts = false;
                    }
                );
              acc
            | _ ->
              (* look up the property in the prototype *)
              let lookup_kind =
                match ldict with
                | None -> Strict lreason
                | _ -> NonstrictReturning (None, None)
              in
              rec_flow
                cx
                trace
                ( lproto,
                  LookupT
                    {
                      reason = ureason;
                      lookup_kind;
                      try_ts_on_failure = [];
                      propref;
                      lookup_action =
                        LookupPropForSubtyping
                          {
                            use_op = use_op';
                            prop = Property.type_ up;
                            prop_name = name;
                            reason_lower = lreason;
                            reason_upper = ureason;
                          };
                      method_accessible = true;
                      ids = None;
                      ignore_dicts = false;
                    }
                );
              acc))
        []
    in
    add_output_prop_polarity_mismatch cx use_op (lreason, ureason) errs;

    (* Any properties in l but not u must match indexer *)
    (match udict with
    | None -> ()
    | Some { key; value; dict_polarity; _ } ->
      let flow_prop_to_indexer lp name =
        let use_op =
          Frame
            (PropertyCompatibility { prop = Some name; lower = lreason; upper = ureason }, use_op)
        in
        let lp =
          match lp with
          | Field
              {
                preferred_def_locs = _;
                key_loc = _;
                type_ = OptionalT { reason = _; type_ = lt; use_desc = _ };
                polarity;
              } ->
            OrdinaryField { type_ = mod_t (Some name) ldro lt; polarity }
          | _ -> Property.type_ lp |> TypeUtil.map_property ~f:(mod_t (Some name) ldro)
        in
        let up = OrdinaryField { type_ = mod_t (Some name) udro value; polarity = dict_polarity } in
        begin
          if lit then
            match (Property.read_t_of_property_type lp, Property.read_t_of_property_type up) with
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ()
          else
            let propref =
              mk_named_prop ~reason:(replace_desc_reason (RProperty (Some name)) lreason) name
            in
            let errs = rec_flow_p cx ~trace ~use_op propref (lp, up) in
            add_output_prop_polarity_mismatch cx use_op (lreason, ureason) errs
        end
      in
      (* If we are in implicit instantiation then we should always flow missing keys & value types to the
       * upper dictionary because that information may be useful to infer a type. Outside of implicit instantiation,
       * flowing both can cause redundant errors when the key is already not a valid indexer key, so we avoid the
       * value flows when that does not pass *)
      ( if not (Context.in_implicit_instantiation cx) then
        Context.iter_real_props cx lflds (fun name lp ->
            if Context.has_prop cx uflds name then
              ()
            else begin
              if speculative_subtyping_succeeds cx (type_of_key_name cx name lreason) key then
                flow_prop_to_indexer lp name
              else
                add_output
                  cx
                  Error_message.(
                    EIndexerCheckFailed
                      {
                        prop_name = name;
                        (* Lower and upper are reversed in this case since the lower object
                         * is the one requiring the prop. *)
                        reason_lower = ureason;
                        reason_upper = lreason;
                        reason_indexer = reason_of_t key;
                        use_op;
                      }
                  )
            end
        )
      else
        let keys =
          Context.fold_real_props
            cx
            lflds
            (fun name lp keys ->
              if Context.has_prop cx uflds name then
                keys
              else (
                flow_prop_to_indexer lp name;
                type_of_key_name cx name lreason :: keys
              ))
            []
          |> union_of_ts lreason
        in
        rec_flow
          cx
          trace
          ( keys,
            UseT (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op), key)
          )
      );
      (* If the left is inexact and the right is indexed, Flow mixed to the indexer
       * value. Mixed represents the possibly unknown properties on the inexact object
       *)
      (match lflags.obj_kind with
      | Inexact ->
        let r =
          mk_reason (RUnknownUnspecifiedProperty (desc_of_reason lreason)) (loc_of_reason lreason)
        in
        let mixed = DefT (r, MixedT Mixed_everything) in
        rec_flow_t cx trace ~use_op (mixed, value)
      | _ -> ());
      (* Previously, call properties were stored in the props map, and were
         checked against dictionary upper bounds. This is wrong, but useful for
         distinguishing between thunk-like types found in graphql-js.

         Now that call properties are stored separately, it is particularly
         egregious to emit this constraint. This only serves to maintain buggy
         behavior, which should be fixed, and this code removed. *)
      (match (lcall, ucall) with
      | (Some lcall, None) ->
        let name = "$call" in
        let use_op =
          Frame
            ( PropertyCompatibility
                { prop = Some (OrdinaryName name); lower = lreason; upper = ureason },
              use_op
            )
        in
        let lp =
          match Context.find_call cx lcall with
          | OptionalT { reason = _; type_ = t; use_desc = _ }
          | t ->
            OrdinaryField { type_ = t; polarity = Polarity.Positive }
        in
        let up = OrdinaryField { type_ = value; polarity = dict_polarity } in
        if lit then
          match (Property.read_t_of_property_type lp, Property.read_t_of_property_type up) with
          | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
          | _ -> ()
        else
          let name = OrdinaryName name in
          let reason_prop = replace_desc_reason (RProperty (Some name)) lreason in
          let propref = mk_named_prop ~reason:reason_prop name in
          let errs = rec_flow_p cx ~trace ~use_op propref (lp, up) in
          add_output_prop_polarity_mismatch cx use_op (lreason, ureason) errs
      | _ -> ()));

    rec_flow cx trace (uproto, ReposUseT (ureason, false, use_op, DefT (lreason, ObjT l_obj)))

  let flow_react_component_instance_to_instance =
    let subtyping_check cx trace use_op = function
      (* Easy cases: LHS and RHS have the same kind of instance information,
       * we can still directly flow the type to each other *)
      | (ComponentInstanceOmitted _, ComponentInstanceOmitted _) -> ()
      (* component(ref: l) ~> component(ref: r) *)
      | (ComponentInstanceAvailableAsRefSetterProp l, ComponentInstanceAvailableAsRefSetterProp r)
        ->
        (* ref prop is contravariantly typed. We need to flip the flow. *)
        rec_flow_t cx trace ~use_op (r, l)
      (* The most tricky cases: LHS and RHS have different kinds of instance information,
       * and one side is ComponentInstanceAvailableAsRefSetterProp. We need to wrap the side
       * that's not ComponentInstanceAvailableAsRefSetterProp with React.RefSetter *)
      (* We finally reached the most hopeless case. We wrap the instance type form with
       * React.RefSetter, and let the subtyping rule take care of the rest. *)
      (* component(ref?: ref_prop) ~> component().
       * Allowed since ``{} ~> {+ref?: ref_prop} *)
      | (ComponentInstanceAvailableAsRefSetterProp _, ComponentInstanceOmitted _) -> ()
      (* component() (equivalent to component(ref?: empty))
       * ~> component(ref: ref_prop) *)
      | (ComponentInstanceOmitted r, ComponentInstanceAvailableAsRefSetterProp ref_prop) ->
        rec_flow_t cx trace ~use_op (ref_prop, VoidT.why r)
    in
    subtyping_check

  (* mutable sites on parent values (i.e. object properties,
     array elements) must be typed invariantly when a value
     flows to the parent, unless the incoming value is fresh,
     in which case covariant typing is sound (since no alias
     will break if the subtyped child value is replaced by a
     non-subtyped value *)
  let flow_to_mutable_child cx trace use_op fresh t1 t2 =
    if fresh then
      rec_flow cx trace (t1, UseT (use_op, t2))
    else
      rec_unify cx trace ~use_op t1 t2

  (* Subtyping of arrays is complicated by tuples. Currently, there are three
     different kinds of types, all encoded by arrays:

     1. Array<T> (array type)
     2. [T1, T2] (tuple type)
     3. "internal" Array<X>[T1, T2] where T1 | T2 ~> X (array literal type)

     We have the following rules:

     (1) When checking types against Array<U>, the rules are not surprising. Array
     literal types behave like array types in these checks.

     * Array<T> ~> Array<U> checks T <~> U
     * [T1, T2] ~> Array<U> checks T1 | T2 ~> U
     * Array<X>[T1, T2] ~> Array<U> checks Array<X> ~> Array<U>

     (2) When checking types against [T1, T2], the rules are again not
     surprising. Array literal types behave like tuple types in these checks. We
     consider missing tuple elements to be undefined, following common usage (and
     consistency with missing call arguments).

     * Array<T> ~> [U1, U2] checks T ~> U1, T ~> U2
     * [T1, T2] ~> [U1, U2] checks T1 ~> U1 and T2 ~> U2
     * [T1, T2] ~> [U1] checks T1 ~> U1
     * [T1] ~> [U1, U2] checks T1 ~> U1 and void ~> U2
     * Array<X>[T1, T2] ~> [U1, U2] checks [T1, T2] ~> [U1, U2]

     (3) When checking types against Array<Y>[U1, U2], the rules are a bit
     unsound. Array literal types were not designed to appear as upper bounds. In
     particular, their summary element types are often overly precise. Checking
     individual element types of one array literal type against the summary
     element type of another array literal type can lead to crazy errors, so we
     currently drop such checks.

     TODO: Make these rules great again by computing more reasonable summary
     element types for array literal types.

     * Array<T> ~> Array<Y>[U1, U2] checks Array<T> ~> Array<Y>
     * [T1, T2] ~> Array<Y>[U1, U2] checks T1 ~> U1, T2 ~> U2
     * [T1, T2] ~> Array<Y>[U1] checks T1 ~> U1
     * [T1] ~> Array<Y>[U1, U2] checks T1 ~> U1
     * Array<X>[T1, T2] ~> Array<Y>[U1, U2] checks [T1, T2] ~> Array<Y>[U1, U2]
  *)
  let rec array_flow cx trace use_op lit1 r1 ?(index = 0) = function
    (* empty array / array literal / tuple flowing to array / array literal /
       tuple (includes several cases, analyzed below) *)
    | ([], e1, _, e2) ->
      (* if lower bound is an empty array / array literal *)
      if index = 0 then
        (* general element1 = general element2 *)
        flow_to_mutable_child cx trace use_op lit1 e1 e2
    (* otherwise, lower bound is an empty tuple (nothing to do) *)
    (* non-empty array literal / tuple ~> empty array / array literal / tuple *)
    | (_, e1, [], e2) ->
      (* general element1 < general element2 *)
      flow_to_mutable_child cx trace use_op lit1 e1 e2
    (* non-empty array literal / tuple ~> non-empty array literal / tuple *)
    | (t1 :: ts1, e1, t2 :: ts2, e2) ->
      (* specific element1 = specific element2 *)
      flow_to_mutable_child cx trace use_op lit1 t1 t2;
      array_flow cx trace use_op lit1 r1 ~index:(index + 1) (ts1, e1, ts2, e2)

  let take_n_from_set n set =
    let exception Done in
    let result = ref [] in
    let i = ref 0 in
    match
      UnionEnumSet.iter
        (fun x ->
          if !i < n then (
            incr i;
            result := x :: !result
          ) else
            raise Done)
        set
    with
    | exception Done -> List.rev !result
    | _ -> List.rev !result

  let union_to_union cx trace use_op l rep u =
    match (union_optimization_guard cx TypeUtil.quick_subtype l u, use_op) with
    | (UnionOptimizationGuardResult.True, _) ->
      if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path (True)"
    | (UnionOptimizationGuardResult.Maybe, _) -> flow_all_in_union cx trace rep (UseT (use_op, u))
    | (UnionOptimizationGuardResult.False { diff }, _) ->
      if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path (False)";
      let reason_lower = reason_of_t l in
      let reason_upper = reason_of_t u in
      let explanation =
        Some
          (Flow_intermediate_error_types.ExplanationAdditionalUnionMembers
             {
               left = reason_lower;
               right = reason_upper;
               members = take_n_from_set 3 diff |> Base.List.map ~f:UnionEnum.to_string;
               extra_number = max 0 (UnionEnumSet.cardinal diff - 3);
             }
          )
      in
      add_output
        cx
        (Error_message.EIncompatibleWithUseOp { reason_lower; reason_upper; use_op; explanation })

  let check_dro_subtyping cx use_op l u trace =
    match (l, u) with
    | ((AnyT _ | AnnotT _), _)
    | (_, (AnyT _ | AnnotT _)) ->
      ()
    | (l, u) -> begin
      match TypeUtil.(dro_of_type l, dro_of_type u |> Base.Option.map ~f:dro_strict) with
      | (Some dro, _) when not (dro_strict dro) -> ()
      | (None, _) -> ()
      | (Some _, Some true) -> ()
      | (Some (dro_loc, _), (None | Some false)) ->
        rec_flow
          cx
          trace
          ( u,
            CheckReactImmutableT
              { use_op; lower_reason = reason_of_t l; upper_reason = reason_of_t u; dro_loc }
          )
    end

  let rec_sub_t cx use_op l u trace =
    check_dro_subtyping cx use_op l u trace;
    match (l, u) with
    (* The sink component of an annotation constrains values flowing
       into the annotated site. *)
    | (_, AnnotT (r, t, use_desc)) -> rec_flow cx trace (t, ReposUseT (r, use_desc, use_op, l))
    | (AnnotT (r, t, use_desc), _) ->
      let t = reposition_reason ~trace cx r ~use_desc t in
      rec_flow_t cx trace ~use_op (t, u)
    (*******************************)
    (* common implicit conversions *)
    (*******************************)
    | (DefT (_, (NumGeneralT _ | SingletonNumT _)), DefT (_, NumGeneralT _)) -> ()
    | (DefT (r, (NullT | VoidT)), MaybeT (_, tout)) ->
      rec_flow_t cx trace ~use_op (EmptyT.why r, tout)
    | (DefT (r, MixedT Mixed_everything), MaybeT (_, tout)) ->
      rec_flow_t cx trace ~use_op (DefT (r, MixedT Mixed_non_maybe), tout)
    | (MaybeT (r, t), MaybeT _) ->
      let t = push_type_alias_reason r t in
      rec_flow_t cx trace ~use_op (t, u)
    | (MaybeT (reason, t), _) ->
      let reason = replace_desc_reason RNullOrVoid reason in
      let t = push_type_alias_reason reason t in
      let null = NullT.make reason in
      let void = VoidT.make reason in
      rec_flow_t cx trace ~use_op (null, u);
      rec_flow_t cx trace ~use_op (void, u);
      rec_flow_t cx trace ~use_op (t, u)
    | (DefT (r, VoidT), OptionalT { reason = _; type_ = tout; use_desc = _ }) ->
      rec_flow_t cx trace ~use_op (EmptyT.why r, tout)
    | (OptionalT { reason = _; type_ = t; use_desc = _ }, OptionalT _)
    | (OptionalT { reason = _; type_ = t; use_desc = _ }, MaybeT _) ->
      rec_flow_t cx trace ~use_op (t, u)
    | (OptionalT { reason = r; type_ = t; use_desc }, _) ->
      let void = VoidT.why_with_use_desc ~use_desc r in
      rec_flow_t cx trace ~use_op (void, u);
      rec_flow_t cx trace ~use_op (t, u)
    | (ThisTypeAppT (reason_tapp, c, this, ts), _) ->
      let reason_op = reason_of_t u in
      instantiate_this_class cx trace ~reason_op ~reason_tapp c ts this (Upper (UseT (use_op, u)))
    | (_, ThisTypeAppT (reason_tapp, c, this, ts)) ->
      let reason_op = reason_of_t l in
      instantiate_this_class cx trace ~reason_op ~reason_tapp c ts this (Lower (use_op, l))
    (*
     * When a subtyping question involves a union appearing on the right or an
     * intersection appearing on the left, the simplification rules are
     * imprecise: we split the union / intersection into cases and try to prove
     * that the subtyping question holds for one of the cases, but each of those
     * cases may be unprovable, which might lead to spurious errors. In
     * particular, obvious assertions such as (A | B) & C is a subtype of A | B
     * cannot be proved if we choose to split the union first (discharging
     * unprovable subgoals of (A | B) & C being a subtype of either A or B);
     * dually, obvious assertions such as A & B is a subtype of (A & B) | C
     * cannot be proved if we choose to simplify the intersection first
     * (discharging unprovable subgoals of either A or B being a subtype of (A &
     * B) | C). So instead, we try inclusion rules to handle such cases.
     *
     * An orthogonal benefit is that for large unions or intersections, checking
     * inclusion is significantly faster that splitting for proving simple
     * inequalities (O(n) instead of O(n^2) for n cases).
     *)
    | (IntersectionT (_, rep), u)
      when Base.List.mem ~equal:(Concrete_type_eq.eq cx) (InterRep.members rep) u ->
      ()
    (* If we have a TypeAppT (c, ts) ~> TypeAppT (c, ts) then we want to
     * concretize both cs to PolyTs so that we may referentially compare them.
     * We cannot compare the non-concretized versions since they may have been
     * reposition, they may be two OpenTs from different locations, or any other
     * way you can access the same PolyT via different means that results in a
     * different c being passed to TypeAppT.
     *
     * We use the ConcretizeTypeAppsT use type to concretize both the c of our
     * upper and lower TypeAppT bound. We start by concretizing the upper bound
     * which we signal by setting the final element in ConcretizeTypeAppsT to
     * true.
     *
     * The next step happens back in flow_js.ml, at the cases for a
     * ConcretizeTypeAppsT use type.
     *)
    | ( TypeAppT
          { reason = r1; use_op = op1; type_ = c1; targs = ts1; from_value = fv1; use_desc = _ },
        TypeAppT
          { reason = r2; use_op = op2; type_ = c2; targs = ts2; from_value = fv2; use_desc = _ }
      ) ->
      if TypeAppExpansion.push_unless_loop cx `Lower (c1, ts1) then (
        if TypeAppExpansion.push_unless_loop cx `Upper (c2, ts2) then (
          rec_flow
            cx
            trace
            (c2, ConcretizeTypeAppsT (use_op, (ts2, fv2, op2, r2), (c1, ts1, fv1, op1, r1), true));
          TypeAppExpansion.pop cx
        );
        TypeAppExpansion.pop cx
      )
    | ( TypeAppT { reason = reason_tapp; use_op = use_op_tapp; type_; targs; from_value; use_desc },
        _
      ) ->
      let reason_op = reason_of_t u in
      if TypeAppExpansion.push_unless_loop cx `Lower (type_, targs) then (
        let t =
          reposition_reason
            ~trace
            cx
            reason_tapp
            ~use_desc
            (mk_typeapp_instance
               cx
               ~trace
               ~use_op:use_op_tapp
               ~reason_op
               ~reason_tapp
               ~from_value
               type_
               targs
            )
        in
        rec_flow_t cx trace ~use_op (t, u);
        TypeAppExpansion.pop cx
      )
    | ( _,
        TypeAppT { reason = reason_tapp; use_op = use_op_tapp; type_; targs; from_value; use_desc }
      ) ->
      let reason_op = reason_of_t l in
      if TypeAppExpansion.push_unless_loop cx `Upper (type_, targs) then (
        let t =
          mk_typeapp_instance
            cx
            ~trace
            ~use_op:use_op_tapp
            ~reason_op
            ~reason_tapp
            ~from_value
            type_
            targs
        in
        (* We do the slingshot trick here so that we flow l to the results of making the typeapp
         * instead of adding another lower bound to t. We can't use an Annot here, which would do
         * that for us, because ts may not be 0->1, so using them to make an Annot would break
         * invariants that we rely on. In particular, it would force us to traverse AnnotTs to
         * do any propagation, which is extremely costly. *)
        rec_flow cx trace (t, ReposUseT (reason_tapp, use_desc, use_op, l));
        TypeAppExpansion.pop cx
      )
    (**********************)
    (*    opaque types    *)
    (**********************)

    (* If the ids are equal, we use flow_type_args to make sure that the type arguments of each
     * are compatible with each other. If there are no type args, this doesn't do anything *)
    | ( OpaqueT (lreason, { opaque_id = id1; opaque_type_args = ltargs; _ }),
        OpaqueT (ureason, { opaque_id = id2; opaque_type_args = utargs; _ })
      )
      when Opaque.equal_id id1 id2 ->
      flow_type_args cx trace ~use_op lreason ureason ltargs utargs
    (* If the opaque type are from the same logical module, we need to do some structural validation
       in additional to type_args check. *)
    | ( OpaqueT
          ( lreason,
            {
              opaque_id = Opaque.UserDefinedOpaqueTypeId id1;
              opaque_name = name1;
              opaque_type_args = ltargs;
              lower_t = lower_1;
              upper_t = upper_1;
              _;
            }
          ),
        OpaqueT
          ( ureason,
            {
              opaque_id = Opaque.UserDefinedOpaqueTypeId id2;
              opaque_name = name2;
              opaque_type_args = utargs;
              lower_t = lower_2;
              upper_t = upper_2;
              _;
            }
          )
      )
      when TypeUtil.nominal_id_have_same_logical_module
             ~file_options:(Context.file_options cx)
             ~projects_options:(Context.projects_options cx)
             (id1, Some name1)
             (id2, Some name2)
           && List.length ltargs = List.length utargs ->
      (* Check super *)
      if TypeUtil.is_in_common_interface_conformance_check use_op then begin
        let lower_1 = Base.Option.value lower_1 ~default:(EmptyT.make lreason) in
        let lower_2 = Base.Option.value lower_2 ~default:(EmptyT.make ureason) in
        rec_unify
          cx
          trace
          ~use_op:
            (Frame
               ( OpaqueTypeLowerBoundCompatibility
                   { lower = reason_of_t lower_1; upper = reason_of_t lower_1 },
                 use_op
               )
            )
          lower_1
          lower_2;
        let upper_1 = Base.Option.value upper_1 ~default:(MixedT.make lreason) in
        let upper_2 = Base.Option.value upper_2 ~default:(MixedT.make ureason) in
        rec_unify
          cx
          trace
          ~use_op:
            (Frame
               ( OpaqueTypeUpperBoundCompatibility
                   { lower = reason_of_t upper_1; upper = reason_of_t upper_2 },
                 use_op
               )
            )
          upper_1
          upper_2
      end;
      (* Do not check underlying type even if we have access to them, because underlying types
       * are not visible across module boundaries. *)
      (* Check targs *)
      flow_type_args cx trace ~use_op lreason ureason ltargs utargs
    (* If the type is still in the same file it was defined, we allow it to
     * expose its underlying type information *)
    | ( OpaqueT
          (_, { opaque_id = Opaque.UserDefinedOpaqueTypeId opaque_id; underlying_t = Some t; _ }),
        _
      )
      when ALoc.source (opaque_id :> ALoc.t) = Some (Context.file cx) ->
      rec_flow_t cx trace ~use_op (t, u)
    (* If the lower bound is in the same file as where the opaque type was defined,
     * we expose the underlying type information *)
    | ( _,
        OpaqueT
          (_, { opaque_id = Opaque.UserDefinedOpaqueTypeId opaque_id; underlying_t = Some t; _ })
      )
      when ALoc.source (opaque_id :> ALoc.t) = Some (Context.file cx) ->
      rec_flow_t cx trace ~use_op (l, t)
    (***********************)
    (* Numeric string keys *)
    (***********************)
    (*
       This type is only to be used to represent numeric-like object keys in
       the context of object-to-object subtyping.
       It is a subtype of both string and number, so both of these are OK:
       ```
       const o = {1: true};
       o as {[number]: boolean}: // OK
       o as {[string]: boolean}: // OK
       ```
    *)
    | (DefT (rl, NumericStrKeyT (actual, _)), DefT (ru, SingletonNumT { value = (expected, _); _ }))
      ->
      if actual = expected then
        ()
      else
        add_output
          cx
          (Error_message.EExpectedNumberLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, NumericStrKeyT (_, actual)), DefT (ru, SingletonStrT { value = expected; _ })) ->
      if OrdinaryName actual = expected then
        ()
      else
        add_output
          cx
          (Error_message.EExpectedStringLit { reason_lower = rl; reason_upper = ru; use_op })
    | (_, DefT (r, NumericStrKeyT (_, s))) ->
      let u = DefT (r, SingletonStrT { value = OrdinaryName s; from_annot = false }) in
      rec_flow_t cx trace ~use_op (l, u)
    (***********************)
    (* Singletons and keys *)
    (***********************)

    (* Finite keysets over arbitrary objects can be represented by KeysT. While
     * it is possible to also represent singleton string types using KeysT (by
     * taking the keyset of an object with a single property whose key is that
     * string and whose value is ignored), we can model them more directly
     * using SingletonStrT. Specifically, SingletonStrT models a type
     * annotation that looks like a string literal, which describes a singleton
     * set containing that string literal. Going further, other uses of KeysT
     * where the underlying object is created solely for the purpose of
     * describing a keyset can be modeled using unions of singleton strings.

     * One may also legitimately wonder why SingletonStrT(_, key) cannot be
     * always replaced by SingletonStrT(Some key). The reason is that types of the
     * latter form (string literal types) are inferred to be the type of string
     * literals appearing as values, and we don't want to prematurely narrow
     * down the type of the location where such values may appear, since that
     * would preclude other strings to be stored in that location. Thus, by
     * necessity we allow all string types to flow to SingletonStrT (whereas only
     * exactly matching string literal types may flow to SingletonStrT).
     * *)
    | ( DefT (rl, SingletonStrT { value = actual; _ }),
        DefT (ru, SingletonStrT { value = expected; _ })
      ) ->
      if expected = actual then
        Flow_js_utils.update_lit_type_from_annot cx l
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          (Error_message.EExpectedStringLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, StrGeneralT _), DefT (ru, SingletonStrT _)) ->
      (* TODO: ordered_reasons should not be necessary *)
      let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
      add_output
        cx
        (Error_message.EExpectedStringLit { reason_lower = rl; reason_upper = ru; use_op })
    | ( DefT (rl, SingletonNumT { value = (actual, _); _ }),
        DefT (ru, SingletonNumT { value = (expected, _); _ })
      ) ->
      if expected = actual then
        Flow_js_utils.update_lit_type_from_annot cx l
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          (Error_message.EExpectedNumberLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, NumGeneralT _), DefT (ru, SingletonNumT _)) ->
      (* TODO: ordered_reasons should not be necessary *)
      let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
      add_output
        cx
        (Error_message.EExpectedNumberLit { reason_lower = rl; reason_upper = ru; use_op })
    | ( DefT (rl, SingletonBoolT { value = actual; _ }),
        DefT (ru, SingletonBoolT { value = expected; _ })
      ) ->
      if expected = actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          (Error_message.EExpectedBooleanLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, BoolGeneralT), DefT (ru, SingletonBoolT _)) ->
      (* TODO: ordered_reasons should not be necessary *)
      let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
      add_output
        cx
        (Error_message.EExpectedBooleanLit { reason_lower = rl; reason_upper = ru; use_op })
    | ( DefT (rl, SingletonBigIntT { value = (actual, _); _ }),
        DefT (ru, SingletonBigIntT { value = (expected, _); _ })
      ) ->
      if expected = actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          (Error_message.EExpectedBigIntLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, BigIntGeneralT _), DefT (ru, SingletonBigIntT _)) ->
      (* TODO: ordered_reasons should not be necessary *)
      let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
      add_output
        cx
        (Error_message.EExpectedBigIntLit { reason_lower = rl; reason_upper = ru; use_op })
    (*****************************************************)
    (* keys (NOTE: currently we only support string keys *)
    (*****************************************************)
    | ( ( DefT (reason_s, SingletonStrT { value = x; _ })
        | GenericT { reason = reason_s; bound = DefT (_, SingletonStrT { value = x; _ }); _ } ),
        KeysT (reason_op, o)
      ) ->
      Flow_js_utils.update_lit_type_from_annot cx l;
      let reason_next = replace_desc_new_reason (RProperty (Some x)) reason_s in
      (* check that o has key x *)
      let u = HasOwnPropT (use_op, reason_next, l) in
      rec_flow cx trace (o, ReposLowerT { reason = reason_op; use_desc = false; use_t = u })
    | ( ( DefT (reason_s, StrGeneralT _)
        | GenericT { reason = reason_s; bound = DefT (_, StrGeneralT _); _ } ),
        KeysT (reason_op, o)
      ) ->
      let reason_next = replace_desc_new_reason RUnknownString reason_s in
      (* check that o has key x *)
      let u = HasOwnPropT (use_op, reason_next, l) in
      rec_flow cx trace (o, ReposLowerT { reason = reason_op; use_desc = false; use_t = u })
    | ( ( DefT (reason_s, NumericStrKeyT (_, s))
        | GenericT { reason = reason_s; bound = DefT (_, NumericStrKeyT (_, s)); _ } ),
        KeysT (reason_op, o)
      ) ->
      let reason_next = replace_desc_new_reason (RProperty (Some (OrdinaryName s))) reason_s in
      let l = DefT (reason_s, SingletonStrT { value = OrdinaryName s; from_annot = false }) in
      let u = HasOwnPropT (use_op, reason_next, l) in
      rec_flow cx trace (o, ReposLowerT { reason = reason_op; use_desc = false; use_t = u })
    | ( ( StrUtilT { reason = reason_s; op; remainder }
        | GenericT { reason = reason_s; bound = StrUtilT { reason = _; op; remainder }; _ } ),
        KeysT (reason_op, o)
      ) ->
      let l = StrUtilT { reason = reason_s; op; remainder } in
      let u = HasOwnPropT (use_op, reason_s, l) in
      rec_flow cx trace (o, ReposLowerT { reason = reason_op; use_desc = false; use_t = u })
    | (KeysT (reason1, o1), _) ->
      (* flow all keys of o1 to u *)
      rec_flow cx trace (o1, GetKeysT (reason1, UseT (use_op, u)))
    (*********************************************)
    (* Using predicate functions as regular ones *)
    (*********************************************)
    | (UnionT (reason, rep), _) when UnionRep.members rep |> List.exists is_union_resolvable ->
      iter_resolve_union ~f:rec_flow cx trace reason rep (UseT (use_op, u))
    (* cases where there is no loss of precision *)
    | (UnionT (_, rep), UnionT _) -> union_to_union cx trace use_op l rep u
    | (OpaqueT (_, { upper_t = Some (UnionT _ as l); _ }), UnionT _)
      when union_optimization_guard cx TypeUtil.quick_subtype l u
           = UnionOptimizationGuardResult.True ->
      if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path (via an opaque type)"
    (* Optimization to treat maybe and optional types as special unions for subset comparison *)
    | (UnionT (reason, rep), MaybeT (r, maybe)) ->
      let quick_subtype = TypeUtil.quick_subtype in
      let void = VoidT.why r in
      let null = NullT.why r in
      let filter_void t = quick_subtype t void in
      let filter_null t = quick_subtype t null in
      let filter_null_and_void t = filter_void t || filter_null t in
      let maybe = push_type_alias_reason r maybe in
      (* if the union doesn't contain void or null,
         then everything in it must be upper-bounded by maybe *)
      begin
        match
          ( UnionRep.quick_mem_enum ~quick_subtype void rep,
            UnionRep.quick_mem_enum ~quick_subtype null rep
          )
        with
        | (UnionRep.No, UnionRep.No) -> rec_flow_t ~use_op cx trace (l, maybe)
        | (UnionRep.Yes, UnionRep.No) ->
          rec_flow_t ~use_op cx trace (remove_predicate_from_union reason cx filter_void rep, maybe)
        | (UnionRep.No, UnionRep.Yes) ->
          rec_flow_t ~use_op cx trace (remove_predicate_from_union reason cx filter_null rep, maybe)
        | (UnionRep.Yes, UnionRep.Yes) ->
          rec_flow_t
            ~use_op
            cx
            trace
            (remove_predicate_from_union reason cx filter_null_and_void rep, maybe)
        | _ -> flow_all_in_union cx trace rep (UseT (use_op, u))
      end
    | (UnionT (reason, rep), OptionalT { reason = r; type_ = opt; use_desc }) ->
      let quick_subtype = TypeUtil.quick_subtype in
      let void = VoidT.why_with_use_desc ~use_desc r in
      let filter_void t = quick_subtype t void in
      (* if the union doesn't contain void, then everything in it must be upper-bounded by u *)
      begin
        match UnionRep.quick_mem_enum ~quick_subtype void rep with
        | UnionRep.No -> rec_flow_t ~use_op cx trace (l, opt)
        | UnionRep.Yes ->
          rec_flow_t ~use_op cx trace (remove_predicate_from_union reason cx filter_void rep, opt)
        | _ -> flow_all_in_union cx trace rep (UseT (use_op, u))
      end
    | (UnionT _, IntersectionT (_, rep)) ->
      ( if Context.is_verbose cx then
        match l with
        | UnionT _ -> prerr_endline "UnionT ~> IntersectionT slow case"
        | _ -> ()
      );
      InterRep.members rep |> List.iter (fun t -> rec_flow cx trace (l, UseT (use_op, t)))
    | (UnionT (_, rep), _) ->
      ( if Context.is_verbose cx then
        match u with
        | UnionT _ -> prerr_endline "UnionT ~> UnionT slow case"
        | IntersectionT _ -> prerr_endline "UnionT ~> IntersectionT slow case"
        | _ -> ()
      );
      let check elt =
        if not (UnionRep.is_optimized_finally rep) then
          UnionRep.optimize_enum_only ~flatten:(Type_mapper.union_flatten cx) rep;
        match UnionRep.check_enum_with_tag rep with
        | Some (enums, enum_tag) when enum_tag = UnionRep.tag_of_member u ->
          if not (UnionEnumSet.for_all (( = ) elt) enums) then
            add_output
              cx
              (Error_message.EIncompatibleWithUseOp
                 {
                   reason_lower = reason_of_t l;
                   reason_upper = reason_of_t u;
                   use_op;
                   explanation = None;
                 }
              )
        | _ -> flow_all_in_union cx trace rep (UseT (use_op, u))
      in
      begin
        match u with
        | DefT (_, SingletonStrT { value = x; _ }) -> check (UnionEnum.Str x)
        | DefT (_, SingletonBoolT { value = x; _ }) -> check (UnionEnum.Bool x)
        | DefT (_, SingletonNumT { value = x; _ }) -> check (UnionEnum.Num x)
        | _ -> flow_all_in_union cx trace rep (UseT (use_op, u))
      end
    | (_, IntersectionT (_, rep)) ->
      ( if Context.is_verbose cx then
        match l with
        | UnionT _ -> prerr_endline "IntersectionT ~> UnionT slow case"
        | _ -> ()
      );
      InterRep.members rep |> List.iter (fun t -> rec_flow cx trace (l, UseT (use_op, t)))
    (* String enum sets can be handled in logarithmic time by just
     * checking for membership in the set.
     *)
    | (DefT (reason_l, SingletonStrT { value = x; _ }), UnionT (reason_u, rep))
      when match UnionRep.check_enum rep with
           | Some enums ->
             if not (UnionEnumSet.mem (UnionEnum.Str x) enums) then
               add_output
                 cx
                 (Error_message.EIncompatibleWithUseOp
                    { reason_lower = reason_l; reason_upper = reason_u; use_op; explanation = None }
                 );
             true
           | _ -> false ->
      Flow_js_utils.update_lit_type_from_annot cx l
    | (_, UnionT (_, rep))
      when let ts = Type_mapper.union_flatten cx @@ UnionRep.members rep in
           let on_singleton_eq = Flow_js_utils.update_lit_type_from_annot cx in
           List.exists (TypeUtil.quick_subtype ~on_singleton_eq l) ts ->
      ()
    | (DefT (renders_r, RendersT _), UnionT (r, rep)) ->
      (* This is a tricky case because there are multiple ways that it could pass. Either
       * the union contains a supertype of the LHS, or the Union itself is a super type of
       * React.Node, in which case we can pass without splitting the union. Crucially, if the
       * union is a super type of React.Node then splitting the union too early will cause
       * spurious errors.
       *
       * This is further complicated during implicit instantiation, where the union may contain
       * implicitly instantiated tvars that should be constrained by the LHS.
       *
       * To handle these cases, we first check to see if the union contains any implicitly instantiated
       * tvars. If so, we start speculation. If not, we try to see if the RHS is a supertype of React.Node
       * before kicking off regular speculation *)
      let union_contains_instantiable_tvars =
        if Context.in_implicit_instantiation cx then
          UnionRep.members rep
          |> List.exists (fun t ->
                 match t with
                 | OpenT (r, id) ->
                   let open Constraint in
                   (match Context.find_graph cx id with
                   | Resolved _
                   | FullyResolved _ ->
                     false
                   | Unresolved _ -> is_instantiable_reason r)
                 | _ -> false
             )
        else
          false
      in
      let node =
        get_builtin_react_type
          cx
          ~use_desc:true
          renders_r
          Flow_intermediate_error_types.ReactModuleForReactNodeType
      in
      if union_contains_instantiable_tvars || not (speculative_subtyping_succeeds cx node u) then
        SpeculationKit.try_union cx trace use_op l r rep
    (* The following case distributes the opaque constructor over a union/maybe/optional
     * type in the super type position of the opaque type. *)
    | (OpaqueT (lreason, ({ upper_t = Some t; _ } as opaquetype)), UnionT (r, rep)) ->
      let ts = possible_concrete_types_for_inspection cx (reason_of_t t) t in
      begin
        match ts with
        | []
        | [_] ->
          (* Same as `_ ~> UnionT` case below *)
          SpeculationKit.try_union cx trace use_op l r rep
        | lt1 :: lt2 :: lts ->
          let make_opaque t = OpaqueT (lreason, { opaquetype with upper_t = Some t }) in
          let union_of_opaques =
            UnionRep.make (make_opaque lt1) (make_opaque lt2) (Base.List.map ~f:make_opaque lts)
          in
          rec_flow_t cx trace ~use_op (UnionT (lreason, union_of_opaques), u)
      end
    | (_, UnionT (r, rep)) ->
      (* Try the branches of the union in turn, with the goal of selecting the
       * correct branch. This process is reused for intersections as well. See
       * comments on try_union and try_intersection. *)
      let on_success () = Flow_js_utils.update_lit_type_from_annot cx l in
      SpeculationKit.try_union cx ~on_success trace use_op l r rep
    (* maybe and optional types are just special union types *)
    | (t1, MaybeT (r2, t2)) ->
      let t2 = push_type_alias_reason r2 t2 in
      rec_flow cx trace (t1, UseT (use_op, t2))
    | (t1, OptionalT { reason = _; type_ = t2; use_desc = _ }) ->
      rec_flow cx trace (t1, UseT (use_op, t2))
    (*
     * object types: an intersection may satisfy an object UB without
     * any particular member of the intersection doing so completely.
     * Here we trap object UBs with more than one property, and
     * decompose them into singletons.
     *
     * This trap is skipped for exact objects, since intersections of inexact objects
     * can never satisfy exact objects, but it might cause spurious errors.
     *
     * Note: should be able to do this with LookupT rather than
     * slices, but that approach behaves in nonobvious ways. TODO why?
     *)
    | (IntersectionT _, DefT (r, ObjT { flags; props_tmap; proto_t; call_t; reachable_targs = _ }))
      when NameUtils.Map.cardinal (Context.find_props cx props_tmap) > 1 && flags.obj_kind <> Exact
      ->
      Debug_js.Verbose.print_if_verbose cx ["trapped"];
      Context.iter_real_props cx props_tmap (fun x p ->
          let pmap = NameUtils.Map.singleton x p in
          let id = Context.generate_property_map cx pmap in
          let obj = mk_objecttype ~flags ~call:call_t id dummy_prototype in
          rec_flow cx trace (l, UseT (use_op, DefT (r, ObjT obj)))
      );
      rec_flow cx trace (l, UseT (use_op, proto_t))
    (*
     * (After the above preprocessing step, try the branches of the intersection
     * in turn, with the goal of selecting the correct branch. This process is
     * reused for unions as well. See comments on try_union and
     * try_intersection.)
     *)
    | (IntersectionT (r, rep), _) ->
      SpeculationKit.try_intersection cx trace (UseT (use_op, u)) r rep
    | (NullProtoT reason, _) -> rec_flow_t cx trace ~use_op (DefT (reason, NullT), u)
    (************)
    (* StrUtilT *)
    (************)
    (* prefix *)
    | ( StrUtilT { reason = _; op = StrPrefix prefix1; remainder = _ },
        StrUtilT { reason = _; op = StrPrefix prefix2; remainder = None }
      )
      when String.starts_with ~prefix:prefix2 prefix1 ->
      ()
    | ( StrUtilT { reason; op = StrPrefix prefix1; remainder = remainder1 },
        StrUtilT { reason = _; op = StrPrefix prefix2; remainder = Some remainder2 }
      )
      when prefix1 = prefix2 ->
      let remainder1 = Option.value ~default:(StrModuleT.why reason) remainder1 in
      rec_flow_t cx trace ~use_op (remainder1, remainder2)
    | ( DefT (reason, SingletonStrT { value = OrdinaryName s; _ }),
        StrUtilT { reason = _; op = StrPrefix prefix; remainder }
      )
      when String.starts_with ~prefix s ->
      Flow_js_utils.update_lit_type_from_annot cx l;
      Base.Option.iter remainder ~f:(fun remainder ->
          let chopped = Base.String.chop_prefix_exn ~prefix s in
          let reason = replace_desc_reason (RStringWithoutPrefix { prefix }) reason in
          let str_t =
            DefT (reason, SingletonStrT { value = OrdinaryName chopped; from_annot = true })
          in
          rec_flow_t cx trace ~use_op (str_t, remainder)
      )
    (* suffix *)
    | ( StrUtilT { reason = _; op = StrSuffix suffix1; remainder = _ },
        StrUtilT { reason = _; op = StrSuffix suffix2; remainder = None }
      )
      when String.ends_with ~suffix:suffix2 suffix1 ->
      ()
    | ( StrUtilT { reason; op = StrSuffix suffix1; remainder = remainder1 },
        StrUtilT { reason = _; op = StrSuffix suffix2; remainder = Some remainder2 }
      )
      when suffix1 = suffix2 ->
      let remainder1 = Option.value ~default:(StrModuleT.why reason) remainder1 in
      rec_flow_t cx trace ~use_op (remainder1, remainder2)
    | ( DefT (reason, SingletonStrT { value = OrdinaryName s; _ }),
        StrUtilT { reason = _; op = StrSuffix suffix; remainder }
      )
      when String.ends_with ~suffix s ->
      Flow_js_utils.update_lit_type_from_annot cx l;
      Base.Option.iter remainder ~f:(fun remainder ->
          let chopped = Base.String.chop_suffix_exn ~suffix s in
          let reason = replace_desc_reason (RStringWithoutSuffix { suffix }) reason in
          let str_t =
            DefT (reason, SingletonStrT { value = OrdinaryName chopped; from_annot = true })
          in
          rec_flow_t cx trace ~use_op (str_t, remainder)
      )
    (* both *)
    | (StrUtilT { reason; op = StrPrefix arg | StrSuffix arg; remainder = _ }, _) ->
      let literal_kind =
        if arg = "" then
          AnyLiteral
        else
          Truthy
      in
      rec_flow_t cx trace ~use_op (DefT (reason, StrGeneralT literal_kind), u)
    (*
     * When do we consider a polymorphic type <X:U> T to be a subtype of another
     * polymorphic type <X:U'> T'? This is the subject of a long line of
     * research. A rule that works (Cardelli/Wegner) is: force U = U', and prove
     * that T is a subtype of T' for any X:U'. A more general rule that proves
     * that U' is a subtype of U instead of forcing U = U' is known to cause
     * undecidable subtyping (Pierce): the counterexamples are fairly
     * pathological, but can be reliably constructed by exploiting the "switch"
     * of bounds from U' to U (and back, with sufficient trickery), in ways that
     * are difficult to detect statically.
     *
     * However, these results are somewhat tricky to interpret in Flow, since we
     * are not proving stuff inductively: instead we are co-inductively assuming
     * what we want to prove, and checking consistency.
     *
     * Separately, none of these rules capture the logical interpretation of the
     * original subtyping question (interpreting subtyping as implication, and
     * polymorphism as universal quantification). What we really want to show is
     * that, for all X:U', there is some X:U such that T is a subtype of T'. But
     * we already deal with statements of this form when checking polymorphic
     * definitions! In particular, statements such as "there is some X:U...")
     * correspond to "create a type variable with that constraint and ...", and
     * statements such as "show that for all X:U" correspond to "introduce a
     * GenericT with bound U".
     *
     * Thus, all we need to do when checking that any type flows to a
     * polymorphic type is to follow the same principles used when checking that
     * a polymorphic definition has a polymorphic type. This has the pleasant
     * side effect that the type we're checking does not itself need to be a
     * polymorphic type at all! For example, we can let a non-generic method be
     * overridden with a generic method, as long as the non-generic signature
     * can be derived as a specialization of the generic signature.
     *)
    (* some shortcuts **)
    | (DefT (_, PolyT { id = id1; _ }), DefT (_, PolyT { id = id2; _ })) when Poly.equal_id id1 id2
      ->
      if Context.is_verbose cx then prerr_endline "PolyT ~> PolyT fast path"
    | ( DefT (_, PolyT { tparams = params1; t_out = t1; _ }),
        DefT (_, PolyT { tparams = params2; t_out = t2; _ })
      )
      when let n1 = Nel.length params1 in
           let n2 = Nel.length params2 in
           n1 = n2 ->
      (* A description of this subtyping rule can be found in "Decidable Bounded
       * Quantifcation" by G. Castagna B. Pierce.
       *
       *   G |- T1 <: S1   G, { X: S1 } |- S2 <: T2
       *   ------------------------------------------ (All-local)
       *   G |- forall X:S1 . S2 <: forall X:T1 . T2
       *
       * The code below implements this rule for the slightly more general case of
       * subtyping between two polymorphic types:
       *
       * forall a1:b1  , ... ai:bi  , ..., an:bn   . t
       * forall a1':b1', ... ai':bi', ..., an':bn' . t'
       *
       * 1st Premise
       * -----------
       * For each type parameter pair (ai:bi, ai':bi') we create the constraints:
       *
       * bi[ai, ..., a_(i-1)] <: bi'[ai'/ai, ..., a_(i-1)'/a_(i-1)]
       *
       * where ai is a GenericT
       *)
      let (map1, map2) =
        Base.List.fold2_exn
          (Nel.to_list params1)
          (Nel.to_list params2)
          ~init:(Subst_name.Map.empty, Subst_name.Map.empty)
          ~f:(fun (prev_map1, prev_map2) param1 param2 ->
            let bound2 = Type_subst.subst cx ~use_op prev_map2 param2.bound in
            rec_flow cx trace (bound2, UseT (use_op, param1.bound));
            if param1.is_const <> param2.is_const then
              add_output
                cx
                (Error_message.ETypeParamConstIncompatibility
                   { use_op; lower = param1.reason; upper = param2.reason }
                );
            let (gen, map1) = Flow_js_utils.generic_bound cx prev_map1 param1 in
            let map2 = Subst_name.Map.add param2.name gen prev_map2 in
            (map1, map2)
        )
      in

      (* 2nd Premise
       * -----------
       * We check t <: t' after substituting ai' for ai
       *)
      let t1 = Type_subst.subst cx ~use_op map1 t1 in
      let t2 = Type_subst.subst cx ~use_op map2 t2 in
      rec_flow_t ~use_op cx trace (t1, t2)
    (* general case **)
    | (_, DefT (_, PolyT { t_out = t; _ })) -> rec_flow cx trace (l, UseT (use_op, t))
    (* TODO: ideally we'd do the same when lower bounds flow to a
     * this-abstracted class, but fixing the class is easier; might need to
     * revisit *)
    | (_, DefT (class_r, ClassT (ThisInstanceT (inst_r, i, this, this_name)))) ->
      let reason = reason_of_t l in
      rec_flow
        cx
        trace
        ( l,
          UseT
            ( use_op,
              DefT (class_r, ClassT (fix_this_instance cx reason (inst_r, i, this, this_name)))
            )
        )
    | (_, ThisInstanceT (r, i, this, this_name)) ->
      let reason = reason_of_t l in
      rec_flow cx trace (l, UseT (use_op, fix_this_instance cx reason (r, i, this, this_name)))
    (*
     * This rule is hit when a polymorphic type appears outside a
     * type application expression - i.e. not followed by a type argument list
     * delimited by angle brackets.
     * We want to require full expressions in type positions like annotations,
     * but allow use of polymorphically-typed values - for example, in class
     * extends clauses and at function call sites - without explicit type
     * arguments, since typically they're easily inferred from context.
     *)
    | (DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }), _) ->
      let reason_op = reason_of_t u in
      let (t_, _) =
        instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, ids, t)
      in
      rec_flow_t cx trace ~use_op (t_, u)
    (* when a this-abstracted class flows to upper bounds, fix the class *)
    | (DefT (class_r, ClassT (ThisInstanceT (inst_r, i, this, this_name))), _) ->
      let reason = reason_of_t u in
      rec_flow_t
        cx
        trace
        ~use_op
        (DefT (class_r, ClassT (fix_this_instance cx reason (inst_r, i, this, this_name))), u)
    | (ThisInstanceT (r, i, this, this_name), _) ->
      let reason = reason_of_t u in
      rec_flow_t cx trace ~use_op (fix_this_instance cx reason (r, i, this, this_name), u)
    (*****************************)
    (* React Abstract Components *)
    (*****************************)
    (*
     * In all of these cases, we check:
     *  1. configu <: configl
     *  2. default_propsl = default_propsu
     *  3. instancel <: instanceu
     *  4. rendersl <: rendersu
     *
     *  2. is necessary because we allow the default props of a component to be read and
     *  written.
     *
     *  1. Is necessary because we need to ensure that any config object that is passed to u
     *  is compatible with the config of l. This also is sufficient; unification is not required.
     *  We can think of AbstractComponents as some sort of callable that accepts a config object.
     *  The only place that the config object type would appear is in the callable signature, which
     *  is contravariant.
     *
     *  In reality, a component is turned into an element via createElement, which accepts a
     *  component and a config object. From there, it creates an object that will become the
     *  props of a component by combining the config object with the component's default props.
     *  This process creates a new fresh unaliased props object, which is passed to the component.
     *
     *  3. Is necessary because we need to ensure the ref passed in is compatible with the instance
     *  type of the component. React will assign ref.current to the instance of the component, so we
     *  need to ensure that the type we assign is compatible with the type ref.current.
     *
     *  4. Is necessary because we need to ensure the element returned from the render function is
     *  compatible with any places that the component can be rendered. This is often used by
     *  components that only accept specific components as children.
     *)

    (* Class component ~> AbstractComponent *)
    | ( DefT (reasonl, ClassT this),
        DefT (_, ReactAbstractComponentT { config; instance; renders; component_kind = Structural })
      ) ->
      (* Contravariant config check *)
      Flow.react_get_config
        cx
        trace
        l
        ~use_op
        ~reason_op:reasonl
        (React.GetConfig l)
        Polarity.Negative
        config;

      flow_react_component_instance_to_instance
        cx
        trace
        use_op
        ( ComponentInstanceAvailableAsRefSetterProp
            (get_builtin_react_typeapp
               cx
               ~use_desc:true
               (replace_desc_reason
                  (RTypeAppImplicit
                     (RTypeAlias ("React.RefSetter", None, RType (OrdinaryName "React.RefSetter")))
                  )
                  (reason_of_t this)
               )
               Flow_intermediate_error_types.ReactModuleForReactRefSetterType
               [this]
            ),
          instance
        );
      (* check rendersl <: rendersu *)
      Flow.react_subtype_class_component_render cx trace ~use_op this ~reason_op:reasonl renders
    (* Function Component ~> AbstractComponent *)
    | ( DefT (reasonl, FunT (_, { return_t; _ })),
        DefT
          ( _reasonu,
            ReactAbstractComponentT { config; instance; renders; component_kind = Structural }
          )
      ) ->
      (* Function components will not always have an annotation, so the config may
       * never resolve. To determine config compatibility, we instead
       * call createElement on the function with the given component to determine
       * the compatibility.
       *
       * We use ConfigCheck instead of CreateElement because:
       *  1. We can't perform the key check. If config is mixed, which can happen in
       *  polymorphic HOCs then the [string]: mixed indexer causes spurious errors.
       *  2. We check the ref here, so we don't need to check it in the config as well.
       *)
      rec_flow
        cx
        trace
        (l, ReactKitT (use_op, reasonl, React.ConfigCheck { props = config; instance }));

      (* check rendered elements are covariant *)
      rec_flow_t cx trace ~use_op (return_t, renders)
    (* Object Component ~> AbstractComponent *)
    | ( DefT (reasonl, ObjT { call_t = Some id; _ }),
        DefT
          ( reasonu,
            ReactAbstractComponentT { config; instance; renders; component_kind = Structural }
          )
      ) ->
      rec_flow
        cx
        trace
        (l, ReactKitT (use_op, reasonl, React.ConfigCheck { props = config; instance }));

      (* Ensure the callable signature's return type is compatible with the rendered element (renders). We
       * do this by flowing it to (...empty): renders *)
      let funtype =
        mk_functiontype
          reasonu
          []
          ~rest_param:
            (Some (None, loc_of_reason reasonu, EmptyT.why (replace_desc_new_reason REmpty reasonu)))
          ~def_reason:reasonl
          ~type_guard:None
          renders
      in
      let mixed = MixedT.why reasonu in
      rec_flow_t ~use_op cx trace (Context.find_call cx id, DefT (reasonu, FunT (mixed, funtype)))
    (* AbstractComponent ~> AbstractComponent *)
    | ( DefT
          ( reasonl,
            ReactAbstractComponentT
              { config = configl; instance = instancel; renders = rendersl; component_kind }
          ),
        DefT
          ( _reasonu,
            ReactAbstractComponentT
              {
                config = configu;
                instance = instanceu;
                renders = rendersu;
                component_kind = Structural;
              }
          )
      ) ->
      rec_flow_t cx trace ~use_op (configu, configl);
      flow_react_component_instance_to_instance cx trace use_op (instancel, instanceu);
      let rendersl =
        match component_kind with
        | Nominal (renders_id, renders_name, _) ->
          let reason = update_desc_reason (fun desc -> RRenderType desc) reasonl in
          DefT
            ( reason,
              RendersT (NominalRenders { renders_id; renders_name; renders_super = rendersl })
            )
        | Structural -> rendersl
      in
      rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (rendersl, rendersu)
    | ( DefT
          ( _reasonl,
            ReactAbstractComponentT
              {
                config = configl;
                instance = instancel;
                renders = rendersl;
                component_kind = Nominal (idl, name_l, _);
              }
          ),
        DefT
          ( _reasonu,
            ReactAbstractComponentT
              {
                config = configu;
                instance = instanceu;
                renders = rendersu;
                component_kind = Nominal (idu, name_u, _);
              }
          )
      )
      when ALoc.equal_id idl idu
           || TypeUtil.nominal_id_have_same_logical_module
                ~file_options:(Context.file_options cx)
                ~projects_options:(Context.projects_options cx)
                (idl, Some name_l)
                (idu, Some name_u) ->
      rec_flow_t cx trace ~use_op (configu, configl);
      flow_react_component_instance_to_instance cx trace use_op (instancel, instanceu);
      rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (rendersl, rendersu)
    | (DefT (reasonl, RendersT r1), DefT (reasonu, RendersT r2)) ->
      RendersKit.rec_renders_to_renders cx trace ~use_op ((reasonl, r1), (reasonu, r2))
    | (l, DefT (renders_r, RendersT upper_renders)) ->
      RendersKit.non_renders_to_renders cx trace ~use_op l (renders_r, upper_renders)
    (* Exiting the renders world *)
    | (DefT (r, RendersT (IntrinsicRenders _ | NominalRenders _)), u) ->
      let mixed_element =
        get_builtin_react_type
          cx
          r
          Flow_intermediate_error_types.ReactModuleForReactMixedElementType
      in
      rec_flow_t cx trace ~use_op (mixed_element, u)
    | ( DefT (r, RendersT (StructuralRenders { renders_variant = _; renders_structural_type = t })),
        u
      ) ->
      let u' = ExitRendersT { renders_reason = r; u = UseT (use_op, u) } in
      rec_flow cx trace (t, u')
    | (DefT (r, RendersT DefaultRenders), u) ->
      let u' = ExitRendersT { renders_reason = r; u = UseT (use_op, u) } in
      rec_flow cx trace (l, u')
    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    (* FunT ~> FunT *)
    | (DefT (lreason, FunT (_, ft1)), DefT (ureason, FunT (_, ft2))) ->
      let use_op =
        Frame
          ( FunCompatibility { lower = lreason; upper = ureason },
            (* The $call PropertyCompatibility is redundant when we have a
             * FunCompatibility use_op. *)
            match use_op with
            | Frame (PropertyCompatibility { prop = Some (OrdinaryName "$call"); _ }, use_op) ->
              use_op
            | _ -> use_op
          )
      in

      begin
        let use_op =
          Frame (FunParam { n = 0; name = Some "this"; lower = lreason; upper = ureason }, use_op)
        in
        let (this_param1, this_status_1) = ft1.this_t in
        let (this_param2, this_status_2) = ft2.this_t in
        match (this_status_1, this_status_2) with
        | (This_Method _, This_Method _) ->
          rec_flow
            cx
            trace
            (subtype_this_of_function ft2, UseT (use_op, subtype_this_of_function ft1))
        (* lower bound method, upper bound function
           This is always banned, as it would allow methods to be unbound through casting *)
        | (This_Method { unbound }, This_Function) ->
          if not unbound then
            add_output
              cx
              (Error_message.EMethodUnbinding
                 { use_op; reason_op = lreason; reason_prop = reason_of_t this_param1 }
              );
          rec_flow cx trace (this_param2, UseT (use_op, subtype_this_of_function ft1))
        (* lower bound function, upper bound method.
           Ok as long as the types match up *)
        | (This_Function, This_Method _)
        (* Both functions *)
        | (This_Function, This_Function) ->
          rec_flow cx trace (this_param2, UseT (use_op, this_param1))
      end;
      let args = List.rev_map (fun (_, t) -> Arg t) ft2.params in
      let args =
        List.rev
          (match ft2.rest_param with
          | Some (_, _, rest) -> SpreadArg rest :: args
          | None -> args)
      in
      multiflow_subtype cx trace ~use_op ureason args ft1;

      begin
        match (ft1.effect_, ft2.effect_) with
        | (AnyEffect, _)
        | (_, AnyEffect)
        | (ArbitraryEffect, ArbitraryEffect)
        | ((HookDecl _ | HookAnnot), HookAnnot) ->
          ()
        | (HookDecl a, HookDecl b) when ALoc.equal_id a b -> ()
        | ((HookDecl _ | HookAnnot), ArbitraryEffect) ->
          add_output
            cx
            (Error_message.EHookIncompatible
               {
                 use_op;
                 lower = lreason;
                 upper = ureason;
                 lower_is_hook = true;
                 hook_is_annot = ft1.effect_ = HookAnnot;
               }
            )
        | (ArbitraryEffect, (HookDecl _ | HookAnnot)) ->
          add_output
            cx
            (Error_message.EHookIncompatible
               {
                 use_op;
                 lower = lreason;
                 upper = ureason;
                 lower_is_hook = false;
                 hook_is_annot = ft2.effect_ = HookAnnot;
               }
            )
        | ((HookDecl _ | HookAnnot), HookDecl _) ->
          add_output
            cx
            (Error_message.EHookUniqueIncompatible { use_op; lower = lreason; upper = ureason })
      end;

      (* Return type subtyping *)
      let ret_use_op =
        Frame
          (FunReturn { lower = reason_of_t ft1.return_t; upper = reason_of_t ft2.return_t }, use_op)
      in
      rec_flow cx trace (ft1.return_t, UseT (ret_use_op, ft2.return_t));

      begin
        match (ft1.type_guard, ft2.type_guard) with
        | (None, Some (TypeGuard _)) ->
          (* Non-predicate functions are incompatible with predicate ones
             TODO: somehow the original flow needs to be propagated as well *)
          add_output
            cx
            (Error_message.ETypeGuardFuncIncompatibility { use_op; reasons = (lreason, ureason) })
        | ( Some
              (TypeGuard
                { reason = r1; one_sided = impl1; param_name = x1; type_guard = t1; inferred = _ }
                ),
            Some
              (TypeGuard
                { reason = r2; one_sided = impl2; param_name = x2; type_guard = t2; inferred = _ }
                )
          ) ->
          func_type_guard_compat
            cx
            trace
            use_op
            (r1, ft1.params, impl1, x1, t1)
            (r2, ft2.params, impl2, x2, t2)
        | (Some _, None)
        | (None, None) ->
          ()
      end
    (* unwrap namespace type into object type, drop all information about types in the namespace *)
    | (NamespaceT { namespace_symbol = _; values_type; types_tmap = _ }, _) ->
      rec_flow_t cx trace ~use_op (values_type, u)
    | (l, NamespaceT { namespace_symbol = _; values_type; types_tmap = _ }) ->
      rec_flow_t cx trace ~use_op (l, values_type)
    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    (* ObjT -> ObjT *)
    | ( DefT (lreason, ObjT ({ props_tmap = lflds; call_t = l_call_id; _ } as l_obj)),
        DefT (ureason, ObjT ({ props_tmap = uflds; call_t = r_call_id; _ } as u_obj))
      ) ->
      let u_deft = u in
      Type_inference_hooks_js.dispatch_obj_to_obj_hook cx l u_deft;
      let print_fast_path = Context.is_verbose cx in
      if Properties.equal_id lflds uflds && l_call_id = r_call_id then (
        if print_fast_path then prerr_endline "ObjT ~> ObjT fast path: yes"
      ) else (
        if print_fast_path then prerr_endline "ObjT ~> ObjT fast path: no";
        flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj)
      )
    | (DefT (_, ObjT _), NullProtoT _) -> ()
    (* InstanceT -> ObjT *)
    | (DefT (lreason, InstanceT _), DefT (ureason, ObjT { flags = { obj_kind = Exact; _ }; _ })) ->
      let reasons = FlowError.ordered_reasons (lreason, ureason) in
      add_output
        cx
        (Error_message.EIncompatibleWithExact
           (reasons, use_op, Flow_intermediate_error_types.UnexpectedInexact)
        )
    | ( DefT
          ( lreason,
            InstanceT
              {
                super;
                inst = { own_props = lown; proto_props = lproto; inst_call_t = lcall; _ };
                _;
              }
          ),
        DefT (ureason, ObjT { props_tmap = uflds; proto_t = uproto; call_t = ucall; _ })
      ) ->
      add_output cx (Error_message.EClassToObject (lreason, ureason, use_op));
      let lflds =
        let own_props = Context.find_props cx lown in
        let proto_props = Context.find_props cx lproto in
        NameUtils.Map.union own_props proto_props
      in
      Base.Option.iter ucall ~f:(fun ucall ->
          let prop_name = Some (OrdinaryName "$call") in
          match lcall with
          | Some lcall ->
            rec_flow cx trace (Context.find_call cx lcall, UseT (use_op, Context.find_call cx ucall))
          | None ->
            let error_message =
              Error_message.EPropNotFoundInSubtyping
                {
                  reason_lower = lreason;
                  reason_upper = ureason;
                  prop_name;
                  use_op;
                  suggestion = None;
                }
            in
            add_output cx error_message
      );
      let errs =
        Context.fold_real_props
          cx
          uflds
          (fun name up acc ->
            let propref =
              mk_named_prop ~reason:(replace_desc_reason (RProperty (Some name)) ureason) name
            in
            match NameUtils.Map.find_opt name lflds with
            | Some lp ->
              let use_op =
                Frame
                  ( PropertyCompatibility { prop = Some name; lower = lreason; upper = ureason },
                    use_op
                  )
              in
              let errs =
                rec_flow_p cx ~trace ~use_op propref (Property.type_ lp, Property.type_ up)
              in
              Base.List.rev_append errs acc
            | _ ->
              let lookup_kind =
                match up with
                | Field { type_ = OptionalT _; _ } -> NonstrictReturning (None, None)
                | _ -> Strict lreason
              in
              rec_flow
                cx
                trace
                ( super,
                  ReposLowerT
                    {
                      reason = lreason;
                      use_desc = false;
                      use_t =
                        LookupT
                          {
                            reason = ureason;
                            lookup_kind;
                            try_ts_on_failure = [];
                            propref;
                            lookup_action =
                              LookupPropForSubtyping
                                {
                                  use_op;
                                  prop = Property.type_ up;
                                  prop_name = name;
                                  reason_lower = lreason;
                                  reason_upper = ureason;
                                };
                            method_accessible = false;
                            ids = Some (Properties.Set.of_list [lown; lproto]);
                            ignore_dicts = false;
                          };
                    }
                );
              acc)
          []
      in
      add_output_prop_polarity_mismatch cx use_op (lreason, ureason) errs;
      rec_flow cx trace (l, UseT (use_op, uproto))
    (* For some object `x` and constructor `C`, if `x instanceof C`, then the
     * object is a subtype. We use `ExtendsUseT` to walk the proto chain of the
     * object, in case it includes a nominal type. *)
    | (DefT (_, ObjT _), DefT (_, InstanceT _)) -> rec_flow cx trace (l, extends_use_type use_op l u)
    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | (DefT (reason, (ObjT _ | InstanceT _)), DefT (reason_op, FunT _)) ->
      let fun_t =
        match l with
        | DefT (_, ObjT { call_t = Some id; _ })
        | DefT (_, InstanceT { inst = { inst_call_t = Some id; _ }; _ }) ->
          Context.find_call cx id
        | _ ->
          let error_message =
            Error_message.EIncompatibleWithUseOp
              {
                reason_lower = reason;
                reason_upper = reason_op;
                use_op;
                explanation =
                  Some Flow_intermediate_error_types.ExplanationNonCallableObjectToFunction;
              }
          in
          add_output cx error_message;
          AnyT.error reason_op
      in
      let use_op =
        Frame
          ( PropertyCompatibility
              { prop = Some (OrdinaryName "$call"); lower = reason; upper = reason_op },
            use_op
          )
      in
      rec_flow_t cx trace ~use_op (fun_t, u)
    (********************************************)
    (* array types deconstruct into their parts *)
    (********************************************)

    (* Arrays can flow to arrays *)
    | ( DefT (r1, ArrT (ArrayAT { elem_t = t1; tuple_view = tv1; react_dro = _ })),
        DefT (r2, ArrT (ArrayAT { elem_t = t2; tuple_view = tv2; react_dro = _ }))
      ) ->
      let use_op = Frame (ArrayElementCompatibility { lower = r1; upper = r2 }, use_op) in
      let lit1 =
        match desc_of_reason r1 with
        | RArrayLit_UNSOUND
        | RReactChildren ->
          true
        | RRestArrayLit
            ( RCode _ | RFunctionCall _ | RConstructorCall _ | RMethodCall _ | RJSXFunctionCall _
            | RFunctionType ) ->
          (* These cases correspond to calls so the rest array can be considered "literal". *)
          true
        | _ -> false
      in
      let ts1 =
        Base.Option.value_map
          ~default:[]
          ~f:(fun (TupleView { elements; arity = _; inexact = _ }) -> tuple_ts_of_elements elements)
          tv1
      in
      let ts2 =
        Base.Option.value_map
          ~default:[]
          ~f:(fun (TupleView { elements; arity = _; inexact = _ }) -> tuple_ts_of_elements elements)
          tv2
      in
      array_flow cx trace use_op lit1 r1 (ts1, t1, ts2, t2)
    (* Tuples can flow to tuples with the same arity *)
    | ( DefT
          ( r1,
            ArrT
              (TupleAT
                {
                  elem_t = _;
                  elements = elements1;
                  arity = lower_arity;
                  inexact = lower_inexact;
                  react_dro = _;
                }
                )
          ),
        DefT
          ( r2,
            ArrT
              (TupleAT
                {
                  elem_t = _;
                  elements = elements2;
                  arity = upper_arity;
                  inexact = upper_inexact;
                  react_dro = _;
                }
                )
          )
      ) ->
      let fresh = is_literal_array_reason r1 in
      let (num_req1, num_total1) = lower_arity in
      let (num_req2, num_total2) = upper_arity in
      if
        not
          ((upper_inexact || not lower_inexact)
          && num_req1 >= num_req2
          && (num_total1 <= num_total2 || upper_inexact)
          )
      then
        add_output
          cx
          (Error_message.ETupleArityMismatch
             {
               use_op;
               lower_reason = r1;
               lower_arity;
               lower_inexact;
               upper_reason = r2;
               upper_arity;
               upper_inexact;
               unify = false;
             }
          )
      else
        let n = ref 0 in
        let tuple_element_compat t1 t2 p1 p2 optional1 optional2 =
          if not (fresh || Polarity.compat (p1, p2)) then
            add_output
              cx
              (Error_message.ETupleElementPolarityMismatch
                 {
                   index = !n;
                   reason_lower = r1;
                   polarity_lower = p1;
                   reason_upper = r2;
                   polarity_upper = p2;
                   use_op;
                 }
              );
          let use_op =
            Frame
              ( TupleElementCompatibility
                  {
                    n = !n;
                    lower = r1;
                    upper = r2;
                    lower_optional = optional1;
                    upper_optional = optional2;
                  },
                use_op
              )
          in
          (* We don't want to allow `undefined` when an element is marked as optional:
           * ```
           * type T = [number, b?: string];
           * ([0, undefined]: T); // Should error
           * ([0]: T); // Should be ok
           * ([0, 's']: T); // Should be ok
           * ```
           * A user can always add `| void` to the element type if they want to denote this.
           *)
          let (t1, t2) =
            match (optional1, (optional2, t2)) with
            | (false, (true, OptionalT { type_ = t2; _ })) -> (t1, t2)
            | _ -> (t1, t2)
          in
          match (fresh, p2) with
          | (true, _)
          | (_, Polarity.Positive) ->
            rec_flow_t cx trace ~use_op (t1, t2)
          | (_, Polarity.Negative) -> rec_flow_t cx trace ~use_op (t2, t1)
          | (_, Polarity.Neutral) -> rec_unify cx trace ~use_op t1 t2
        in
        iter2opt
          (fun t1 t2 ->
            match (t1, t2) with
            | ( Some
                  (TupleElement
                    { t = t1; polarity = p1; name = _; optional = optional1; reason = _ }
                    ),
                Some
                  (TupleElement
                    { t = t2; polarity = p2; name = _; optional = optional2; reason = _ }
                    )
              ) ->
              tuple_element_compat t1 t2 p1 p2 optional1 optional2;
              n := !n + 1
            | ( None,
                Some
                  (TupleElement
                    { t = t2; polarity = p2; name = _; optional = optional2; reason = _ }
                    )
              ) ->
              let p1 = Polarity.Neutral in
              let t1 = VoidT.make (replace_desc_new_reason (RTupleOutOfBoundsAccess !n) r1) in

              let optional1 = true in
              tuple_element_compat t1 t2 p1 p2 optional1 optional2;
              n := !n + 1
            | _ -> ())
          (elements1, elements2)
    (* Arrays with known elements can flow to tuples *)
    | (DefT (r1, ArrT (ArrayAT { elem_t = t1; tuple_view; react_dro })), DefT (r2, ArrT (TupleAT _)))
      -> begin
      match tuple_view with
      | None -> add_output cx (Error_message.ENonLitArrayToTuple ((r1, r2), use_op))
      | Some (TupleView { elements; arity; inexact }) ->
        rec_flow_t
          cx
          trace
          ~use_op
          (DefT (r1, ArrT (TupleAT { elem_t = t1; elements; arity; inexact; react_dro })), u)
    end
    (* Read only arrays are the super type of all tuples and arrays *)
    | ( DefT (r1, ArrT (ArrayAT { elem_t = t1; _ } | TupleAT { elem_t = t1; _ } | ROArrayAT (t1, _))),
        DefT (r2, ArrT (ROArrayAT (t2, _)))
      ) ->
      let use_op = Frame (ArrayElementCompatibility { lower = r1; upper = r2 }, use_op) in
      rec_flow cx trace (t1, UseT (use_op, t2))
    | (DefT (_, InstanceT _), DefT (r2, ArrT (ArrayAT { elem_t; _ }))) ->
      let arrt = get_builtin_typeapp cx r2 "Array" [elem_t] in
      rec_flow cx trace (l, UseT (use_op, arrt))
    | (DefT (_, InstanceT _), DefT (r2, ArrT (ROArrayAT (elemt, _)))) ->
      let arrt = get_builtin_typeapp cx r2 "$ReadOnlyArray" [elemt] in
      rec_flow cx trace (l, UseT (use_op, arrt))
    (**************************************************)
    (* instances of classes follow declared hierarchy *)
    (**************************************************)
    | (DefT (_, InstanceT _), DefT (_, InstanceT _)) ->
      rec_flow cx trace (l, extends_use_type use_op l u)
    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)
    | (DefT (rl, ClassT l), DefT (_, ClassT u)) ->
      rec_flow cx trace (reposition cx ~trace (loc_of_reason rl) l, UseT (use_op, u))
    (***********************************************)
    (* You can use a function as a callable object *)
    (***********************************************)
    | (DefT (_, FunT _), DefT (reason, ObjT ({ call_t = Some id; _ } as o))) ->
      let t = Context.find_call cx id in
      rec_flow cx trace (l, UseT (use_op, t));
      rec_flow_t cx trace ~use_op (l, DefT (reason, ObjT { o with call_t = None }))
    | ( DefT (_, FunT _),
        DefT
          (reason, InstanceT { static; super; implements; inst = { inst_call_t = Some id; _ } as i })
      ) ->
      let t = Context.find_call cx id in
      rec_flow cx trace (l, UseT (use_op, t));
      rec_flow_t
        cx
        trace
        ~use_op
        ( l,
          DefT
            (reason, InstanceT { static; super; implements; inst = { i with inst_call_t = None } })
        )
    (* FunT ~> ObjT *)
    (*
     * Previously, call properties were stored in the props map, and were
     * checked against dictionary upper bounds. This is wrong, but useful for
     * distinguishing between thunk-like types found in graphql-js.
     *
     * Now that call properties are stored separately, it is particularly
     * egregious to emit this constraint. This only serves to maintain buggy
     * behavior, which should be fixed, and this code removed.
     *)
    | ( DefT (lreason, FunT _),
        DefT (ureason, ObjT { flags = { obj_kind = (Exact | Indexed _) as obj_kind; _ }; _ })
      ) ->
      let reasons = FlowError.ordered_reasons (lreason, ureason) in
      (match obj_kind with
      | Exact ->
        add_output
          cx
          (Error_message.EIncompatibleWithExact
             (reasons, use_op, Flow_intermediate_error_types.UnexpectedInexact)
          )
      | Indexed _ -> add_output cx (Error_message.EFunctionIncompatibleWithIndexer (reasons, use_op))
      | _ -> failwith "Impossible")
    (*
     * TODO: This rule doesn't interact very well with union-type checking. It
     * looks up Function.prototype, which currently doesn't appear structurally
     * in the function type, and thus may not be fully resolved when the
     * function type is checked with a union containing the object
     * type. Ideally, we should either add Function.prototype to function types
     * or fully resolve them when resolving function types, but either way we
     * might bomb perf without additional work. Meanwhile, we need an immediate
     * fix for the common case where this bug shows up. So leaving this comment
     * here as a marker for future work, while going with a band-aid solution
     * for now, as motivated below.
     *
     * Fortunately, it is quite hard for a function type to successfully
     * check against an object type, and even more unlikely when the latter
     * is part of a union: the object type must only contain
     * Function.prototype methods or statics. Quickly confirming that the
     * check would fail before looking up Function.prototype (while falling
     * back to the general rule when we cannot guarantee failure) is a safe
     * optimization in any case, and fixes the commonly observed case where
     * the union type contains both a function type and a object type as
     * members, clearly intending for function types to match the former
     * instead of the latter.
     *)
    | (DefT (reason, FunT (statics, _)), DefT (reason_o, ObjT { props_tmap; _ })) ->
      if
        not
          (quick_error_fun_as_obj
             cx
             ~use_op
             reason
             statics
             reason_o
             (Context.find_props cx props_tmap)
          )
      then
        rec_flow_t cx trace ~use_op (statics, u)
    (* TODO: similar concern as above *)
    | ( DefT (reason, FunT (statics, _)),
        DefT (reason_inst, InstanceT { inst = { own_props; inst_kind = InterfaceKind _; _ }; _ })
      ) ->
      if
        not
          (quick_error_fun_as_obj
             cx
             ~use_op
             reason
             statics
             reason_inst
             (NameUtils.Map.filter
                (fun x _ -> x <> OrdinaryName "constructor")
                (Context.find_props cx own_props)
             )
          )
      then
        rec_flow_t cx trace ~use_op (statics, u)
    (***************************************************)
    (* classes and arrays can implement some interface *)
    (***************************************************)
    | ( DefT (_, (ClassT _ | ArrT _)),
        (DefT (_, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ }) as i)
      ) ->
      rec_flow cx trace (i, ImplementsT (use_op, l))
    | ( DefT (reason, (BoolGeneralT | SingletonBoolT _)),
        DefT (interface_reason, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ })
      ) ->
      add_output
        cx
        (Error_message.EPrimitiveAsInterface { use_op; reason; interface_reason; kind = `Boolean })
    | ( DefT (reason, (NumGeneralT _ | SingletonNumT _)),
        DefT (interface_reason, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ })
      ) ->
      add_output
        cx
        (Error_message.EPrimitiveAsInterface { use_op; reason; interface_reason; kind = `Number })
    | ( DefT (reason, (StrGeneralT _ | SingletonStrT _)),
        DefT (interface_reason, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ })
      ) ->
      add_output
        cx
        (Error_message.EPrimitiveAsInterface { use_op; reason; interface_reason; kind = `String })
    (************************************)
    (* opaque types lower & upper bound *)
    (************************************)
    (* When both bounds are available, we need to do a speculative check, since only one of them
     * needs to pass. *)
    | ( OpaqueT (opaque_l_reason, { upper_t = Some lower_upper; _ }),
        OpaqueT (opaque_u_reason, { lower_t = Some upper_lower; _ })
      ) ->
      SpeculationKit.try_custom
        cx
        ~use_op
        ~no_match_error_loc:(loc_of_reason opaque_l_reason)
        [
          (fun () ->
            rec_flow_t
              cx
              trace
              ~use_op:(Frame (OpaqueTypeLowerBound { opaque_t_reason = opaque_u_reason }, use_op))
              (l, upper_lower));
          (fun () ->
            rec_flow_t
              cx
              trace
              ~use_op:(Frame (OpaqueTypeUpperBound { opaque_t_reason = opaque_l_reason }, use_op))
              (lower_upper, u));
        ]
    (* Opaque types may be treated as their upper bound when they are a lower bound for a use *)
    | (OpaqueT (opaque_t_reason, { upper_t = Some t; _ }), _) ->
      rec_flow_t cx trace ~use_op:(Frame (OpaqueTypeUpperBound { opaque_t_reason }, use_op)) (t, u)
      (* Similar to the case of OpaqueT { upper_t=Some _ }  ~> OpaqueT { lower_t=Some _ }
         We need to do the same for GenericT. *)
    | ( GenericT { reason; bound = lower_upper; _ },
        OpaqueT (opaque_u_reason, { lower_t = Some upper_lower; _ })
      ) ->
      SpeculationKit.try_custom
        cx
        ~use_op
        ~no_match_error_loc:(loc_of_reason reason)
        [
          (fun () ->
            rec_flow_t
              cx
              trace
              ~use_op:(Frame (OpaqueTypeLowerBound { opaque_t_reason = opaque_u_reason }, use_op))
              (l, upper_lower));
          (fun () -> rec_flow_t cx trace ~use_op (reposition_reason cx reason lower_upper, u));
        ]
    (* Opaque types may be treated as their lower bound when they are a upper bound for a use *)
    | (_, OpaqueT (opaque_t_reason, { lower_t = Some t; _ })) ->
      rec_flow_t cx trace ~use_op:(Frame (OpaqueTypeLowerBound { opaque_t_reason }, use_op)) (l, t)
    (*********************)
    (* functions statics *)
    (*********************)
    | (DefT (reason, FunT (static, _)), AnyT _) ->
      rec_flow cx trace (static, ReposLowerT { reason; use_desc = false; use_t = UseT (use_op, u) })
    (*****************)
    (* class statics *)
    (*****************)
    | (DefT (reason, ClassT instance), (DefT (_, ObjT _) | AnyT _)) ->
      let statics = (reason, Tvar.mk_no_wrap cx reason) in
      rec_flow cx trace (instance, GetStaticsT statics);
      rec_flow_t cx trace ~use_op (OpenT statics, u)
    (************************)
    (* classes as functions *)
    (************************)

    (*
     * When a class value flows to a function annotation or call site, check for
     * the presence of a call property in the former (as a static) compatible
     * with the latter.
     *
     * TODO: Call properties are excluded from the subclass compatibility
     * checks, which makes it unsafe to call a Class<T> type like this.
     * For example:
     *
     *   declare class A { static (): string };
     *   declare class B extends A { static (): number }
     *   var klass: Class<A> = B;
     *   var foo: string = klass(); // passes, but `foo` is a number
     *
     * The same issue is also true for constructors, which are similarly
     * excluded from subclass compatibility checks, but are allowed on ClassT
     * types.
     *)
    | (DefT (reason, ClassT instance), DefT (_, FunT _)) ->
      let statics = (reason, Tvar.mk_no_wrap cx reason) in
      rec_flow cx trace (instance, GetStaticsT statics);
      rec_flow_t cx trace ~use_op (OpenT statics, u)
    (*********)
    (* Enums *)
    (*********)
    | ( DefT (_, EnumObjectT { enum_info = ConcreteEnum { enum_id = id1; _ }; _ }),
        DefT (_, EnumObjectT { enum_info = ConcreteEnum { enum_id = id2; _ }; _ })
      )
      when ALoc.equal_id id1 id2 ->
      ()
    | ( DefT (_, EnumValueT (ConcreteEnum { enum_id = id1; _ })),
        DefT (_, EnumValueT (ConcreteEnum { enum_id = id2; _ }))
      )
      when ALoc.equal_id id1 id2 ->
      ()
    | ( DefT
          ( enum_reason_l,
            EnumObjectT
              {
                enum_info =
                  ConcreteEnum
                    {
                      enum_id = id1;
                      enum_name = n1;
                      members = m1;
                      representation_t = r1;
                      has_unknown_members = has_unknown1;
                    };
                _;
              }
          ),
        DefT
          ( enum_reason_u,
            EnumObjectT
              {
                enum_info =
                  ConcreteEnum
                    {
                      enum_id = id2;
                      enum_name = n2;
                      members = m2;
                      representation_t = r2;
                      has_unknown_members = has_unknown2;
                    };
                _;
              }
          )
      )
    | ( DefT
          ( enum_reason_l,
            EnumValueT
              (ConcreteEnum
                {
                  enum_id = id1;
                  enum_name = n1;
                  members = m1;
                  representation_t = r1;
                  has_unknown_members = has_unknown1;
                  _;
                }
                )
          ),
        DefT
          ( enum_reason_u,
            EnumValueT
              (ConcreteEnum
                {
                  enum_id = id2;
                  enum_name = n2;
                  members = m2;
                  representation_t = r2;
                  has_unknown_members = has_unknown2;
                }
                )
          )
      )
      when TypeUtil.nominal_id_have_same_logical_module
             ~file_options:(Context.file_options cx)
             ~projects_options:(Context.projects_options cx)
             (id1, Some n1)
             (id2, Some n2)
           && SSet.equal (SSet.of_list @@ SMap.keys m1) (SSet.of_list @@ SMap.keys m2)
           && has_unknown1 = has_unknown2 ->
      if TypeUtil.is_in_common_interface_conformance_check use_op then
        let use_op =
          Frame
            ( EnumRepresentationTypeCompatibility { lower = enum_reason_l; upper = enum_reason_u },
              use_op
            )
        in
        rec_flow_t cx trace ~use_op (r1, r2)
    | ( DefT (_, EnumObjectT { enum_value_t = enum_value_t1; _ }),
        DefT (_, EnumObjectT { enum_value_t = enum_value_t2; _ })
      ) ->
      rec_flow_t cx trace ~use_op (enum_value_t1, enum_value_t2)
    | ( DefT
          ( enum_reason_l,
            EnumValueT
              ( ConcreteEnum { representation_t = representation_t_l; _ }
              | AbstractEnum { representation_t = representation_t_l } )
          ),
        DefT (enum_reason_u, EnumValueT (AbstractEnum { representation_t = representation_t_u }))
      ) ->
      let use_op =
        Frame
          ( EnumRepresentationTypeCompatibility { lower = enum_reason_l; upper = enum_reason_u },
            use_op
          )
      in
      rec_flow_t cx trace ~use_op (representation_t_l, representation_t_u)
    | ( DefT
          ( enum_reason,
            EnumValueT
              ((ConcreteEnum { representation_t; _ } | AbstractEnum { representation_t }) as enum)
          ),
        t
      )
      when TypeUtil.quick_subtype representation_t t ->
      let enum_kind =
        match enum with
        | ConcreteEnum _ -> Error_message.ConcreteEnumKind
        | AbstractEnum _ -> Error_message.AbstractEnumKind
      in
      let representation_type =
        match representation_t with
        | DefT (_, BoolGeneralT)
        | DefT (_, SingletonBoolT _) ->
          Some "boolean"
        | DefT (_, NumGeneralT _)
        | DefT (_, SingletonNumT _) ->
          Some "number"
        | DefT (_, StrGeneralT _)
        | DefT (_, SingletonStrT _) ->
          Some "string"
        | DefT (_, SymbolT) -> Some "symbol"
        | DefT (_, BigIntGeneralT _)
        | DefT (_, SingletonBigIntT _) ->
          Some "bigint"
        | _ -> None
      in
      let casting_syntax = Context.casting_syntax cx in
      add_output
        cx
        (Error_message.EEnumIncompatible
           {
             reason_lower = enum_reason;
             reason_upper = reason_of_t t;
             use_op;
             enum_kind;
             representation_type;
             casting_syntax;
           }
        )
    | ( GenericT ({ bound = bound1; id = id1; reason = reason1; _ } as g1),
        GenericT ({ bound = bound2; id = id2; reason = reason2; _ } as g2)
      ) -> begin
      match Generic.satisfies ~printer:(print_if_verbose_lazy cx ~trace) id1 id2 with
      | Generic.Satisfied ->
        rec_flow_t
          cx
          trace
          ~use_op
          (reposition_reason cx reason1 bound1, reposition_reason cx reason2 bound2)
      | Generic.Lower id ->
        rec_flow_t cx trace ~use_op (GenericT { g1 with id }, reposition_reason cx reason2 bound2)
      | Generic.Upper id ->
        rec_flow_t cx trace ~use_op (reposition_reason cx reason1 bound1, GenericT { g2 with id })
    end
    | (GenericT { reason; bound; _ }, _) ->
      rec_flow_t cx trace ~use_op (reposition_reason cx reason bound, u)
    | (_, GenericT { reason; name; _ }) ->
      let desc = RIncompatibleInstantiation name in
      let bot = DefT (replace_desc_reason desc reason, EmptyT) in
      rec_flow_t cx trace ~use_op (l, bot)
    | (ObjProtoT reason, _) ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx reason ~use_desc "Object" in
      rec_flow_t cx trace ~use_op (obj_proto, u)
    | (_, ObjProtoT reason) ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx reason ~use_desc "Object" in
      rec_flow_t cx trace ~use_op (l, obj_proto)
    | (FunProtoT reason, _) ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx reason ~use_desc "Function" in
      rec_flow_t cx trace ~use_op (fun_proto, u)
    | (_, FunProtoT reason) ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx reason ~use_desc "Function" in
      rec_flow_t cx trace ~use_op (l, fun_proto)
    | (DefT (lreason, MixedT Mixed_function), DefT (ureason, FunT _)) ->
      add_output
        cx
        (Error_message.EIncompatible
           {
             lower = (lreason, None);
             upper = (ureason, Error_message.IncompatibleMixedCallT);
             use_op = Some use_op;
           }
        );
      rec_flow_t cx trace ~use_op (AnyT.make (AnyError None) lreason, u)
    | (FunProtoBindT reason, _) -> rec_flow_t cx trace ~use_op (FunProtoT reason, u)
    | (OpaqueT (reason, { opaque_id = Opaque.InternalEnforceUnionOptimized; _ }), _) ->
      add_output
        cx
        (Error_message.EUnionOptimizationOnNonUnion
           { loc = loc_of_reason reason; arg = reason_of_t u }
        )
    | (_, _) ->
      let reason_lower = generalized_reason_of_t ~compared_with_t:u l in
      let reason_upper = generalized_reason_of_t ~compared_with_t:l u in
      add_output
        cx
        (Error_message.EIncompatibleWithUseOp
           { reason_lower; reason_upper; use_op; explanation = None }
        )

  let rec_flow_p cx ?trace ~use_op ?(report_polarity = true) lreason ureason propref p =
    let errs = rec_flow_p cx ?trace ~use_op ~report_polarity propref p in
    add_output_prop_polarity_mismatch cx use_op (lreason, ureason) errs
end
