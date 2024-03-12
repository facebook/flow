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
  val rec_sub_t : Context.t -> Type.use_op -> Type.t -> Type.t -> Type.trace -> unit

  val rec_flow_p :
    Context.t ->
    ?trace:Type.trace ->
    use_op:use_op ->
    ?report_polarity:bool ->
    reason ->
    reason ->
    Type.propref ->
    Type.property_type * Type.property_type ->
    unit
end

module Make (Flow : INPUT) : OUTPUT = struct
  open Flow
  module SpeculationKit = Speculation_kit.Make (Flow)
  module RendersKit = Renders_kit.Make (Flow)

  let flow_all_in_union cx trace rep u =
    iter_union ~f:rec_flow ~init:() ~join:(fun _ _ -> ()) cx trace rep u

  let rec_flow_p cx ?trace ~use_op ?(report_polarity = true) lreason ureason propref = function
    (* unification cases *)
    | ( OrdinaryField { type_ = lt; polarity = Polarity.Neutral },
        OrdinaryField { type_ = ut; polarity = Polarity.Neutral }
      ) ->
      unify_opt cx ?trace ~use_op lt ut
    (* directional cases *)
    | (lp, up) ->
      let propref_error = name_of_propref propref in
      (match (Property.read_t_of_property_type lp, Property.read_t_of_property_type up) with
      | (Some lt, Some ut) -> flow_opt cx ?trace (lt, UseT (use_op, ut))
      | (None, Some _) when report_polarity ->
        add_output
          cx
          ?trace
          (Error_message.EPropPolarityMismatch
             ( (lreason, ureason),
               propref_error,
               (Property.polarity_of_property_type lp, Property.polarity_of_property_type up),
               use_op
             )
          )
      | _ -> ());
      (match (Property.write_t_of_property_type lp, Property.write_t_of_property_type up) with
      | (Some lt, Some ut) -> flow_opt cx ?trace (ut, UseT (use_op, lt))
      | (None, Some _) when report_polarity ->
        add_output
          cx
          ?trace
          (Error_message.EPropPolarityMismatch
             ( (lreason, ureason),
               propref_error,
               (Property.polarity_of_property_type lp, Property.polarity_of_property_type up),
               use_op
             )
          )
      | _ -> ())

  let func_predicate_compat =
    let rec subst_map (n, map) = function
      | ((Some k, _) :: ps1, (Some v, _) :: ps2) ->
        let map' =
          if k <> v then
            SMap.add k (OrdinaryName v, []) map
          else
            (* Skip trivial entry *)
            map
        in
        subst_map (n + 1, map') (ps1, ps2)
      | (_, []) -> Ok (map : Type.substitution)
      | ([], ps2) ->
        (* Flag an error if predicate counts do not coincide
           TODO: somehow the original flow needs to be propagated as well *)
        let n2 = n + List.length ps2 in
        Error (`ArityMismatch (n, n2))
      | ((None, _) :: _, _)
      | (_, (None, _) :: _) ->
        Error `NoParamNames
    in
    fun cx trace use_op (lreason, params1, pred1) (ureason, params2, pred2) ->
      match subst_map (0, SMap.empty) (params1, params2) with
      | Error (`ArityMismatch (n1, n2)) ->
        add_output
          cx
          ~trace
          (Error_message.EPredicateFuncArityMismatch
             { use_op; reasons = (lreason, ureason); arities = (n1, n2) }
          )
      | Error `NoParamNames ->
        (* Already an unsupported-syntax error on the definition side of the function. *)
        ()
      | Ok map ->
        let (lreason, (lazy (pmap1, _nmap1))) = pred1 in
        let (ureason, (lazy (pmap2, nmap2))) = pred2 in
        if SMap.is_empty map then (
          if not (TypeUtil.pred_map_implies pmap1 pmap2) then
            add_output
              cx
              ~trace
              (Error_message.EIncompatibleWithUseOp
                 { reason_lower = lreason; reason_upper = ureason; use_op }
              )
        ) else if Key_map.(is_empty pmap2 && is_empty nmap2) then
          ()
        else
          add_output
            cx
            ~trace
            (Error_message.EIncompatibleWithUseOp
               { reason_lower = lreason; reason_upper = ureason; use_op }
            )

  let index_of_param params x =
    Base.List.find_mapi params ~f:(fun i p ->
        match p with
        | (Some x', _) when x = x' -> Some i
        | _ -> None
    )

  let func_type_guard_compat cx trace use_op grd1 grd2 =
    let (params1, (loc1, x1), t1) = grd1 in
    let (params2, (loc2, x2), t2) = grd2 in
    let idx1 = index_of_param params1 x1 in
    let idx2 = index_of_param params2 x2 in
    let use_op = Frame (TypePredicateCompatibility, use_op) in
    let lower = Reason.mk_reason (RTypeGuardParam x1) loc1 in
    let upper = Reason.mk_reason (RTypeGuardParam x2) loc2 in
    if idx1 <> idx2 then
      add_output
        cx
        ~trace
        (Error_message.ETypeGuardIndexMismatch { use_op; reasons = (lower, upper) });

    rec_flow_t cx trace ~use_op (t1, t2)

  let flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj) =
    let {
      flags = lflags;
      call_t = lcall;
      props_tmap = lflds;
      proto_t = lproto;
      reachable_targs = _;
    } =
      l_obj
    in
    let {
      flags = rflags;
      call_t = ucall;
      props_tmap = uflds;
      proto_t = uproto;
      reachable_targs = _;
    } =
      u_obj
    in
    (* if inflowing type is literal (thus guaranteed to be
       unaliased), propertywise subtyping is sound *)
    let lit = is_literal_object_reason lreason || lflags.frozen in
    (* If both are dictionaries, ensure the keys and values are compatible
       with each other. *)
    let ldict = Obj_type.get_dict_opt lflags.obj_kind in
    let udict = Obj_type.get_dict_opt rflags.obj_kind in
    (match (ldict, udict) with
    | ( Some { key = lk; value = lv; dict_polarity = lpolarity; _ },
        Some { key = uk; value = uv; dict_polarity = upolarity; _ }
      ) ->
      (* Don't report polarity errors when checking the indexer key. We would
       * report these errors again a second time when checking values. *)
      rec_flow_p
        cx
        ~trace
        ~report_polarity:false
        ~use_op:(Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op))
        lreason
        ureason
        (Computed uk)
        ( OrdinaryField { type_ = lk; polarity = lpolarity },
          OrdinaryField { type_ = uk; polarity = upolarity }
        );
      rec_flow_p
        cx
        ~trace
        ~use_op:
          (Frame (PropertyCompatibility { prop = None; lower = lreason; upper = ureason }, use_op))
        lreason
        ureason
        (Computed uv)
        ( OrdinaryField { type_ = lv; polarity = lpolarity },
          OrdinaryField { type_ = uv; polarity = upolarity }
        )
    | _ -> ());

    if rflags.obj_kind = Exact && not (is_literal_object_reason ureason) then (
      Context.iter_real_props cx lflds (fun name _ ->
          if not (Context.has_prop cx uflds name) then
            let use_op =
              Frame
                ( PropertyCompatibility
                    {
                      prop = Some name;
                      (* Lower and upper are reversed in this case since the lower object
                       * is the one requiring the prop. *)
                      lower = ureason;
                      upper = lreason;
                    },
                  use_op
                )
            in
            let reason_prop = replace_desc_reason (RProperty (Some name)) lreason in
            let err =
              Error_message.EPropNotFound
                {
                  prop_name = Some name;
                  reason_prop;
                  reason_obj = ureason;
                  use_op;
                  suggestion = None;
                }
            in
            add_output cx ~trace err
      );
      Base.Option.iter lcall ~f:(fun _ ->
          if Base.Option.is_none ucall then
            let prop = Some (OrdinaryName "$call") in
            let use_op =
              Frame
                ( PropertyCompatibility
                    {
                      prop;
                      (* Lower and upper are reversed in this case since the lower object
                       * is the one requiring the prop. *)
                      lower = ureason;
                      upper = lreason;
                    },
                  use_op
                )
            in
            let reason_prop = replace_desc_reason (RProperty prop) lreason in
            let err =
              Error_message.EPropNotFound
                { prop_name = prop; reason_prop; reason_obj = ureason; use_op; suggestion = None }
            in
            add_output cx ~trace err
      )
    );

    (match ucall with
    | Some ucall ->
      let prop_name = Some (OrdinaryName "$call") in
      let use_op =
        Frame (PropertyCompatibility { prop = prop_name; lower = lreason; upper = ureason }, use_op)
      in
      (match lcall with
      | Some lcall ->
        rec_flow cx trace (Context.find_call cx lcall, UseT (use_op, Context.find_call cx ucall))
      | None ->
        let reason_prop = replace_desc_reason (RProperty prop_name) ureason in
        let error_message =
          Error_message.EPropNotFound
            { reason_prop; reason_obj = lreason; prop_name; use_op; suggestion = None }
        in
        add_output cx ~trace error_message)
    | None -> ());

    (* Properties in u must either exist in l, or match l's indexer. *)
    Context.iter_real_props cx uflds (fun name up ->
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
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ()
          else
            (* prop from aliased LB *)
            rec_flow_p
              cx
              ~trace
              ~use_op
              lreason
              ureason
              propref
              (Property.type_ lp, Property.type_ up)
        | (None, Some { key; value; dict_polarity; _ }) when not (is_dictionary_exempt name) ->
          rec_flow
            cx
            trace
            ( type_of_key_name cx name reason_prop,
              UseT
                (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op'), key)
            );
          let lp = OrdinaryField { type_ = value; polarity = dict_polarity } in
          let up =
            match up with
            | Field
                {
                  preferred_def_locs = _;
                  key_loc = _;
                  type_ = OptionalT { reason = _; type_ = ut; use_desc = _ };
                  polarity;
                } ->
              OrdinaryField { type_ = ut; polarity }
            | _ -> Property.type_ up
          in
          if lit then
            match (Property.read_t_of_property_type lp, Property.read_t_of_property_type up) with
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ()
          else
            rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
        | _ ->
          (* property doesn't exist in inflowing type *)
          (match up with
          | Field { type_ = OptionalT _; _ } when lit -> ()
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
                      LookupProp (use_op, OrdinaryField { type_; polarity = Polarity.Positive });
                    method_accessible = true;
                    ids = None;
                    ignore_dicts = false;
                  }
              )
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
                    lookup_action = LookupProp (use_op, Property.type_ up);
                    method_accessible = true;
                    ids = None;
                    ignore_dicts = false;
                  }
              ))
    );

    (* Any properties in l but not u must match indexer *)
    (match udict with
    | None -> ()
    | Some { key; value; dict_polarity; _ } ->
      let keys =
        Context.fold_real_props
          cx
          lflds
          (fun name lp keys ->
            if Context.has_prop cx uflds name then
              keys
            else
              let use_op =
                Frame
                  ( PropertyCompatibility { prop = Some name; lower = lreason; upper = ureason },
                    use_op
                  )
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
                  OrdinaryField { type_ = lt; polarity }
                | _ -> Property.type_ lp
              in
              let up = OrdinaryField { type_ = value; polarity = dict_polarity } in
              begin
                if lit then
                  match
                    (Property.read_t_of_property_type lp, Property.read_t_of_property_type up)
                  with
                  | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
                  | _ -> ()
                else
                  let propref =
                    mk_named_prop ~reason:(replace_desc_reason (RProperty (Some name)) lreason) name
                  in
                  rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
              end;
              type_of_key_name cx name lreason :: keys)
          []
        |> union_of_ts lreason
      in
      rec_flow
        cx
        trace
        ( keys,
          UseT (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op), key)
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
          rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
      | _ -> ()));

    rec_flow cx trace (uproto, ReposUseT (ureason, false, use_op, DefT (lreason, ObjT l_obj)))

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
      rec_flow cx trace (e1, UseT (use_op, e2))
    (* non-empty array literal / tuple ~> non-empty array literal / tuple *)
    | (t1 :: ts1, e1, t2 :: ts2, e2) ->
      (* specific element1 = specific element2 *)
      flow_to_mutable_child cx trace use_op lit1 t1 t2;
      array_flow cx trace use_op lit1 r1 ~index:(index + 1) (ts1, e1, ts2, e2)

  let rec_sub_t cx use_op l u trace =
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
    | (DefT (_, (NumT _ | SingletonNumT _)), DefT (_, NumT _)) -> ()
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
      if TypeAppExpansion.push_unless_loop cx (c1, ts1) then (
        if TypeAppExpansion.push_unless_loop cx (c2, ts2) then (
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
      if TypeAppExpansion.push_unless_loop cx (type_, targs) then (
        let reason_op = reason_of_t u in
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
      if TypeAppExpansion.push_unless_loop cx (type_, targs) then (
        let reason_op = reason_of_t l in
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
      when ALoc.equal_id id1 id2 ->
      flow_type_args cx trace ~use_op lreason ureason ltargs utargs
    (* If the opaque type are from the same logical module, we need to do some structural validation
       in additional to type_args check. *)
    | ( OpaqueT
          ( lreason,
            { opaque_id = id1; opaque_name = name1; opaque_type_args = ltargs; super_t = super1; _ }
          ),
        OpaqueT
          ( ureason,
            { opaque_id = id2; opaque_name = name2; opaque_type_args = utargs; super_t = super2; _ }
          )
      )
      when TypeUtil.nominal_id_have_same_logical_module
             ~file_options:Context.((metadata cx).file_options)
             (id1, Some name1)
             (id2, Some name2)
           && List.length ltargs = List.length utargs ->
      (* Check super *)
      begin
        let super1 = Base.Option.value super1 ~default:(MixedT.make lreason) in
        let super2 = Base.Option.value super2 ~default:(MixedT.make ureason) in
        let use_op =
          Frame
            ( OpaqueTypeSuperCompatibility { lower = reason_of_t super1; upper = reason_of_t super2 },
              use_op
            )
        in
        rec_unify cx trace ~use_op super1 super2
      end;
      (* Do not check underlying type even if we have access to them, because underlying types
       * are not visible across module boundaries. *)
      (* Check targs *)
      flow_type_args cx trace ~use_op lreason ureason ltargs utargs
    (* If the type is still in the same file it was defined, we allow it to
     * expose its underlying type information *)
    | (OpaqueT (r, { underlying_t = Some t; _ }), _)
      when ALoc.source (loc_of_reason r) = ALoc.source (def_loc_of_reason r) ->
      rec_flow_t cx trace ~use_op (t, u)
    (* If the lower bound is in the same file as where the opaque type was defined,
     * we expose the underlying type information *)
    | (_, OpaqueT (r, { underlying_t = Some t; _ }))
      when ALoc.source (loc_of_reason (reason_of_t l)) = ALoc.source (def_loc_of_reason r) ->
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
    | (DefT (_, NumericStrKeyT _), DefT (_, (NumT _ | StrT _))) -> ()
    | (DefT (rl, NumericStrKeyT (actual, _)), DefT (ru, SingletonNumT (expected, _))) ->
      if actual = expected then
        ()
      else
        add_output
          cx
          ~trace
          (Error_message.EExpectedNumberLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, NumericStrKeyT (_, actual)), DefT (ru, SingletonStrT expected)) ->
      if OrdinaryName actual = expected then
        ()
      else
        add_output
          cx
          ~trace
          (Error_message.EExpectedStringLit { reason_lower = rl; reason_upper = ru; use_op })
    | (_, DefT (r, NumericStrKeyT (_, s))) ->
      let u = DefT (r, StrT (Literal (None, OrdinaryName s))) in
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
     * always replaced by StrT(_, Some key). The reason is that types of the
     * latter form (string literal types) are inferred to be the type of string
     * literals appearing as values, and we don't want to prematurely narrow
     * down the type of the location where such values may appear, since that
     * would preclude other strings to be stored in that location. Thus, by
     * necessity we allow all string types to flow to StrT (whereas only
     * exactly matching string literal types may flow to SingletonStrT).
     * *)
    | (DefT (rl, StrT actual), DefT (ru, SingletonStrT expected)) ->
      if TypeUtil.literal_eq expected actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          ~trace
          (Error_message.EExpectedStringLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, NumT actual), DefT (ru, SingletonNumT expected)) ->
      if TypeUtil.number_literal_eq expected actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          ~trace
          (Error_message.EExpectedNumberLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, BoolT actual), DefT (ru, SingletonBoolT expected)) ->
      if TypeUtil.boolean_literal_eq expected actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          ~trace
          (Error_message.EExpectedBooleanLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, BigIntT actual), DefT (ru, SingletonBigIntT expected)) ->
      if TypeUtil.bigint_literal_eq expected actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          ~trace
          (Error_message.EExpectedBigIntLit { reason_lower = rl; reason_upper = ru; use_op })
    (*****************************************************)
    (* keys (NOTE: currently we only support string keys *)
    (*****************************************************)
    | ( ( DefT (reason_s, StrT literal)
        | GenericT { reason = reason_s; bound = DefT (_, StrT literal); _ } ),
        KeysT (reason_op, o)
      ) ->
      let reason_next =
        match literal with
        | Literal (_, x) -> replace_desc_new_reason (RProperty (Some x)) reason_s
        | _ -> replace_desc_new_reason RUnknownString reason_s
      in
      (* check that o has key x *)
      let u = HasOwnPropT (use_op, reason_next, l) in
      rec_flow cx trace (o, ReposLowerT (reason_op, false, u))
    | ( ( DefT (reason_s, NumericStrKeyT (_, s))
        | GenericT { reason = reason_s; bound = DefT (_, NumericStrKeyT (_, s)); _ } ),
        KeysT (reason_op, o)
      ) ->
      let reason_next = replace_desc_new_reason (RProperty (Some (OrdinaryName s))) reason_s in
      let l = DefT (reason_s, StrT (Literal (None, OrdinaryName s))) in
      let u = HasOwnPropT (use_op, reason_next, l) in
      rec_flow cx trace (o, ReposLowerT (reason_op, false, u))
    | (KeysT (reason1, o1), _) ->
      (* flow all keys of o1 to u *)
      rec_flow cx trace (o1, GetKeysT (reason1, UseT (use_op, u)))
    (*********************************************)
    (* Using predicate functions as regular ones *)
    (*********************************************)
    | (UnionT (reason, rep), _) when UnionRep.members rep |> List.exists is_union_resolvable ->
      iter_resolve_union ~f:rec_flow cx trace reason rep (UseT (use_op, u))
    (* cases where there is no loss of precision *)
    | (UnionT _, UnionT _) when union_optimization_guard cx ~equiv:false TypeUtil.quick_subtype l u
      ->
      if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path"
    | (OpaqueT (_, { super_t = Some (UnionT _ as l); _ }), UnionT _)
      when union_optimization_guard cx ~equiv:false TypeUtil.quick_subtype l u ->
      if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path (via an opaque type)"
    (* Optimization to treat maybe and optional types as special unions for subset comparision *)
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

      flow_all_in_union cx trace rep (UseT (use_op, u))
    | (_, IntersectionT (_, rep)) ->
      ( if Context.is_verbose cx then
        match l with
        | UnionT _ -> prerr_endline "IntersectionT ~> UnionT slow case"
        | _ -> ()
      );
      InterRep.members rep |> List.iter (fun t -> rec_flow cx trace (l, UseT (use_op, t)))
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
    (* String enum sets can be handled in logarithmic time by just
     * checking for membership in the set.
     *)
    | (DefT (reason_l, StrT (Literal (_, x))), UnionT (reason_u, rep))
      when match UnionRep.check_enum rep with
           | Some enums ->
             if not (UnionEnumSet.mem (UnionEnum.Str x) enums) then
               add_output
                 cx
                 ~trace
                 (Error_message.EIncompatibleWithUseOp
                    {
                      reason_lower = reason_l;
                      reason_upper =
                        UnionRep.specialized_reason ~reason_of_t:TypeUtil.reason_of_t reason_u rep;
                      use_op;
                    }
                 );
             true
           | _ -> false ->
      ()
    | (_, UnionT (_, rep))
      when let ts = Type_mapper.union_flatten cx @@ UnionRep.members rep in
           List.exists (TypeUtil.quick_subtype l) ts ->
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
      let node = get_builtin_type cx ~use_desc:true renders_r "React$Node" in
      if union_contains_instantiable_tvars || not (speculative_subtyping_succeeds cx node u) then
        SpeculationKit.try_union cx trace use_op l r rep
    | (_, UnionT (r, rep)) ->
      (* Try the branches of the union in turn, with the goal of selecting the
       * correct branch. This process is reused for intersections as well. See
       * comments on try_union and try_intersection. *)
      SpeculationKit.try_union cx trace use_op l r rep
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
     * Note: should be able to do this with LookupT rather than
     * slices, but that approach behaves in nonobvious ways. TODO why?
     *)
    | (IntersectionT _, DefT (r, ObjT { flags; props_tmap; proto_t; call_t; reachable_targs = _ }))
      when NameUtils.Map.cardinal (Context.find_props cx props_tmap) > 1 ->
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
      let unresolved = parts_to_replace_t cx u in
      let u' = UseT (use_op, u) in
      SpeculationKit.prep_try_intersection cx trace (reason_of_t u) unresolved [] u' r rep
    (************)
    (* matching *)
    (************)
    | (MatchingPropT _, GenericT { bound; _ }) -> rec_flow_t cx trace ~use_op (l, bound)
    | (MatchingPropT (reason, name, t), l) ->
      (* Things that can have properties are object-like (objects, instances,
       * and their exact versions). Notably, "meta" types like union, annot,
       * typeapp, eval, maybe, optional, and intersection should have boiled
       * away by this point. Generics should have been "unsealed" as well. *)
      let propref = mk_named_prop ~reason (OrdinaryName name) in
      let lookup_kind = NonstrictReturning (None, None) in
      let drop_generic = true in
      let u =
        LookupT
          {
            reason;
            lookup_kind;
            try_ts_on_failure = [];
            propref;
            lookup_action = MatchProp { use_op; drop_generic; prop_t = t };
            method_accessible = true;
            ids = Some Properties.Set.empty;
            ignore_dicts = false;
          }
      in
      rec_flow cx trace (l, u)
    | (DefT (reason, SingletonStrT key), _) ->
      rec_flow_t cx trace ~use_op (DefT (reason, StrT (Literal (None, key))), u)
    | (DefT (reason, SingletonNumT lit), _) ->
      rec_flow_t cx trace ~use_op (DefT (reason, NumT (Literal (None, lit))), u)
    | (DefT (reason, SingletonBoolT b), _) ->
      rec_flow_t cx trace ~use_op (DefT (reason, BoolT (Some b)), u)
    | (DefT (reason, SingletonBigIntT lit), _) ->
      rec_flow_t cx trace ~use_op (DefT (reason, BigIntT (Literal (None, lit))), u)
    | (NullProtoT reason, _) -> rec_flow_t cx trace ~use_op (DefT (reason, NullT), u)
    (************************************************************************)
    (* exact object types *)
    (************************************************************************)

    (* ExactT<X> comes from annotation, may behave as LB or UB *)

    (* when $Exact<LB> ~> UB, forward to MakeExactT *)
    | (ExactT (r, t), _) ->
      let t = push_type_alias_reason r t in
      rec_flow cx trace (t, MakeExactT (r, Upper (UseT (use_op, u))))
    (* ObjT LB ~> $Exact<UB>. make exact if exact and unsealed *)
    | (DefT (_, ObjT { flags; _ }), ExactT (r, t)) ->
      if Obj_type.is_exact flags.obj_kind then
        let t = push_type_alias_reason r t in
        rec_flow cx trace (t, MakeExactT (r, Lower (use_op, l)))
      else (
        exact_obj_error cx trace flags.obj_kind ~use_op ~exact_reason:r l;
        (* Continue the Flow even after we've errored. Often, there is more that
         * is different then just the fact that the upper bound is exact and the
         * lower bound is not. This could easily hide errors in ObjT ~> ExactT *)
        rec_flow_t cx trace ~use_op (l, t)
      )
    (* any ~> $Exact<UB>. unwrap exact *)
    | ((DefT (_, EmptyT) | AnyT _), ExactT (_, t)) -> rec_flow_t cx trace ~use_op (l, t)
    (* anything else ~> $Exact<UB>. error *)
    | (_, ExactT (r, t)) -> rec_flow cx trace (t, MakeExactT (r, Lower (use_op, l)))
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
      (* check instancel <: instanceu *)
      rec_flow_t cx trace ~use_op (this, instance);

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
      rec_flow cx trace (l, ReactKitT (use_op, reasonl, React.ConfigCheck config));

      (* Ensure this is a function component *)
      rec_flow_t ~use_op cx trace (return_t, get_builtin_type cx reasonl "React$Node");

      (* check rendered elements are covariant *)
      rec_flow_t cx trace ~use_op (return_t, renders);

      (* A function component instance type is always void, so flow void to instance *)
      rec_flow_t cx trace ~use_op (VoidT.make (replace_desc_new_reason RVoid reasonl), instance)
    (* Object Component ~> AbstractComponent *)
    | ( DefT (reasonl, ObjT { call_t = Some id; _ }),
        DefT
          ( reasonu,
            ReactAbstractComponentT { config; instance; renders; component_kind = Structural }
          )
      ) ->
      rec_flow cx trace (l, ReactKitT (use_op, reasonl, React.ConfigCheck config));

      (* Ensure the callable signature's return type is compatible with the rendered element (renders). We
       * do this by flowing it to (...empty): renders *)
      let funtype =
        mk_functiontype
          reasonu
          []
          ~rest_param:
            (Some (None, loc_of_reason reasonu, EmptyT.why (replace_desc_new_reason REmpty reasonu)))
          ~def_reason:reasonl
          ~predicate:None
          renders
      in
      let mixed = MixedT.why reasonu in
      rec_flow_t ~use_op cx trace (Context.find_call cx id, DefT (reasonu, FunT (mixed, funtype)));

      (* An object component instance type is always void, so flow void to instance *)
      rec_flow_t cx trace ~use_op (VoidT.make (replace_desc_new_reason RVoid reasonl), instance)
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
      rec_flow_t cx trace ~use_op (instancel, instanceu);
      let rendersl =
        match component_kind with
        | Nominal (renders_id, renders_name) ->
          let reason = update_desc_reason (fun desc -> RRenderType desc) reasonl in
          DefT
            ( reason,
              RendersT (NominalRenders { renders_id; renders_name; renders_super = rendersl })
            )
        | Structural -> rendersl
      in
      rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (rendersl, rendersu)
    | ( DefT
          ( _,
            ReactAbstractComponentT
              {
                config = configl;
                instance = instancel;
                renders = rendersl;
                component_kind = Nominal (idl, _name_l);
              }
          ),
        DefT
          ( _,
            ReactAbstractComponentT
              {
                config = configu;
                instance = instanceu;
                renders = rendersu;
                component_kind = Nominal (idu, _name_u);
              }
          )
      )
      when ALoc.equal_id idl idu ->
      rec_flow_t cx trace ~use_op (configu, configl);
      rec_flow_t cx trace ~use_op (instancel, instanceu);
      rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (rendersl, rendersu)
    | (DefT (reasonl, RendersT r1), DefT (reasonu, RendersT r2)) ->
      RendersKit.rec_renders cx trace ~use_op ((reasonl, r1), (reasonu, r2))
    | ( DefT (_, (NullT | VoidT | BoolT (Some false))),
        DefT
          ( _,
            RendersT
              (StructuralRenders
                { renders_variant = RendersMaybe | RendersStar; renders_structural_type = _ }
                )
          )
      ) ->
      ()
    | ( DefT (_, ArrT (ArrayAT { elem_t = t; _ } | TupleAT { elem_t = t; _ } | ROArrayAT (t, _))),
        DefT
          ( _,
            RendersT
              (StructuralRenders { renders_variant = RendersStar; renders_structural_type = _ })
          )
      ) ->
      rec_flow_t cx trace ~use_op (t, u)
    (* Try to do structural subtyping. If that fails promote to a render type *)
    | (OpaqueT (reason_opaque, _), DefT (renders_r, RendersT (NominalRenders _ as form))) ->
      rec_flow
        cx
        trace
        ( l,
          TryRenderTypePromotionT
            {
              use_op = Frame (RendersCompatibility, use_op);
              reason = renders_r;
              reason_obj = reason_opaque;
              upper_renders = form;
              tried_promotion = false;
            }
        )
    | ( OpaqueT (reason_opaque, _),
        DefT
          ( renders_r,
            RendersT (StructuralRenders { renders_variant = _; renders_structural_type = t } as form)
          )
      ) ->
      if not (speculative_subtyping_succeeds cx l t) then
        rec_flow
          cx
          trace
          ( l,
            TryRenderTypePromotionT
              {
                use_op = Frame (RendersCompatibility, use_op);
                reason = renders_r;
                reason_obj = reason_opaque;
                upper_renders = form;
                tried_promotion = false;
              }
          )
    (* given x <: y, x <: renders y. The only case in which this is not true is when `x` is a component reference,
     * Foo <: renders Foo fails in that case. Since the RHS is in its canonical form we know that we're safe
     * to Flow the LHS to the structural type on the RHS *)
    | ( l,
        DefT
          ( _renders_reason,
            RendersT (StructuralRenders { renders_variant = _; renders_structural_type = t })
          )
      ) ->
      rec_flow_t cx trace ~use_op:(Frame (RendersCompatibility, use_op)) (l, t)
    | (l, DefT (_, RendersT _)) ->
      add_output
        cx
        ~trace
        (Error_message.EIncompatibleWithUseOp
           {
             reason_lower = reason_of_t l;
             reason_upper = reason_of_t u;
             use_op = Frame (RendersCompatibility, use_op);
           }
        )
    (* Exiting the renders world *)
    | (DefT (r, RendersT (NominalRenders _)), u) ->
      let mixed_element = get_builtin_type cx r "React$MixedElement" in
      rec_flow_t cx trace ~use_op (mixed_element, u)
    | ( DefT
          ( r,
            RendersT
              (StructuralRenders { renders_variant = RendersNormal; renders_structural_type = t })
          ),
        u
      ) ->
      let u' = ExitRendersT { renders_reason = r; u = UseT (use_op, u) } in
      rec_flow cx trace (t, u')
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
          if
            (not unbound) && not (Context.allowed_method_unbinding cx (Reason.loc_of_reason lreason))
          then
            add_output
              cx
              ~trace
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
        match (ft1.hook, ft2.hook) with
        | (AnyHook, _)
        | (_, AnyHook)
        | (NonHook, NonHook)
        | ((HookDecl _ | HookAnnot), HookAnnot) ->
          ()
        | (HookDecl a, HookDecl b) when ALoc.equal_id a b -> ()
        | ((HookDecl _ | HookAnnot), NonHook) ->
          add_output
            cx
            ~trace
            (Error_message.EHookIncompatible
               {
                 use_op;
                 lower = lreason;
                 upper = ureason;
                 lower_is_hook = true;
                 hook_is_annot = ft1.hook = HookAnnot;
               }
            )
        | (NonHook, (HookDecl _ | HookAnnot)) ->
          add_output
            cx
            ~trace
            (Error_message.EHookIncompatible
               {
                 use_op;
                 lower = lreason;
                 upper = ureason;
                 lower_is_hook = false;
                 hook_is_annot = ft2.hook = HookAnnot;
               }
            )
        | ((HookDecl _ | HookAnnot), HookDecl _) ->
          add_output
            cx
            ~trace
            (Error_message.EHookUniqueIncompatible { use_op; lower = lreason; upper = ureason })
      end;

      (* Return type subtyping *)
      let ret_use_op =
        Frame
          (FunReturn { lower = reason_of_t ft1.return_t; upper = reason_of_t ft2.return_t }, use_op)
      in
      rec_flow cx trace (ft1.return_t, UseT (ret_use_op, ft2.return_t));

      begin
        match (ft1.predicate, ft2.predicate) with
        | (None, Some _)
        | (Some (PredBased _), Some (TypeGuardBased _))
        | (Some (TypeGuardBased _), Some (PredBased _)) ->
          (* Non-predicate functions are incompatible with predicate ones
             TODO: somehow the original flow needs to be propagated as well *)
          add_output
            cx
            ~trace
            (Error_message.EPredicateFuncIncompatibility { use_op; reasons = (lreason, ureason) })
        | (Some (PredBased p1), Some (PredBased p2)) ->
          func_predicate_compat cx trace use_op (lreason, ft1.params, p1) (ureason, ft2.params, p2)
        | ( Some (TypeGuardBased { param_name = x1; type_guard = t1 }),
            Some (TypeGuardBased { param_name = x2; type_guard = t2 })
          ) ->
          func_type_guard_compat cx trace use_op (ft1.params, x1, t1) (ft2.params, x2, t2)
        | (Some _, None)
        | (None, None) ->
          ()
      end
    | (DefT (reason, StrT (Literal (_, name))), DefT (reason_op, CharSetT chars)) ->
      let str = display_string_of_name name in
      let module CharSet = String_utils.CharSet in
      Error_message.(
        let (invalid, _) =
          String_utils.fold_left
            ~f:(fun (invalid, seen) chr ->
              if not (CharSet.mem chr chars) then
                (InvalidCharSetSet.add (InvalidChar chr) invalid, seen)
              else if CharSet.mem chr seen then
                (InvalidCharSetSet.add (DuplicateChar chr) invalid, seen)
              else
                (invalid, CharSet.add chr seen))
            ~acc:(InvalidCharSetSet.empty, CharSet.empty)
            str
        in
        if not (InvalidCharSetSet.is_empty invalid) then
          add_output
            cx
            ~trace
            (EInvalidCharSet
               {
                 invalid = (replace_desc_reason (RStringLit name) reason, invalid);
                 valid = reason_op;
                 use_op;
               }
            )
      )
    | (DefT (reason, CharSetT _), _) -> rec_flow_t cx trace ~use_op (StrT.why reason, u)
    | (_, DefT (reason, CharSetT _)) -> rec_flow_t cx trace ~use_op (l, StrT.why reason)
    (* Custom functions are still functions, so they have all the prototype properties *)
    | (CustomFunT (r, _), AnyT _) -> rec_flow_t cx trace ~use_op (FunProtoT r, u)
    (* unwrap namespace type into object type, drop all information about types in the namespace *)
    | (NamespaceT { values_type; types_tmap = _ }, _) -> rec_flow_t cx trace ~use_op (values_type, u)
    | (l, NamespaceT { values_type; types_tmap = _ }) -> rec_flow_t cx trace ~use_op (l, values_type)
    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    (* ObjT -> ObjT *)
    | ( DefT (lreason, ObjT ({ props_tmap = lflds; _ } as l_obj)),
        DefT (ureason, ObjT ({ props_tmap = uflds; _ } as u_obj))
      ) ->
      let u_deft = u in
      Type_inference_hooks_js.dispatch_obj_to_obj_hook cx l u_deft;
      let print_fast_path = Context.is_verbose cx in
      if Properties.equal_id lflds uflds then (
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
        ~trace
        (Error_message.EIncompatibleWithExact (reasons, use_op, Error_message.Inexact))
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
      add_output cx ~trace (Error_message.EClassToObject (lreason, ureason, use_op));
      let lflds =
        let own_props = Context.find_props cx lown in
        let proto_props = Context.find_props cx lproto in
        NameUtils.Map.union own_props proto_props
      in
      Base.Option.iter ucall ~f:(fun ucall ->
          let prop_name = Some (OrdinaryName "$call") in
          let use_op =
            Frame
              (PropertyCompatibility { prop = prop_name; lower = lreason; upper = ureason }, use_op)
          in
          match lcall with
          | Some lcall ->
            rec_flow cx trace (Context.find_call cx lcall, UseT (use_op, Context.find_call cx ucall))
          | None ->
            let reason_prop = replace_desc_reason (RProperty prop_name) ureason in
            let error_message =
              Error_message.EPropNotFound
                { reason_prop; reason_obj = lreason; prop_name; use_op; suggestion = None }
            in
            add_output cx ~trace error_message
      );

      Context.iter_real_props cx uflds (fun name up ->
          let use_op =
            Frame
              (PropertyCompatibility { prop = Some name; lower = lreason; upper = ureason }, use_op)
          in
          let propref =
            mk_named_prop ~reason:(replace_desc_reason (RProperty (Some name)) ureason) name
          in
          match NameUtils.Map.find_opt name lflds with
          | Some lp ->
            rec_flow_p
              cx
              ~trace
              ~use_op
              lreason
              ureason
              propref
              (Property.type_ lp, Property.type_ up)
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
                  ( lreason,
                    false,
                    LookupT
                      {
                        reason = ureason;
                        lookup_kind;
                        try_ts_on_failure = [];
                        propref;
                        lookup_action = LookupProp (use_op, Property.type_ up);
                        method_accessible = false;
                        ids = Some (Properties.Set.of_list [lown; lproto]);
                        ignore_dicts = false;
                      }
                  )
              )
      );

      rec_flow cx trace (l, UseT (use_op, uproto))
    (* For some object `x` and constructor `C`, if `x instanceof C`, then the
     * object is a subtype. We use `ExtendsT` to walk the proto chain of the
     * object, in case it includes a nominal type. *)
    | (DefT (_, ObjT _), DefT (_, InstanceT _)) -> rec_flow cx trace (l, extends_use_type use_op l u)
    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | (DefT (reason, (ObjT _ | InstanceT _)), DefT (reason_op, FunT _)) ->
      let prop_name = Some (OrdinaryName "$call") in
      let use_op =
        Frame (PropertyCompatibility { prop = prop_name; lower = reason; upper = reason_op }, use_op)
      in
      let fun_t =
        match l with
        | DefT (_, ObjT { call_t = Some id; _ })
        | DefT (_, InstanceT { inst = { inst_call_t = Some id; _ }; _ }) ->
          Context.find_call cx id
        | _ ->
          let reason_prop = replace_desc_reason (RProperty prop_name) reason_op in
          let error_message =
            Error_message.EPropNotFound
              { reason_prop; reason_obj = reason; prop_name; use_op; suggestion = None }
          in
          add_output cx ~trace error_message;
          AnyT.error reason_op
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
      let lit1 = desc_of_reason r1 = RArrayLit in
      let ts1 =
        Base.Option.value_map
          ~default:[]
          ~f:(fun (elements, _arity) -> tuple_ts_of_elements elements)
          tv1
      in
      let ts2 =
        Base.Option.value_map
          ~default:[]
          ~f:(fun (elements, _arity) -> tuple_ts_of_elements elements)
          tv2
      in
      array_flow cx trace use_op lit1 r1 (ts1, t1, ts2, t2)
    (* Tuples can flow to tuples with the same arity *)
    | ( DefT (r1, ArrT (TupleAT { elem_t = _; elements = elements1; arity = arity1; react_dro = _ })),
        DefT (r2, ArrT (TupleAT { elem_t = _; elements = elements2; arity = arity2; react_dro = _ }))
      ) ->
      let fresh =
        match desc_of_reason r1 with
        | RArrayLit
        | REmptyArrayLit
        | RRestArrayLit _
        | RReactChildren ->
          true
        | _ -> false
      in
      let (num_req1, num_total1) = arity1 in
      let (num_req2, num_total2) = arity2 in
      (* Arity range LHS is within arity range RHS *)
      let arities_are_valid = num_req1 >= num_req2 && num_total1 <= num_total2 in
      if not arities_are_valid then
        add_output cx ~trace (Error_message.ETupleArityMismatch ((r1, r2), arity1, arity2, use_op))
      else
        let n = ref 0 in
        let tuple_element_compat t1 t2 p1 p2 optional1 optional2 =
          if not (fresh || Polarity.compat (p1, p2)) then
            add_output
              cx
              ~trace
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
          match p2 with
          | Polarity.Positive -> rec_flow_t cx trace ~use_op (t1, t2)
          | Polarity.Negative -> rec_flow_t cx trace ~use_op (t2, t1)
          | Polarity.Neutral -> flow_to_mutable_child cx trace use_op fresh t1 t2
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
      | None -> add_output cx ~trace (Error_message.ENonLitArrayToTuple ((r1, r2), use_op))
      | Some (elements, arity) ->
        rec_flow_t
          cx
          trace
          ~use_op
          (DefT (r1, ArrT (TupleAT { elem_t = t1; elements; arity; react_dro })), u)
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
    | (DefT (_, FunT (static1, _)), DefT (_, ClassT (DefT (_, InstanceT { static = static2; _ }))))
      ->
      rec_unify cx trace ~use_op static1 static2
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
          ~trace
          (Error_message.EIncompatibleWithExact (reasons, use_op, Error_message.Inexact))
      | Indexed _ ->
        add_output cx ~trace (Error_message.EFunctionIncompatibleWithIndexer (reasons, use_op))
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
             trace
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
             trace
             ~use_op
             reason
             statics
             reason_inst
             (NameUtils.Map.filter
                (fun x _ -> x = OrdinaryName "constructor")
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
    | ( DefT (reason, BoolT _),
        DefT (interface_reason, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ })
      ) ->
      add_output
        cx
        ~trace
        (Error_message.EPrimitiveAsInterface { use_op; reason; interface_reason; kind = `Boolean })
    | ( DefT (reason, NumT _),
        DefT (interface_reason, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ })
      ) ->
      add_output
        cx
        ~trace
        (Error_message.EPrimitiveAsInterface { use_op; reason; interface_reason; kind = `Number })
    | ( DefT (reason, StrT _),
        DefT (interface_reason, InstanceT { inst = { inst_kind = InterfaceKind _; _ }; _ })
      ) ->
      add_output
        cx
        ~trace
        (Error_message.EPrimitiveAsInterface { use_op; reason; interface_reason; kind = `String })
    (**************************)
    (* opaque types supertype *)
    (**************************)
    (* Opaque types may be treated as their supertype when they are a lower bound for a use *)
    | (OpaqueT (opaque_t_reason, { super_t = Some t; _ }), _) ->
      rec_flow_t cx trace ~use_op:(Frame (OpaqueTypeBound { opaque_t_reason }, use_op)) (t, u)
    (***********************************************************)
    (* coercion                                                *)
    (***********************************************************)

    (* string and number can be coerced to strings *)
    | (DefT (_, NumT _), DefT (_, StrT _))
    (* TODO matching on the use_op seems wrong *)
      when match use_op with
           | Op (Coercion _) -> true
           | _ -> false ->
      ()
    (*********************)
    (* functions statics *)
    (*********************)
    | (DefT (reason, FunT (static, _)), AnyT _) ->
      rec_flow cx trace (static, ReposLowerT (reason, false, UseT (use_op, u)))
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
    | (DefT (_, EnumObjectT { enum_id = id1; _ }), DefT (_, EnumObjectT { enum_id = id2; _ }))
      when ALoc.equal_id id1 id2 ->
      ()
    | (DefT (_, EnumT { enum_id = id1; _ }), DefT (_, EnumT { enum_id = id2; _ }))
      when ALoc.equal_id id1 id2 ->
      ()
    | ( DefT
          ( _,
            EnumObjectT
              {
                enum_id = id1;
                enum_name = n1;
                members = m1;
                representation_t = r1;
                has_unknown_members = has_unknown1;
              }
          ),
        DefT
          ( _,
            EnumObjectT
              {
                enum_id = id2;
                enum_name = n2;
                members = m2;
                representation_t = r2;
                has_unknown_members = has_unknown2;
              }
          )
      )
    | ( DefT
          ( _,
            EnumT
              {
                enum_id = id1;
                enum_name = n1;
                members = m1;
                representation_t = r1;
                has_unknown_members = has_unknown1;
                _;
              }
          ),
        DefT
          ( _,
            EnumT
              {
                enum_id = id2;
                enum_name = n2;
                members = m2;
                representation_t = r2;
                has_unknown_members = has_unknown2;
              }
          )
      )
      when TypeUtil.nominal_id_have_same_logical_module
             ~file_options:Context.((metadata cx).file_options)
             (id1, Some n1)
             (id2, Some n2)
           && SSet.equal (SSet.of_list @@ SMap.keys m1) (SSet.of_list @@ SMap.keys m2)
           && has_unknown1 = has_unknown2 ->
      rec_flow_t cx trace ~use_op (r1, r2)
    | (DefT (enum_reason, EnumT { representation_t; _ }), t)
      when TypeUtil.quick_subtype representation_t t ->
      let representation_type =
        match representation_t with
        | DefT (_, BoolT _) -> Some "boolean"
        | DefT (_, NumT _) -> Some "number"
        | DefT (_, StrT _) -> Some "string"
        | DefT (_, SymbolT) -> Some "symbol"
        | DefT (_, BigIntT _) -> Some "bigint"
        | _ -> None
      in
      let casting_syntax = Context.casting_syntax cx in
      add_output
        cx
        ~trace
        (Error_message.EEnumIncompatible
           {
             reason_lower = enum_reason;
             reason_upper = reason_of_t t;
             use_op;
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
        ~trace
        (Error_message.EIncompatible
           {
             lower = (lreason, None);
             upper = (ureason, Error_message.IncompatibleMixedCallT);
             use_op = Some use_op;
             branches = [];
           }
        );
      rec_flow_t cx trace ~use_op (AnyT.make (AnyError None) lreason, u)
    | (FunProtoApplyT reason, _)
    | (FunProtoBindT reason, _)
    | (FunProtoCallT reason, _) ->
      rec_flow_t cx trace ~use_op (FunProtoT reason, u)
    | (InternalT (EnforceUnionOptimized reason), _) ->
      add_output
        cx
        ~trace
        (Error_message.EUnionOptimizationOnNonUnion
           { loc = loc_of_reason reason; arg = reason_of_t u }
        )
    | (_, _) ->
      add_output
        cx
        ~trace
        (Error_message.EIncompatibleWithUseOp
           { reason_lower = reason_of_t l; reason_upper = reason_of_t u; use_op }
        )
end
