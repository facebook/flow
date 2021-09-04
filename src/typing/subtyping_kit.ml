(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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

  include Flow_common.SUBTYPING
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
    Type.property * Type.property ->
    unit
end

module Make (Flow : INPUT) : OUTPUT = struct
  open Flow
  module SpeculationKit = Speculation_kit.Make (Flow)

  let flow_all_in_union cx trace rep u = iter_union ~f:rec_flow cx trace rep u

  let rec_flow_p cx ?trace ~use_op ?(report_polarity = true) lreason ureason propref = function
    (* unification cases *)
    | (Field (_, lt, Polarity.Neutral), Field (_, ut, Polarity.Neutral)) ->
      unify_opt cx ?trace ~use_op lt ut
    (* directional cases *)
    | (lp, up) ->
      let propref_error =
        match propref with
        | Named (_, x) -> Some x
        | Computed _ -> None
      in
      (match (Property.read_t lp, Property.read_t up) with
      | (Some lt, Some ut) -> flow_opt cx ?trace (lt, UseT (use_op, ut))
      | (None, Some _) when report_polarity ->
        add_output
          cx
          ?trace
          (Error_message.EPropPolarityMismatch
             ( (lreason, ureason),
               propref_error,
               (Property.polarity lp, Property.polarity up),
               use_op ))
      | _ -> ());
      (match (Property.write_t lp, Property.write_t up) with
      | (Some lt, Some ut) -> flow_opt cx ?trace (ut, UseT (use_op, lt))
      | (None, Some _) when report_polarity ->
        add_output
          cx
          ?trace
          (Error_message.EPropPolarityMismatch
             ( (lreason, ureason),
               propref_error,
               (Property.polarity lp, Property.polarity up),
               use_op ))
      | _ -> ())

  let flow_predicate_func =
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
    fun cx trace use_op (lreason, ft1) (ureason, ft2) ->
      match subst_map (0, SMap.empty) (ft1.params, ft2.params) with
      | Error (`ArityMismatch (n1, n2)) ->
        let mod_reason n =
          replace_desc_reason (RCustom (spf "predicate function with %d arguments" n))
        in
        let error =
          Error_message.EFunPredCustom
            ( (mod_reason n1 lreason, mod_reason n2 ureason),
              "Predicate function is incompatible with" )
        in
        add_output cx ~trace error
      | Error `NoParamNames ->
        let error = Error_message.(EInternal (aloc_of_reason ureason, PredFunWithoutParamNames)) in
        add_output cx ~trace error
      | Ok map ->
        let reason =
          update_desc_new_reason
            (fun desc -> RCustom (spf "predicate of %s" (string_of_desc desc)))
            (reason_of_t ft2.return_t)
        in
        (* We need to treat the return type of the predicated function as an
           annotation, to ensure that the LHS return type is checked against it,
           if ft2.return_t happens to be an OpenT. *)
        let out =
          if SMap.is_empty map then
            UseT (use_op, annot false ft2.return_t)
          else
            SubstOnPredT (use_op, reason, map, annot false ft2.return_t)
        in
        rec_flow cx trace (ft1.return_t, out)

  let flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj) =
    let { flags = lflags; call_t = lcall; props_tmap = lflds; proto_t = lproto } = l_obj in
    let { flags = rflags; call_t = ucall; props_tmap = uflds; proto_t = uproto } = u_obj in
    (* if inflowing type is literal (thus guaranteed to be
       unaliased), propertywise subtyping is sound *)
    let lit = is_literal_object_reason lreason || lflags.frozen in
    (* If both are dictionaries, ensure the keys and values are compatible
       with each other. *)
    let ldict = Obj_type.get_dict_opt lflags.obj_kind in
    let udict = Obj_type.get_dict_opt rflags.obj_kind in
    (match (ldict, udict) with
    | ( Some { key = lk; value = lv; dict_polarity = lpolarity; _ },
        Some { key = uk; value = uv; dict_polarity = upolarity; _ } ) ->
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
        (Field (None, lk, lpolarity), Field (None, uk, upolarity));
      rec_flow_p
        cx
        ~trace
        ~use_op:
          (Frame (PropertyCompatibility { prop = None; lower = lreason; upper = ureason }, use_op))
        lreason
        ureason
        (Computed uv)
        (Field (None, lv, lpolarity), Field (None, uv, upolarity))
    | _ -> ());

    if rflags.obj_kind = Exact && not (is_literal_object_reason ureason) then (
      Context.iter_real_props cx lflds (fun s _ ->
          if not (Context.has_prop cx uflds s) then
            let use_op =
              Frame
                ( PropertyCompatibility
                    {
                      prop = Some s;
                      (* Lower and upper are reversed in this case since the lower object
                       * is the one requiring the prop. *)
                      lower = ureason;
                      upper = lreason;
                    },
                  use_op )
            in
            let reason_prop = replace_desc_reason (RProperty (Some s)) lreason in
            let err =
              Error_message.EPropNotFound
                { prop_name = Some s; reason_prop; reason_obj = ureason; use_op; suggestion = None }
            in
            add_output cx ~trace err);
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
                  use_op )
            in
            let reason_prop = replace_desc_reason (RProperty prop) lreason in
            let err =
              Error_message.EPropNotFound
                { prop_name = prop; reason_prop; reason_obj = ureason; use_op; suggestion = None }
            in
            add_output cx ~trace err)
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
          if is_builtin_reason ALoc.source lreason then
            Error_message.EBuiltinLookupFailed { reason = reason_prop; name = prop_name }
          else
            Error_message.EStrictLookupFailed
              {
                reason_prop;
                reason_obj = lreason;
                name = prop_name;
                use_op = Some use_op;
                suggestion = None;
              }
        in
        add_output cx ~trace error_message)
    | None -> ());

    (* Properties in u must either exist in l, or match l's indexer. *)
    Context.iter_real_props cx uflds (fun s up ->
        let reason_prop = replace_desc_reason (RProperty (Some s)) ureason in
        let propref = Named (reason_prop, s) in
        let use_op' = use_op in
        let use_op =
          Frame (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op')
        in
        match (Context.get_prop cx lflds s, ldict) with
        | (Some lp, _) ->
          if lit then (
            (* prop from unaliased LB: check <:, then make exact *)
            (match (Property.read_t lp, Property.read_t up) with
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ());
            speculative_object_write cx lflds s up
          ) else
            (* prop from aliased LB *)
            rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
        | (None, Some { key; value; dict_polarity; _ }) when not (is_dictionary_exempt s) ->
          rec_flow
            cx
            trace
            ( string_key s reason_prop,
              UseT
                (Frame (IndexerKeyCompatibility { lower = lreason; upper = ureason }, use_op'), key)
            );
          let lp = Field (None, value, dict_polarity) in
          let up =
            match up with
            | Field (loc, OptionalT { reason = _; type_ = ut; use_desc = _ }, upolarity) ->
              Field (loc, ut, upolarity)
            | _ -> up
          in
          if lit then
            match (Property.read_t lp, Property.read_t up) with
            | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ()
          else
            rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
        | _ ->
          (* property doesn't exist in inflowing type *)
          (match up with
          | Field (_, OptionalT _, _) when lit ->
            (* if property is marked optional or otherwise has a maybe type,
               and if inflowing type is a literal (i.e., it is not an
               annotation), then we add it to the inflowing type as
               an optional property *)
            speculative_object_write cx lflds s up
          | Field (_, OptionalT _, Polarity.Positive)
            when Obj_type.is_exact_or_sealed ureason lflags.obj_kind ->
            rec_flow
              cx
              trace
              ( lproto,
                LookupT
                  {
                    reason = ureason;
                    lookup_kind = NonstrictReturning (None, None);
                    ts = [];
                    propref;
                    lookup_action = LookupProp (use_op, up);
                    method_accessible = true;
                    ids = None;
                  } )
          | _ ->
            (* When an object type is unsealed, typing it as another object type should add properties
               of that object type to it as needed. We do this when not speculating, because adding
               properties changes state, and the state change is necessary to enforce
               consistency. *)
            if not (Obj_type.sealed_in_op ureason lflags.obj_kind) then
              speculative_object_write cx lflds s up
            else
              (* otherwise, look up the property in the prototype *)
              let strict =
                match (Obj_type.sealed_in_op ureason lflags.obj_kind, ldict) with
                | (false, None) -> ShadowRead (Some lreason, Nel.one lflds)
                | (true, None) -> Strict lreason
                | _ -> NonstrictReturning (None, None)
              in
              rec_flow
                cx
                trace
                ( lproto,
                  LookupT
                    {
                      reason = ureason;
                      lookup_kind = strict;
                      ts = [];
                      propref;
                      lookup_action = LookupProp (use_op, up);
                      method_accessible = true;
                      ids = None;
                    } )));

    (* Any properties in l but not u must match indexer *)
    (match udict with
    | None -> ()
    | Some { key; value; dict_polarity; _ } ->
      let keys =
        Context.fold_real_props
          cx
          lflds
          (fun s lp keys ->
            if Context.has_prop cx uflds s then
              keys
            else
              let use_op =
                Frame
                  (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op)
              in
              let lp =
                match lp with
                | Field (loc, OptionalT { reason = _; type_ = lt; use_desc = _ }, lpolarity) ->
                  Field (loc, lt, lpolarity)
                | _ -> lp
              in
              let up = Field (None, value, dict_polarity) in
              begin
                if lit then
                  match (Property.read_t lp, Property.read_t up) with
                  | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
                  | _ -> ()
                else
                  let reason_prop = replace_desc_reason (RProperty (Some s)) lreason in
                  let propref = Named (reason_prop, s) in
                  rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
              end;
              string_key s lreason :: keys)
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
          mk_reason (RUnknownUnspecifiedProperty (desc_of_reason lreason)) (aloc_of_reason lreason)
        in
        let mixed = DefT (r, bogus_trust (), MixedT Mixed_everything) in
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
        let s = OrdinaryName "$call" in
        let use_op =
          Frame (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op)
        in
        let lp =
          match Context.find_call cx lcall with
          | OptionalT { reason = _; type_ = t; use_desc = _ } -> Field (None, t, Polarity.Positive)
          | t -> Field (None, t, Polarity.Positive)
        in
        let up = Field (None, value, dict_polarity) in
        if lit then
          match (Property.read_t lp, Property.read_t up) with
          | (Some lt, Some ut) -> rec_flow cx trace (lt, UseT (use_op, ut))
          | _ -> ()
        else
          let reason_prop = replace_desc_reason (RProperty (Some s)) lreason in
          let propref = Named (reason_prop, s) in
          rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
      | _ -> ()));

    rec_flow
      cx
      trace
      (uproto, ReposUseT (ureason, false, use_op, DefT (lreason, bogus_trust (), ObjT l_obj)))

  let match_shape cx trace ~use_op proto reason props =
    let reason_op = reason_of_t proto in
    NameUtils.Map.iter
      (fun x p ->
        let reason_prop = update_desc_reason (fun desc -> RPropertyOf (x, desc)) reason in
        match Property.read_t p with
        | Some t ->
          let use_op =
            Frame
              (PropertyCompatibility { prop = Some x; upper = reason; lower = reason_op }, use_op)
          in
          let propref = Named (reason_prop, x) in
          let t = filter_optional cx ~trace reason_prop t in
          rec_flow cx trace (proto, MatchPropT (use_op, reason_op, propref, (reason_prop, t)))
        | None ->
          add_output
            cx
            ~trace
            (Error_message.EPropNotReadable { reason_prop; prop_name = Some x; use_op }))
      props

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
    (***************************)
    (* type destructor trigger *)
    (***************************)

    (* For evaluating type destructors we add a trigger, TypeDestructorTriggerT,
     * to both sides of a type. When TypeDestructorTriggerT sees a new upper or
     * lower bound we destruct that bound and flow the result in the same
     * direction to some tout type. *)

    (* Don't let two TypeDestructorTriggerTs reach each other or else we quickly
     * run into non-termination scenarios. *)
    | (TypeDestructorTriggerT _, TypeDestructorTriggerT _) -> ()
    | (l, TypeDestructorTriggerT (use_op', reason, repos, d, tout)) ->
      let l =
        match repos with
        | None -> l
        | Some (reason, use_desc) -> reposition_reason cx ~trace reason ~use_desc l
      in
      eval_destructor cx ~trace use_op' reason l d tout
    | (TypeDestructorTriggerT (use_op', reason, _, d, tout), AnnotT (r, t, use_desc)) ->
      let tout' =
        Tvar.mk_no_wrap_where cx reason (fun tout' ->
            let repos = Some (r, use_desc) in
            rec_flow
              cx
              trace
              (t, UseT (use_op, TypeDestructorTriggerT (use_op', reason, repos, d, tout'))))
      in
      rec_flow cx trace (tout', ReposUseT (reason, false, use_op, OpenT tout))
    | (TypeDestructorTriggerT (use_op', reason, _, d, tout), _) ->
      (* With the same "slingshot" trick used by AnnotT, hold the lower bound
       * at bay until result itself gets concretized, and then flow the lower
       * bound to that concrete type. *)
      let t =
        Tvar.mk_no_wrap_where cx reason (fun t -> eval_destructor cx ~trace use_op' reason u d t)
      in
      let use_desc = false in
      rec_flow cx trace (t, ReposUseT (reason, use_desc, use_op, OpenT tout))
    (* if a ReposT is used as a lower bound, `reposition` can reposition it *)
    | (ReposT (reason, l), _) ->
      rec_flow_t cx trace ~use_op (reposition_reason cx ~trace reason l, u)
    (* if a ReposT is used as an upper bound, wrap the now-concrete lower bound
       in a `ReposUpperT`, which will repos `u` when `u` becomes concrete. *)
    | (_, ReposT (reason, u)) ->
      rec_flow cx trace (InternalT (ReposUpperT (reason, l)), UseT (use_op, u))
    | (InternalT (ReposUpperT (reason, l)), u) ->
      (* since this guarantees that `u` is not an OpenT, it's safe to use
       * `reposition` on the upper bound here. *)
      let u = reposition_reason cx ~trace reason u in
      rec_flow cx trace (l, UseT (use_op, u))
    (* The sink component of an annotation constrains values flowing
       into the annotated site. *)
    | (_, AnnotT (r, t, use_desc)) -> rec_flow cx trace (t, ReposUseT (r, use_desc, use_op, l))
    | (AnnotT (r, t, use_desc), _) ->
      let t = reposition_reason ~trace cx r ~use_desc t in
      rec_flow_t cx trace ~use_op (t, u)
    (*******************************)
    (* common implicit conversions *)
    (*******************************)
    | (_, DefT (_, _, NumT _)) when numeric l -> ()
    | (DefT (reason, _, IdxWrapper _), _) -> add_output cx ~trace (Error_message.EIdxUse1 reason)
    | (DefT (r, trust, (NullT | VoidT)), MaybeT (_, tout)) ->
      rec_flow_t cx trace ~use_op (EmptyT.why r trust, tout)
    | (DefT (r, trust, MixedT Mixed_everything), MaybeT (_, tout)) ->
      rec_flow_t cx trace ~use_op (DefT (r, trust, MixedT Mixed_non_maybe), tout)
    | (MaybeT (r, t), MaybeT _) ->
      let t = push_type_alias_reason r t in
      rec_flow_t cx trace ~use_op (t, u)
    | (MaybeT (reason, t), _) ->
      let reason = replace_desc_reason RNullOrVoid reason in
      let t = push_type_alias_reason reason t in
      rec_flow_t cx trace ~use_op (NullT.make reason |> with_trust Trust.bogus_trust, u);
      rec_flow_t cx trace ~use_op (VoidT.make reason |> with_trust Trust.bogus_trust, u);
      rec_flow_t cx trace ~use_op (t, u)
    | (DefT (r, trust, VoidT), OptionalT { reason = _; type_ = tout; use_desc = _ }) ->
      rec_flow_t cx trace ~use_op (EmptyT.why r trust, tout)
    | (OptionalT { reason = _; type_ = t; use_desc = _ }, OptionalT _)
    | (OptionalT { reason = _; type_ = t; use_desc = _ }, MaybeT _) ->
      rec_flow_t cx trace ~use_op (t, u)
    | (OptionalT { reason = r; type_ = t; use_desc }, _) ->
      rec_flow_t
        cx
        trace
        ~use_op
        (VoidT.why_with_use_desc ~use_desc r |> with_trust Trust.bogus_trust, u);
      rec_flow_t cx trace ~use_op (t, u)
    | (ThisTypeAppT (reason_tapp, c, this, ts), _) ->
      let reason_op = reason_of_t u in
      let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
      instantiate_this_class cx trace reason_tapp tc this (Upper (UseT (use_op, u)))
    | (_, ThisTypeAppT (reason_tapp, c, this, ts)) ->
      let reason_op = reason_of_t l in
      let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
      instantiate_this_class cx trace reason_tapp tc this (Lower (use_op, l))
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
     * true. *)
    | (TypeAppT (r1, op1, c1, ts1), TypeAppT (r2, op2, c2, ts2)) ->
      if TypeAppExpansion.push_unless_loop cx (c1, ts1) then (
        if TypeAppExpansion.push_unless_loop cx (c2, ts2) then (
          rec_flow
            cx
            trace
            (c2, ConcretizeTypeAppsT (use_op, (ts2, op2, r2), (c1, ts1, op1, r1), true));
          TypeAppExpansion.pop ()
        );
        TypeAppExpansion.pop ()
      )
    | (TypeAppT (reason_tapp, use_op_tapp, c, ts), _) ->
      if TypeAppExpansion.push_unless_loop cx (c, ts) then (
        let reason_op = reason_of_t u in
        let t = mk_typeapp_instance cx ~trace ~use_op:use_op_tapp ~reason_op ~reason_tapp c ts in
        rec_flow_t cx trace ~use_op (t, u);
        TypeAppExpansion.pop ()
      )
    | (_, TypeAppT (reason_tapp, use_op_tapp, c, ts)) ->
      if TypeAppExpansion.push_unless_loop cx (c, ts) then (
        let reason_op = reason_of_t l in
        let t = mk_typeapp_instance cx ~trace ~use_op:use_op_tapp ~reason_op ~reason_tapp c ts in
        rec_flow cx trace (l, UseT (use_op, t));
        TypeAppExpansion.pop ()
      )
    (**********************)
    (*    opaque types    *)
    (**********************)

    (* If the ids are equal, we use flow_type_args to make sure that the type arguments of each
     * are compatible with each other. If there are no type args, this doesn't do anything *)
    | ( OpaqueT (lreason, { opaque_id = id1; opaque_type_args = ltargs; _ }),
        OpaqueT (ureason, { opaque_id = id2; opaque_type_args = utargs; _ }) )
      when ALoc.equal_id id1 id2 ->
      flow_type_args cx trace ~use_op lreason ureason ltargs utargs
    (* If the type is still in the same file it was defined, we allow it to
     * expose its underlying type information *)
    | (OpaqueT (r, { underlying_t = Some t; _ }), _)
      when ALoc.source (aloc_of_reason r) = ALoc.source (def_aloc_of_reason r) ->
      rec_flow_t cx trace ~use_op (t, u)
    (* If the lower bound is in the same file as where the opaque type was defined,
     * we expose the underlying type information *)
    | (_, OpaqueT (r, { underlying_t = Some t; _ }))
      when ALoc.source (aloc_of_reason (reason_of_t l)) = ALoc.source (def_aloc_of_reason r) ->
      rec_flow_t cx trace ~use_op (l, t)
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
    | (DefT (rl, _, StrT actual), DefT (ru, _, SingletonStrT expected)) ->
      if TypeUtil.literal_eq expected actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          ~trace
          (Error_message.EExpectedStringLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, _, NumT actual), DefT (ru, _, SingletonNumT expected)) ->
      if TypeUtil.number_literal_eq expected actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          ~trace
          (Error_message.EExpectedNumberLit { reason_lower = rl; reason_upper = ru; use_op })
    | (DefT (rl, _, BoolT actual), DefT (ru, _, SingletonBoolT expected)) ->
      if TypeUtil.boolean_literal_eq expected actual then
        ()
      else
        (* TODO: ordered_reasons should not be necessary *)
        let (rl, ru) = FlowError.ordered_reasons (rl, ru) in
        add_output
          cx
          ~trace
          (Error_message.EExpectedBooleanLit { reason_lower = rl; reason_upper = ru; use_op })
    (*****************************************************)
    (* keys (NOTE: currently we only support string keys *)
    (*****************************************************)
    | ( ( DefT (reason_s, _, StrT literal)
        | GenericT { reason = reason_s; bound = DefT (_, _, StrT literal); _ } ),
        KeysT (reason_op, o) ) ->
      let reason_next =
        match literal with
        | Literal (_, x) -> replace_desc_new_reason (RProperty (Some x)) reason_s
        | _ -> replace_desc_new_reason RUnknownString reason_s
      in
      (* check that o has key x *)
      let u = HasOwnPropT (use_op, reason_next, l) in
      rec_flow cx trace (o, ReposLowerT (reason_op, false, u))
    | (KeysT (reason1, o1), _) ->
      (* flow all keys of o1 to u *)
      rec_flow cx trace (o1, GetKeysT (reason1, UseT (use_op, u)))
    (* Identical predicates are okay, just do the base type check. *)
    | ( OpenPredT { base_t = t1; m_pos = p_pos_1; m_neg = _; reason = lreason },
        OpenPredT { base_t = t2; m_pos = p_pos_2; m_neg = _; reason = ureason } ) ->
      if TypeUtil.pred_map_implies p_pos_1 p_pos_2 then
        rec_flow_t ~use_op cx trace (t1, t2)
      else
        let error =
          Error_message.EIncompatibleWithUseOp
            { reason_lower = lreason; reason_upper = ureason; use_op }
        in
        add_output cx ~trace error
    (*********************************************)
    (* Using predicate functions as regular ones *)
    (*********************************************)
    | (OpenPredT { base_t = l; m_pos = _; m_neg = _; reason = _ }, _) ->
      rec_flow_t cx trace ~use_op (l, u)
    | (UnionT (reason, rep), _) when UnionRep.members rep |> List.exists is_union_resolvable ->
      iter_resolve_union ~f:rec_flow cx trace reason rep (UseT (use_op, u))
    (* cases where there is no loss of precision *)
    | (UnionT _, UnionT _)
      when union_optimization_guard cx (Context.trust_errors cx |> TypeUtil.quick_subtype) l u ->
      if Context.is_verbose cx then prerr_endline "UnionT ~> UnionT fast path"
    (* Optimization to treat maybe and optional types as special unions for subset comparision *)
    | (UnionT (reason, rep), MaybeT (r, maybe)) ->
      let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
      let void = VoidT.why r |> with_trust bogus_trust in
      let null = NullT.why r |> with_trust bogus_trust in
      let filter_void t = quick_subtype t void in
      let filter_null t = quick_subtype t null in
      let filter_null_and_void t = filter_void t || filter_null t in
      let maybe = push_type_alias_reason r maybe in
      (* if the union doesn't contain void or null,
         then everything in it must be upper-bounded by maybe *)
      begin
        match
          ( UnionRep.quick_mem_enum ~quick_subtype void rep,
            UnionRep.quick_mem_enum ~quick_subtype null rep )
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
      let quick_subtype = TypeUtil.quick_subtype (Context.trust_errors cx) in
      let void = VoidT.why_with_use_desc ~use_desc r |> with_trust bogus_trust in
      let filter_void t = quick_subtype t void in
      (* if the union doesn't contain void, then everything in it must be upper-bounded by u *)
      begin
        match UnionRep.quick_mem_enum ~quick_subtype void rep with
        | UnionRep.No -> rec_flow_t ~use_op cx trace (l, opt)
        | UnionRep.Yes ->
          rec_flow_t ~use_op cx trace (remove_predicate_from_union reason cx filter_void rep, opt)
        | _ -> flow_all_in_union cx trace rep (UseT (use_op, u))
      end
    | (UnionT (_, rep), _) ->
      (if Context.is_verbose cx then
        match u with
        | UnionT _ -> prerr_endline "UnionT ~> UnionT slow case"
        | IntersectionT _ -> prerr_endline "UnionT ~> IntersectionT slow case"
        | _ -> ());

      flow_all_in_union cx trace rep (UseT (use_op, u))
    | (_, IntersectionT (_, rep)) ->
      (if Context.is_verbose cx then
        match l with
        | UnionT _ -> prerr_endline "IntersectionT ~> UnionT slow case"
        | _ -> ());
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
    | (IntersectionT (_, rep), u) when List.mem u (InterRep.members rep) -> ()
    (* String enum sets can be handled in logarithmic time by just
     * checking for membership in the set.
     *)
    | (DefT (reason_l, _, StrT (Literal (_, x))), UnionT (reason_u, rep))
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
                    });
             true
           | _ -> false ->
      ()
    | (_, UnionT (_, rep))
      when let ts = Type_mapper.union_flatten cx @@ UnionRep.members rep in
           List.exists (TypeUtil.quick_subtype (Context.trust_errors cx) l) ts ->
      ()
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
    | (IntersectionT _, DefT (r, _, ObjT { flags; props_tmap; proto_t; call_t }))
      when NameUtils.Map.cardinal (Context.find_props cx props_tmap) > 1 ->
      Context.iter_real_props cx props_tmap (fun x p ->
          let pmap = NameUtils.Map.singleton x p in
          let id = Context.generate_property_map cx pmap in
          let obj = mk_objecttype ~flags ~call:call_t id dummy_prototype in
          rec_flow cx trace (l, UseT (use_op, DefT (r, bogus_trust (), ObjT obj))));
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
    | (MatchingPropT (reason, x, t), l) ->
      (* Things that can have properties are object-like (objects, instances,
       * and their exact versions). Notably, "meta" types like union, annot,
       * typeapp, eval, maybe, optional, and intersection should have boiled
       * away by this point. Generics should have been "unsealed" as well. *)
      let propref = Named (reason, OrdinaryName x) in
      let strict = NonstrictReturning (None, None) in
      let u =
        LookupT
          {
            reason;
            lookup_kind = strict;
            ts = [];
            propref;
            lookup_action = MatchProp (use_op, t);
            method_accessible = true;
            ids = Some Properties.Set.empty;
          }
      in
      rec_flow cx trace (l, u)
    | (DefT (reason, trust, SingletonStrT key), _) ->
      rec_flow_t cx trace ~use_op (DefT (reason, trust, StrT (Literal (None, key))), u)
    | (DefT (reason, trust, SingletonNumT lit), _) ->
      rec_flow_t cx trace ~use_op (DefT (reason, trust, NumT (Literal (None, lit))), u)
    | (DefT (reason, trust, SingletonBoolT b), _) ->
      rec_flow_t cx trace ~use_op (DefT (reason, trust, BoolT (Some b)), u)
    | (NullProtoT reason, _) -> rec_flow_t cx trace ~use_op (DefT (reason, bogus_trust (), NullT), u)
    (************************************************************************)
    (* exact object types *)
    (************************************************************************)

    (* ExactT<X> comes from annotation, may behave as LB or UB *)

    (* when $Exact<LB> ~> UB, forward to MakeExactT *)
    | (ExactT (r, t), _) ->
      let t = push_type_alias_reason r t in
      rec_flow cx trace (t, MakeExactT (r, Upper (UseT (use_op, u))))
    (* ObjT LB ~> $Exact<UB>. make exact if exact and unsealed *)
    | (DefT (_, _, ObjT { flags; _ }), ExactT (r, t)) ->
      if Obj_type.is_exact_or_sealed r flags.obj_kind then
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
    | ((DefT (_, _, EmptyT) | AnyT _), ExactT (_, t)) -> rec_flow_t cx trace ~use_op (l, t)
    (*
     * Shapes need to be trapped here to avoid error-ing when used as exact types.
     * Below (see "matching shapes of objects"), we have a rule that allows ShapeT(o)
     * to be used just as o is allowed to be used.
     *)
    | (ShapeT (_, o), ExactT _) -> rec_flow_t cx trace ~use_op (o, u)
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
     * statements such as "show that for all X:U" correspond to "show that for
     * both X = bottom and X = U, ...".
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
    | (DefT (_, _, PolyT { id = id1; _ }), DefT (_, _, PolyT { id = id2; _ }))
      when Poly.equal_id id1 id2 ->
      if Context.is_verbose cx then prerr_endline "PolyT ~> PolyT fast path"
    | ( DefT (r1, _, PolyT { tparams_loc = tparams_loc1; tparams = params1; t_out = t1; id = id1 }),
        DefT (r2, _, PolyT { tparams_loc = tparams_loc2; tparams = params2; t_out = t2; id = id2 })
      ) ->
      let n1 = Nel.length params1 in
      let n2 = Nel.length params2 in
      if n2 > n1 then
        add_output cx ~trace (Error_message.ETooManyTypeArgs (r2, r1, n1))
      else if n2 < n1 then
        add_output cx ~trace (Error_message.ETooFewTypeArgs (r2, r1, n1))
      else
        (* for equal-arity polymorphic types, flow param upper bounds, then
         * instances parameterized by these *)
        let args1 = instantiate_poly_param_upper_bounds cx params1 in
        let args2 = instantiate_poly_param_upper_bounds cx params2 in
        List.iter2 (fun arg1 arg2 -> rec_flow_t cx trace ~use_op (arg2, arg1)) args1 args2;
        let inst1 =
          let r = reason_of_t t1 in
          mk_typeapp_of_poly
            cx
            trace
            ~use_op
            ~reason_op:r
            ~reason_tapp:r
            id1
            tparams_loc1
            params1
            t1
            args1
        in
        let inst2 =
          let r = reason_of_t t2 in
          mk_typeapp_of_poly
            cx
            trace
            ~use_op
            ~reason_op:r
            ~reason_tapp:r
            id2
            tparams_loc2
            params2
            t2
            args2
        in
        rec_flow_t ~use_op cx trace (inst1, inst2)
    (* general case **)
    | (_, DefT (_, _, PolyT { tparams = ids; t_out = t; _ })) ->
      check_with_generics cx (Nel.to_list ids) (fun map_ ->
          rec_flow cx trace (l, UseT (use_op, Subst.subst cx ~use_op map_ t)))
    (* TODO: ideally we'd do the same when lower bounds flow to a
     * this-abstracted class, but fixing the class is easier; might need to
     * revisit *)
    | (_, ThisClassT (r, i, this)) ->
      let reason = reason_of_t l in
      rec_flow cx trace (l, UseT (use_op, fix_this_class cx trace reason (r, i, this)))
    | (DefT (reason_tapp, _, PolyT { tparams_loc; tparams = ids; _ }), DefT (_, _, TypeT _)) ->
      (* TODO: add use op to missing type arg error? *)
      add_output
        cx
        ~trace
        (Error_message.EMissingTypeArgs
           {
             reason_tapp;
             reason_arity = mk_poly_arity_reason tparams_loc;
             min_arity = poly_minimum_arity ids;
             max_arity = Nel.length ids;
           })
    (*
     * This rule is hit when a polymorphic type appears outside a
     * type application expression - i.e. not followed by a type argument list
     * delimited by angle brackets.
     * We want to require full expressions in type positions like annotations,
     * but allow use of polymorphically-typed values - for example, in class
     * extends clauses and at function call sites - without explicit type
     * arguments, since typically they're easily inferred from context.
     *)
    | (DefT (reason_tapp, _, PolyT { tparams_loc; tparams = ids; t_out = t; _ }), _) ->
      let reason_op = reason_of_t u in
      let t_ = instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp (tparams_loc, ids, t) in
      rec_flow_t cx trace ~use_op (t_, u)
    (* when a this-abstracted class flows to upper bounds, fix the class *)
    | (ThisClassT (r, i, this), _) ->
      let reason = reason_of_t u in
      rec_flow_t cx trace ~use_op (fix_this_class cx trace reason (r, i, this), u)
    (*****************************)
    (* React Abstract Components *)
    (*****************************)
    (*
     * In all of these cases, we check:
     *  1. configu <: configl
     *  2. default_propsl = default_propsu
     *  3. instancel <: instanceu
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
     *)

    (* Class component ~> AbstractComponent *)
    | ( DefT (reasonl, _, ClassT this),
        DefT (_reasonu, _, ReactAbstractComponentT { config; instance }) ) ->
      (* Contravariant config check *)
      React_kit.get_config
        cx
        trace
        l
        ~use_op
        ~reason_op:reasonl
        ~rec_flow
        ~rec_flow_t
        ~rec_unify
        ~get_builtin_type
        ~add_output
        (React.GetConfig l)
        Polarity.Negative
        config;

      (* check instancel <: instanceu *)
      rec_flow_t cx trace ~use_op (this, instance)
    (* Function Component ~> AbstractComponent *)
    | ( DefT (reasonl, _, FunT (_, _, { return_t; _ })),
        DefT (_reasonu, _, ReactAbstractComponentT { config; instance }) ) ->
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
      rec_flow_t ~use_op cx trace (return_t, get_builtin_type cx reasonl (OrdinaryName "React$Node"));

      (* A function component instance type is always void, so flow void to instance *)
      rec_flow_t
        cx
        trace
        ~use_op
        (VoidT.make (replace_desc_new_reason RVoid reasonl) |> with_trust bogus_trust, instance)
    (* Object Component ~> AbstractComponent *)
    | ( DefT (reasonl, _, ObjT { call_t = Some id; _ }),
        DefT (reasonu, trust, ReactAbstractComponentT { config; instance }) ) ->
      rec_flow cx trace (l, ReactKitT (use_op, reasonl, React.ConfigCheck config));

      (* Ensure the callable signature's return type is compatible with React.Node. We
       * do this by flowing it to (...empty): React.Node *)
      let funtype =
        mk_functiontype
          reasonu
          []
          ~rest_param:
            (Some
               ( None,
                 aloc_of_reason reasonu,
                 EmptyT.why (replace_desc_new_reason REmpty reasonu) (bogus_trust ()) ))
          ~def_reason:reasonl
          (get_builtin_type cx reasonu (OrdinaryName "React$Node"))
      in
      let mixed = MixedT.why reasonu (bogus_trust ()) in
      rec_flow_t
        ~use_op
        cx
        trace
        (Context.find_call cx id, DefT (reasonu, trust, FunT (mixed, mixed, funtype)));

      (* An object component instance type is always void, so flow void to instance *)
      rec_flow_t
        cx
        trace
        ~use_op
        (VoidT.make (replace_desc_new_reason RVoid reasonl) |> with_trust bogus_trust, instance)
    (* AbstractComponent ~> AbstractComponent *)
    | ( DefT (_reasonl, _, ReactAbstractComponentT { config = configl; instance = instancel }),
        DefT (_reasonu, _, ReactAbstractComponentT { config = configu; instance = instanceu }) ) ->
      rec_flow_t cx trace ~use_op (configu, configl);
      rec_flow_t cx trace ~use_op (instancel, instanceu)
    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    (* FunT ~> FunT *)
    | (DefT (lreason, _, FunT (_, _, ft1)), DefT (ureason, _, FunT (_, _, ft2))) ->
      let use_op =
        Frame
          ( FunCompatibility { lower = lreason; upper = ureason },
            (* The $call PropertyCompatibility is redundant when we have a
             * FunCompatibility use_op. *)
            match use_op with
            | Frame (PropertyCompatibility { prop = Some (OrdinaryName "$call"); _ }, use_op) ->
              use_op
            | _ -> use_op )
      in

      begin
        let use_op =
          Frame (FunParam { n = 0; name = Some "this"; lower = lreason; upper = ureason }, use_op)
        in
        let (this_param1, subtyping1) = ft1.this_t in
        let (this_param2, subtyping2) = ft2.this_t in
        match (subtyping1, subtyping2) with
        (* Both methods *)
        | (false, false) ->
          rec_flow
            cx
            trace
            (subtype_this_of_function ft2, UseT (use_op, subtype_this_of_function ft1))
        (* lower bound method, upper bound function
           This is always banned, as it would allow methods to be unbound through casting *)
        | (false, true) ->
          add_output
            cx
            ~trace
            (Error_message.EMethodUnbinding
               { use_op; reason_op = lreason; reason_prop = reason_of_t this_param1 });
          rec_flow cx trace (this_param2, UseT (use_op, subtype_this_of_function ft1))
        (* lower bound function, upper bound method.
           Ok as long as the types match up *)
        | (true, false)
        (* Both functions *)
        | (true, true) ->
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

      (* Well-formedness adjustment: If this is predicate function subtyping,
         make sure to apply a latent substitution on the right-hand use to
         bridge the mismatch of the parameter naming. Otherwise, proceed with
         the subtyping of the return types normally. In general it should
         hold as an invariant that OpenPredTs (where free variables appear)
         should not flow to other OpenPredTs without wrapping the latter in
         SubstOnPredT.
      *)
      if ft2.is_predicate then
        if not ft1.is_predicate then
          (* Non-predicate functions are incompatible with predicate ones
             TODO: somehow the original flow needs to be propagated as well *)
          add_output
            cx
            ~trace
            (Error_message.EFunPredCustom ((lreason, ureason), "Function is incompatible with"))
        else
          flow_predicate_func cx trace use_op (lreason, ft1) (ureason, ft2)
      else
        let use_op =
          Frame
            ( FunReturn { lower = reason_of_t ft1.return_t; upper = reason_of_t ft2.return_t },
              use_op )
        in
        rec_flow cx trace (ft1.return_t, UseT (use_op, ft2.return_t))
    | (DefT (reason, _, StrT (Literal (_, name))), DefT (reason_op, _, CharSetT chars)) ->
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
               }))
    | (DefT (reason, trust, CharSetT _), _) -> rec_flow_t cx trace ~use_op (StrT.why reason trust, u)
    | (_, DefT (reason, trust, CharSetT _)) -> rec_flow_t cx trace ~use_op (l, StrT.why reason trust)
    | ( CustomFunT (reason, ReactPropType (React.PropType.Primitive (req, _))),
        (DefT (_, _, ObjT _) | DefT (_, _, FunT _) | AnyT _) ) ->
      let builtin_name =
        if req then
          "ReactPropsCheckType"
        else
          "ReactPropsChainableTypeChecker"
      in
      let l = get_builtin_type cx ~trace reason (OrdinaryName builtin_name) in
      rec_flow_t cx trace ~use_op (l, u)
    | ( CustomFunT (reason, ReactPropType (React.PropType.Complex kind)),
        (DefT (_, _, ObjT _) | DefT (_, _, FunT _) | AnyT _) ) ->
      rec_flow_t cx trace ~use_op (get_builtin_prop_type cx ~trace reason kind, u)
    | ( CustomFunT (_, ReactPropType (React.PropType.Primitive (is_req1, t1))),
        CustomFunT (_, ReactPropType (React.PropType.Primitive (is_req2, t2))) )
      when (not is_req2) || is_req1 ->
      rec_unify cx trace ~use_op t1 t2
    (* Custom functions are still functions, so they have all the prototype properties *)
    | (CustomFunT (r, _), AnyT _) -> rec_flow_t cx trace ~use_op (FunProtoT r, u)
    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    (* ObjT -> ObjT *)
    | ( DefT (lreason, _, ObjT ({ props_tmap = lflds; _ } as l_obj)),
        DefT (ureason, _, ObjT ({ props_tmap = uflds; _ } as u_obj)) ) ->
      let u_deft = u in
      Type_inference_hooks_js.dispatch_obj_to_obj_hook cx l u_deft;
      let print_fast_path =
        match Context.verbose cx with
        | Some _ -> true
        | _ -> false
      in
      if lflds = uflds then (
        if print_fast_path then prerr_endline "ObjT ~> ObjT fast path: yes"
      ) else (
        if print_fast_path then prerr_endline "ObjT ~> ObjT fast path: no";
        flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj)
      )
    | (DefT (_, _, ObjT _), NullProtoT _) -> ()
    (* InstanceT -> ObjT *)
    | ( DefT (lreason, _, InstanceT _),
        DefT (ureason, _, ObjT { flags = { obj_kind = Exact; _ }; _ }) ) ->
      let reasons = FlowError.ordered_reasons (lreason, ureason) in
      add_output
        cx
        ~trace
        (Error_message.EIncompatibleWithExact (reasons, use_op, Error_message.Inexact))
    | ( DefT
          ( lreason,
            _,
            InstanceT
              (_, super, _, { own_props = lown; proto_props = lproto; inst_call_t = lcall; _ }) ),
        DefT (ureason, _, ObjT { props_tmap = uflds; proto_t = uproto; call_t = ucall; _ }) ) ->
      add_output cx ~trace (Error_message.EClassToObject (lreason, ureason, use_op));
      let u_deft = u in
      Type_inference_hooks_js.dispatch_instance_to_obj_hook cx l u_deft;

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
              if is_builtin_reason ALoc.source lreason then
                Error_message.EBuiltinLookupFailed { reason = reason_prop; name = prop_name }
              else
                Error_message.EStrictLookupFailed
                  {
                    reason_prop;
                    reason_obj = lreason;
                    name = prop_name;
                    use_op = Some use_op;
                    suggestion = None;
                  }
            in
            add_output cx ~trace error_message);

      Context.iter_real_props cx uflds (fun s up ->
          let use_op =
            Frame (PropertyCompatibility { prop = Some s; lower = lreason; upper = ureason }, use_op)
          in
          let propref =
            let reason_prop = replace_desc_reason (RProperty (Some s)) ureason in
            Named (reason_prop, s)
          in
          match NameUtils.Map.find_opt s lflds with
          | Some lp -> rec_flow_p cx ~trace ~use_op lreason ureason propref (lp, up)
          | _ ->
            let strict =
              match up with
              | Field (_, OptionalT _, _) -> NonstrictReturning (None, None)
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
                        lookup_kind = strict;
                        ts = [];
                        propref;
                        lookup_action = LookupProp (use_op, up);
                        method_accessible = false;
                        ids = Some (Properties.Set.of_list [lown; lproto]);
                      } ) ));

      rec_flow cx trace (l, UseT (use_op, uproto))
    (* For some object `x` and constructor `C`, if `x instanceof C`, then the
     * object is a subtype. We use `ExtendsT` to walk the proto chain of the
     * object, in case it includes a nominal type. *)
    | (DefT (_, _, ObjT _), DefT (_, _, InstanceT _)) ->
      rec_flow cx trace (l, extends_use_type use_op l u)
    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | (DefT (reason, _, (ObjT _ | InstanceT _)), (DefT (reason_op, _, FunT _) | AnyT (reason_op, _)))
      ->
      let prop_name = Some (OrdinaryName "$call") in
      let use_op =
        match u with
        | DefT (_, _, FunT _)
        | AnyT _ ->
          Frame
            (PropertyCompatibility { prop = prop_name; lower = reason; upper = reason_op }, use_op)
        | _ -> use_op
      in
      let fun_t =
        match l with
        | DefT (_, _, ObjT { call_t = Some id; _ })
        | DefT (_, _, InstanceT (_, _, _, { inst_call_t = Some id; _ })) ->
          Context.find_call cx id
        | _ ->
          let reason_prop = replace_desc_reason (RProperty prop_name) reason_op in
          let error_message =
            if is_builtin_reason ALoc.source reason then
              Error_message.EBuiltinLookupFailed { reason = reason_prop; name = prop_name }
            else
              Error_message.EStrictLookupFailed
                {
                  reason_prop;
                  reason_obj = reason;
                  name = prop_name;
                  use_op = Some use_op;
                  suggestion = None;
                }
          in
          add_output cx ~trace error_message;
          AnyT.error reason_op
      in
      rec_flow_t cx trace ~use_op (fun_t, u)
    (* When something of type ShapeT(o) is used, it behaves like it had type o.
     *
     * On the other hand, things that can be passed to something of type
     * ShapeT(o) must be "subobjects" of o: they may have fewer properties, but
     * those properties should be transferable to o.
     *
     * Because a property x with a type OptionalT(t) could be considered
     * missing or having type t, we consider such a property to be transferable
     * if t is a subtype of x's type in o. Otherwise, the property should be
     * assignable to o.
     *
     * TODO: The type constructors ShapeT, ObjAssignToT/ObjAssignFromT,
     * ObjRestT express related meta-operations on objects. Consolidate these
     * meta-operations and ensure consistency of their semantics.
     *)
    | (ShapeT (r, o), _) ->
      rec_flow_t cx trace ~use_op (reposition cx ~trace (aloc_of_reason r) o, u)
    | (DefT (reason, _, ObjT ({ call_t = None; _ } as o)), ShapeT (_, proto)) ->
      let props = Context.find_real_props cx o.props_tmap in
      match_shape cx trace ~use_op proto reason props
    | (DefT (reason, _, InstanceT (_, _, _, ({ inst_call_t = None; _ } as i))), ShapeT (_, proto))
      ->
      let own_props = Context.find_props cx i.own_props in
      let proto_props = Context.find_props cx i.proto_props in
      let proto_props =
        match i.inst_kind with
        | InterfaceKind _ -> proto_props
        | ClassKind -> NameUtils.Map.remove (OrdinaryName "constructor") proto_props
      in
      let props = NameUtils.Map.union own_props proto_props in
      match_shape cx trace ~use_op proto reason props
    (* Function definitions are incompatible with ShapeT. ShapeT is meant to
     * match an object type with a subset of the props in the type being
     * destructured. It would be complicated and confusing to use a function for
     * this.
     *
     * This invariant is important for the React setState() type definition. *)
    | (_, ShapeT (_, o)) ->
      add_output
        cx
        ~trace
        (Error_message.EIncompatibleWithShape (reason_of_t l, reason_of_t o, use_op))
    (********************************************)
    (* array types deconstruct into their parts *)
    (********************************************)

    (* Arrays can flow to arrays *)
    | (DefT (r1, _, ArrT (ArrayAT (t1, ts1))), DefT (r2, _, ArrT (ArrayAT (t2, ts2)))) ->
      let use_op = Frame (ArrayElementCompatibility { lower = r1; upper = r2 }, use_op) in
      let lit1 = desc_of_reason r1 = RArrayLit in
      let ts1 = Base.Option.value ~default:[] ts1 in
      let ts2 = Base.Option.value ~default:[] ts2 in
      array_flow cx trace use_op lit1 r1 (ts1, t1, ts2, t2)
    (* Tuples can flow to tuples with the same arity *)
    | (DefT (r1, _, ArrT (TupleAT (_, ts1))), DefT (r2, _, ArrT (TupleAT (_, ts2)))) ->
      let fresh = desc_of_reason r1 = RArrayLit in
      let l1 = List.length ts1 in
      let l2 = List.length ts2 in
      if l1 <> l2 then
        add_output cx ~trace (Error_message.ETupleArityMismatch ((r1, r2), l1, l2, use_op));
      let n = ref 0 in
      iter2opt
        (fun t1 t2 ->
          match (t1, t2) with
          | (Some t1, Some t2) ->
            n := !n + 1;
            let use_op =
              Frame (TupleElementCompatibility { n = !n; lower = r1; upper = r2 }, use_op)
            in
            flow_to_mutable_child cx trace use_op fresh t1 t2
          | _ -> ())
        (ts1, ts2)
    (* Arrays with known elements can flow to tuples *)
    | (DefT (r1, trust, ArrT (ArrayAT (t1, ts1))), DefT (r2, _, ArrT (TupleAT _))) ->
      begin
        match ts1 with
        | None -> add_output cx ~trace (Error_message.ENonLitArrayToTuple ((r1, r2), use_op))
        | Some ts1 -> rec_flow_t cx trace ~use_op (DefT (r1, trust, ArrT (TupleAT (t1, ts1))), u)
      end
    (* Read only arrays are the super type of all tuples and arrays *)
    | ( DefT (r1, _, ArrT (ArrayAT (t1, _) | TupleAT (t1, _) | ROArrayAT t1)),
        DefT (r2, _, ArrT (ROArrayAT t2)) ) ->
      let use_op = Frame (ArrayElementCompatibility { lower = r1; upper = r2 }, use_op) in
      rec_flow cx trace (t1, UseT (use_op, t2))
    | (DefT (_, _, InstanceT _), DefT (r2, _, ArrT (ArrayAT (elemt, _)))) ->
      let arrt = get_builtin_typeapp cx ~trace r2 (OrdinaryName "Array") [elemt] in
      rec_flow cx trace (l, UseT (use_op, arrt))
    | (DefT (_, _, InstanceT _), DefT (r2, _, ArrT (ROArrayAT elemt))) ->
      let arrt = get_builtin_typeapp cx ~trace r2 (OrdinaryName "$ReadOnlyArray") [elemt] in
      rec_flow cx trace (l, UseT (use_op, arrt))
    (**************************************************)
    (* instances of classes follow declared hierarchy *)
    (**************************************************)
    | (DefT (_, _, InstanceT _), DefT (_, _, InstanceT _)) ->
      rec_flow cx trace (l, extends_use_type use_op l u)
    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)
    | (DefT (_, _, ClassT it), DefT (r, _, TypeT (_, t))) ->
      (* a class value annotation becomes the instance type *)
      rec_flow cx trace (it, BecomeT { reason = r; t; empty_success = true })
    | (DefT (_, _, TypeT (_, l)), DefT (_, _, TypeT (_, u))) ->
      rec_unify cx trace ~use_op ~unify_any:true l u
    | (DefT (lreason, trust, EnumObjectT enum), DefT (_r, _, TypeT (_, t))) ->
      (* an enum object value annotation becomes the enum type *)
      let enum_type = mk_enum_type ~trust lreason enum in
      rec_unify cx trace ~use_op enum_type t
    | (DefT (enum_reason, _, EnumT _), DefT (reason, _, TypeT _)) ->
      add_output cx ~trace Error_message.(EEnumMemberUsedAsType { reason; enum_reason })
    (* non-class/function values used in annotations are errors *)
    | (_, DefT (reason_use, _, TypeT _)) ->
      (match l with
      (* Short-circut as we already error on the unresolved name. *)
      | AnyT (_, AnyError (Some UnresolvedName)) -> ()
      | AnyT _ -> add_output cx ~trace Error_message.(EAnyValueUsedAsType { reason_use })
      | _ -> add_output cx ~trace Error_message.(EValueUsedAsType { reason_use }))
    | (DefT (rl, _, ClassT l), DefT (_, _, ClassT u)) ->
      rec_flow cx trace (reposition cx ~trace (aloc_of_reason rl) l, UseT (use_op, u))
    | ( DefT (_, _, FunT (static1, prototype, _)),
        DefT (_, _, ClassT (DefT (_, _, InstanceT (static2, _, _, _)))) ) ->
      rec_unify cx trace ~use_op static1 static2;
      rec_unify cx trace ~use_op prototype u
    (***********************************************)
    (* You can use a function as a callable object *)
    (***********************************************)
    | ( DefT (_, _, FunT _),
        DefT
          (_, _, (ObjT { call_t = Some id; _ } | InstanceT (_, _, _, { inst_call_t = Some id; _ })))
      ) ->
      let t = Context.find_call cx id in
      rec_flow cx trace (l, UseT (use_op, t))
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
    | ( DefT (lreason, _, FunT _),
        DefT (ureason, _, ObjT { flags = { obj_kind = (Exact | Indexed _) as obj_kind; _ }; _ }) )
      ->
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
    | (DefT (reason, _, FunT (statics, _, _)), DefT (reason_o, _, ObjT { props_tmap; _ })) ->
      if
        not
          (quick_error_fun_as_obj
             cx
             trace
             ~use_op
             reason
             statics
             reason_o
             (Context.find_props cx props_tmap))
      then
        rec_flow_t cx trace ~use_op (statics, u)
    (* TODO: similar concern as above *)
    | ( DefT (reason, _, FunT (statics, _, _)),
        DefT (reason_inst, _, InstanceT (_, _, _, { own_props; inst_kind = InterfaceKind _; _ })) )
      ->
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
                (Context.find_props cx own_props)))
      then
        rec_flow_t cx trace ~use_op (statics, u)
    (***************************************************************)
    (* Enable structural subtyping for upperbounds like interfaces *)
    (***************************************************************)
    | (DefT (reason_lower, _, (NullT | MixedT _ | VoidT)), DefT (reason_upper, _, InstanceT _)) ->
      add_output
        cx
        ~trace
        (Error_message.EIncompatibleWithUseOp { reason_lower; reason_upper; use_op });
      rec_flow_t cx trace ~use_op (AnyT.make (AnyError None) reason_lower, u)
    | (_, (DefT (_, _, InstanceT (_, _, _, { inst_kind = InterfaceKind _; _ })) as i)) ->
      rec_flow cx trace (i, ImplementsT (use_op, l))
    (* Opaque types may be treated as their supertype when they are a lower bound for a use *)
    | (OpaqueT (_, { super_t = Some t; _ }), _) -> rec_flow_t cx trace ~use_op (t, u)
    (***********************************************************)
    (* coercion                                                *)
    (***********************************************************)

    (* string and number can be coerced to strings *)
    | (DefT (_, _, NumT _), DefT (_, _, StrT _))
    (* TODO matching on the use_op seems wrong *)
      when match use_op with
           | Op (Coercion _) -> true
           | _ -> false ->
      ()
    (*********************)
    (* functions statics *)
    (*********************)
    | (DefT (reason, _, FunT (static, _, _)), AnyT _) ->
      rec_flow cx trace (static, ReposLowerT (reason, false, UseT (use_op, u)))
    (*****************)
    (* class statics *)
    (*****************)
    | (DefT (reason, _, ClassT instance), (DefT (_, _, ObjT _) | AnyT _)) ->
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
    | (DefT (reason, _, ClassT instance), DefT (_, _, FunT _)) ->
      let statics = (reason, Tvar.mk_no_wrap cx reason) in
      rec_flow cx trace (instance, GetStaticsT statics);
      rec_flow_t cx trace ~use_op (OpenT statics, u)
    | (DefT (_, _, EnumObjectT { enum_id = id1; _ }), DefT (_, _, EnumObjectT { enum_id = id2; _ }))
      when ALoc.equal_id id1 id2 ->
      ()
    | (DefT (_, _, EnumT { enum_id = id1; _ }), DefT (_, _, EnumT { enum_id = id2; _ }))
      when ALoc.equal_id id1 id2 ->
      ()
    | (DefT (enum_reason, _, EnumT { representation_t; _ }), t)
      when TypeUtil.quick_subtype (Context.trust_errors cx) representation_t t ->
      let representation_type =
        match representation_t with
        | DefT (_, _, BoolT _) -> Some "boolean"
        | DefT (_, _, NumT _) -> Some "number"
        | DefT (_, _, StrT _) -> Some "string"
        | DefT (_, _, SymbolT) -> Some "symbol"
        | _ -> None
      in
      add_output
        cx
        ~trace
        (Error_message.EEnumIncompatible
           { reason_lower = enum_reason; reason_upper = reason_of_t t; use_op; representation_type })
    | ( GenericT ({ bound = bound1; id = id1; reason = reason1; _ } as g1),
        GenericT ({ bound = bound2; id = id2; reason = reason2; _ } as g2) ) ->
      begin
        match Generic.satisfies ~printer:(print_if_verbose_lazy cx ~trace) id1 id2 with
        | Generic.Satisfied ->
          rec_flow_t
            cx
            trace
            ~use_op
            (position_generic_bound reason1 bound1, position_generic_bound reason2 bound2)
        | Generic.Lower id ->
          rec_flow_t
            cx
            trace
            ~use_op
            (GenericT { g1 with id }, position_generic_bound reason2 bound2)
        | Generic.Upper id ->
          rec_flow_t
            cx
            trace
            ~use_op
            (position_generic_bound reason1 bound1, GenericT { g2 with id })
      end
    | (GenericT { reason; bound; _ }, _) ->
      rec_flow_t cx trace ~use_op (position_generic_bound reason bound, u)
    | (_, GenericT { reason; name; _ }) ->
      let desc = RIncompatibleInstantiation name in
      let bot = DefT (replace_desc_reason desc reason, literal_trust (), EmptyT) in
      rec_flow_t cx trace ~use_op (l, bot)
    | (ObjProtoT reason, _) ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx ~trace reason ~use_desc (OrdinaryName "Object") in
      rec_flow_t cx trace ~use_op (obj_proto, u)
    | (_, ObjProtoT reason) ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx ~trace reason ~use_desc (OrdinaryName "Object") in
      rec_flow_t cx trace ~use_op (l, obj_proto)
    | (FunProtoT reason, _) ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx ~trace reason ~use_desc (OrdinaryName "Function") in
      rec_flow_t cx trace ~use_op (fun_proto, u)
    | (_, FunProtoT reason) ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx ~trace reason ~use_desc (OrdinaryName "Function") in
      rec_flow_t cx trace ~use_op (l, fun_proto)
    | (DefT (lreason, _, MixedT Mixed_function), DefT (ureason, _, FunT _)) ->
      add_output
        cx
        ~trace
        (Error_message.EIncompatible
           {
             lower = (lreason, None);
             upper = (ureason, Error_message.IncompatibleMixedCallT);
             use_op = Some use_op;
             branches = [];
           });
      rec_flow_t cx trace ~use_op (AnyT.make (AnyError None) lreason, u)
    | (FunProtoApplyT reason, _)
    | (FunProtoBindT reason, _)
    | (FunProtoCallT reason, _) ->
      rec_flow_t cx trace ~use_op (FunProtoT reason, u)
    | (_, _) ->
      add_output
        cx
        ~trace
        (Error_message.EIncompatibleWithUseOp
           { reason_lower = reason_of_t l; reason_upper = reason_of_t u; use_op })
end
