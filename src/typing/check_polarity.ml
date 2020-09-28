(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open TypeUtil

exception UnexpectedType of string

module Kit (Flow : Flow_common.S) : Flow_common.CHECK_POLARITY = struct
  (* TODO: flesh this out *)
  let rec check_polarity cx ?trace tparams polarity = function
    (* base case *)
    | BoundT (reason, name) ->
      begin
        match SMap.find_opt name tparams with
        | None -> ()
        | Some tp ->
          if not (Polarity.compat (tp.polarity, polarity)) then
            Flow.add_output
              cx
              ?trace
              (Error_message.EPolarityMismatch
                 { reason; name; expected_polarity = tp.polarity; actual_polarity = polarity })
      end
    (* No need to walk into tvars, since we're looking for BoundT types, which
     * will certainly never appear in the bounds of a tvar. *)
    | OpenT _ -> ()
    (* This type can appear in an annotation due to the $Pred type constructor,
     * but it won't contain a BoundT. *)
    | OpenPredT _ -> ()
    (* Even though we don't yet know what this type will become, we can at least
     * be certain that it won't be a BoundT. *)
    | ExistsT _ -> ()
    (* The annot will resolve to some type, but it doesn't matter because that
     * type will certainly not contain a BoundT. *)
    | AnnotT _ -> ()
    | AnyT _
    | DefT (_, _, BoolT _)
    | DefT (_, _, CharSetT _)
    | DefT (_, _, EmptyT _)
    | DefT (_, _, EnumObjectT _)
    | DefT (_, _, EnumT _)
    | DefT (_, _, MixedT _)
    | DefT (_, _, NullT)
    | DefT (_, _, NumT _)
    | DefT (_, _, SingletonBoolT _)
    | DefT (_, _, SingletonNumT _)
    | DefT (_, _, SingletonStrT _)
    | DefT (_, _, StrT _)
    | DefT (_, _, VoidT)
    | DefT (_, _, SymbolT)
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _
    | FunProtoT _
    | NullProtoT _
    | ObjProtoT _ ->
      ()
    | OptionalT { type_ = t; _ }
    | ExactT (_, t)
    | MaybeT (_, t) ->
      check_polarity cx ?trace tparams polarity t
    | DefT (_, _, ClassT t) -> check_polarity cx ?trace tparams polarity t
    | DefT (_, _, InstanceT (static, super, implements, i)) ->
      let {
        class_id = _;
        type_args = _;
        own_props;
        proto_props;
        inst_call_t = call_t;
        initialized_fields = _;
        initialized_static_fields = _;
        has_unknown_react_mixins = _;
        inst_kind = _;
      } =
        i
      in
      check_polarity cx ?trace tparams polarity static;
      check_polarity cx ?trace tparams polarity super;
      List.iter (check_polarity cx ?trace tparams polarity) implements;
      check_polarity_propmap cx ?trace tparams polarity own_props;
      check_polarity_propmap cx ?trace ~skip_ctor:true tparams polarity proto_props;
      Base.Option.iter call_t ~f:(check_polarity_call cx ?trace tparams polarity)
    (* We can ignore the statics and prototype of function annotations, since
     * they will always be "uninteresting," never containing a BoundT. *)
    | DefT (_, _, FunT (_static, _prototype, f)) ->
      let {
        (* Similarly, we can ignore this types, which can not be explicitly
         * provided, and thus will not contain a BoundT. *)
        this_t = _;
        params;
        rest_param;
        return_t;
        closure_t = _;
        is_predicate = _;
        changeset = _;
        def_reason = _;
      } =
        f
      in
      let check_inv = check_polarity cx ?trace tparams (Polarity.inv polarity) in
      List.iter (fun (_, t) -> check_inv t) params;
      Base.Option.iter ~f:(fun (_, _, t) -> check_inv t) rest_param;
      check_polarity cx ?trace tparams polarity return_t
    | DefT (_, _, ArrT (ArrayAT (_, Some _))) as t ->
      (* This representation signifies a literal, which is not a type. *)
      raise (UnexpectedType (Debug_js.dump_t cx t))
    | DefT (_, _, ArrT (ArrayAT (t, None))) -> check_polarity cx ?trace tparams Polarity.Neutral t
    | DefT (_, _, ArrT (TupleAT (_, ts))) ->
      List.iter (check_polarity cx ?trace tparams Polarity.Neutral) ts
    | DefT (_, _, ArrT (ROArrayAT t)) -> check_polarity cx ?trace tparams polarity t
    | DefT (_, _, ObjT o) ->
      let { flags; props_tmap; proto_t; call_t } = o in
      check_polarity_propmap cx ?trace tparams polarity props_tmap;
      let dict = Obj_type.get_dict_opt flags.obj_kind in
      Base.Option.iter dict ~f:(check_polarity_dict cx ?trace tparams polarity);
      check_polarity cx ?trace tparams polarity proto_t;
      Base.Option.iter call_t ~f:(check_polarity_call cx ?trace tparams polarity)
    | UnionT (_, rep) ->
      List.iter (check_polarity cx ?trace tparams polarity) (UnionRep.members rep)
    | IntersectionT (_, rep) ->
      List.iter (check_polarity cx ?trace tparams polarity) (InterRep.members rep)
    | DefT (_, _, PolyT { tparams = tps; t_out = t; _ }) ->
      (* We might encounter a polymorphic function type or method inside of an
       * annotation. A newly introduced type parameter's bound or default might
       * refer to one of the tparams we're looking for. *)
      let tparams =
        Nel.fold_left
          (fun acc tp ->
            check_polarity_typeparam cx ?trace acc polarity tp;
            SMap.add tp.name tp acc)
          tparams
          tps
      in
      check_polarity cx ?trace tparams polarity t
    | ThisTypeAppT (_, _, _, None) ->
      (* Perhaps surprisingly, there is nothing to do here. This type is used
       * specifically for the extends clause of a class declaration. The root
       * type of the extended class is looked up from the environment, and will
       * not contain any type parameters in scope -- only concrete types. *)
      ()
    | ThisTypeAppT (_, c, _, Some targs)
    | TypeAppT (_, _, c, targs) ->
      (* Type arguments in a typeapp might contain a BoundT, but the root type
       * which defines the type parameters is not necessarily resolved at this
       * point. We need to know the polarity of the type parameters in order to
       * know the position of any found BoundTs. This constraint will continue
       * checking the type args once the root type is resolved. *)
      let reason = reason_of_t c in
      Flow.flow_opt cx ?trace (c, VarianceCheckT (reason, tparams, targs, polarity))
    | DefT (_, _, ReactAbstractComponentT { config; instance }) ->
      check_polarity cx ?trace tparams (Polarity.inv polarity) config;
      check_polarity cx ?trace tparams polarity instance
    | ShapeT (_, t) -> check_polarity cx ?trace tparams polarity t
    | KeysT (_, t) -> check_polarity cx ?trace tparams Polarity.Positive t
    (* TODO *)
    | CustomFunT _
    | EvalT _ ->
      ()
    (* We only expect types which can appear in annotations. *)
    | ( InternalT _ | ReposT _
      | DefT (_, _, TypeT _)
      | DefT (_, _, IdxWrapper _)
      | OpaqueT _ | ThisClassT _ | ModuleT _ | MatchingPropT _ | TypeDestructorTriggerT _
      | MergedT _ ) as t ->
      raise (UnexpectedType (Debug_js.dump_t cx t))

  and check_polarity_propmap cx ?trace ?(skip_ctor = false) tparams polarity id =
    let pmap = Context.find_props cx id in
    SMap.iter
      (fun x p ->
        if skip_ctor && x = "constructor" then
          ()
        else
          check_polarity_prop cx ?trace tparams polarity p)
      pmap

  and check_polarity_prop cx ?trace tparams polarity = function
    | Field (_, t, p) -> check_polarity cx ?trace tparams (Polarity.mult (polarity, p)) t
    | Get (_, t) -> check_polarity cx ?trace tparams polarity t
    | Set (_, t) -> check_polarity cx ?trace tparams (Polarity.inv polarity) t
    | GetSet (_, t1, _, t2) ->
      check_polarity cx ?trace tparams polarity t1;
      check_polarity cx ?trace tparams (Polarity.inv polarity) t2
    | Method (_, t) -> check_polarity cx ?trace tparams polarity t

  and check_polarity_dict cx ?trace tparams polarity d =
    let { dict_name = _; key; value; dict_polarity } = d in
    check_polarity cx ?trace tparams Polarity.Neutral key;
    check_polarity cx ?trace tparams (Polarity.mult (polarity, dict_polarity)) value

  and check_polarity_call cx ?trace tparams polarity id =
    let t = Context.find_call cx id in
    check_polarity cx ?trace tparams polarity t

  and check_polarity_typeparam cx ?trace tparams polarity tp =
    let { reason = _; name = _; bound; polarity = tp_polarity; default } = tp in
    let check_mult =
      let polarity = Polarity.mult (polarity, tp_polarity) in
      check_polarity cx ?trace tparams polarity
    in
    check_mult bound;
    Base.Option.iter ~f:check_mult default
end
