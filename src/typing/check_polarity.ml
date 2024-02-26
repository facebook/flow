(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open TypeUtil

exception UnexpectedType of string

module Kit (Flow : Flow_common.S) : Flow_common.CHECK_POLARITY = struct
  (* TODO: flesh this out *)
  (* [seen] is the set of visited EvalT ids *)
  let rec check_polarity cx ?trace seen tparams polarity t =
    match t with
    (* base case *)
    | GenericT { reason; name; bound; _ } -> begin
      match Subst_name.Map.find_opt name tparams with
      | None -> check_polarity cx ?trace seen tparams polarity bound
      | Some tp ->
        if not (Polarity.compat (tp.polarity, polarity)) then
          Flow_js_utils.add_output
            cx
            ?trace
            (Error_message.EPolarityMismatch
               {
                 reason;
                 name = Subst_name.string_of_subst_name name;
                 expected_polarity = tp.polarity;
                 actual_polarity = polarity;
               }
            )
    end
    (* No need to walk into tvars, since we're looking for GenericT types, which
     * will certainly never appear in the bounds of a tvar. *)
    | OpenT _ -> ()
    (* The annot will resolve to some type, but it doesn't matter because that
     * type will certainly not contain a GenericT. *)
    | AnnotT _ -> ()
    | AnyT _
    | DefT (_, BoolT _)
    | DefT (_, BigIntT _)
    | DefT (_, CharSetT _)
    | DefT (_, EmptyT)
    | DefT (_, EnumObjectT _)
    | DefT (_, EnumT _)
    | DefT (_, MixedT _)
    | DefT (_, NullT)
    | DefT (_, NumT _)
    | DefT (_, NumericStrKeyT _)
    | DefT (_, SingletonBoolT _)
    | DefT (_, SingletonNumT _)
    | DefT (_, SingletonStrT _)
    | DefT (_, SingletonBigIntT _)
    | DefT (_, StrT _)
    | DefT (_, VoidT)
    | DefT (_, SymbolT)
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
      check_polarity cx ?trace seen tparams polarity t
    | DefT (_, ClassT t) -> check_polarity cx ?trace seen tparams polarity t
    | DefT (_, InstanceT { static; super; implements; inst }) ->
      let {
        class_id = _;
        class_name = _;
        type_args = _;
        own_props;
        proto_props;
        inst_call_t = call_t;
        initialized_fields = _;
        initialized_static_fields = _;
        inst_kind = _;
        inst_dict;
        class_private_fields;
        class_private_static_fields = _;
        class_private_methods;
        class_private_static_methods = _;
      } =
        inst
      in
      check_polarity cx ?trace seen tparams polarity static;
      check_polarity cx ?trace seen tparams polarity super;
      List.iter (check_polarity cx ?trace seen tparams polarity) implements;
      check_polarity_propmap cx ?trace seen tparams polarity own_props;
      check_polarity_propmap cx ?trace ~skip_ctor:true seen tparams polarity proto_props;
      check_polarity_propmap cx ?trace seen tparams polarity class_private_fields;
      check_polarity_propmap cx ?trace seen tparams polarity class_private_methods;
      Base.Option.iter call_t ~f:(check_polarity_call cx ?trace seen tparams polarity);
      Base.Option.iter inst_dict ~f:(check_polarity_dict cx ?trace seen tparams polarity)
    (* We can ignore the statics of function annotations, since
     * they will always be "uninteresting," never containing a GenericT. *)
    | DefT (_, FunT (_static, f)) ->
      let {
        (* Similarly, we can ignore this types, which can not be explicitly
         * provided, and thus will not contain a GenericT. *)
        this_t = _;
        params;
        rest_param;
        return_t;
        predicate;
        def_reason = _;
        hook = _;
      } =
        f
      in
      let check_inv = check_polarity cx ?trace seen tparams (Polarity.inv polarity) in
      List.iter (fun (_, t) -> check_inv t) params;
      Base.Option.iter ~f:(fun (_, _, t) -> check_inv t) rest_param;
      check_polarity cx ?trace seen tparams polarity return_t;
      (match predicate with
      | Some (TypeGuardBased { type_guard = t; _ }) ->
        check_polarity cx ?trace seen tparams polarity t
      | _ -> ())
    | DefT (_, ArrT (ArrayAT { elem_t = _; tuple_view = Some _; react_dro = _ })) as t ->
      (* This representation signifies a literal, which is not a type. *)
      raise (UnexpectedType (Debug_js.dump_t cx t))
    | DefT (_, ArrT (ArrayAT { elem_t; tuple_view = None; react_dro = _ })) ->
      check_polarity cx ?trace seen tparams Polarity.Neutral elem_t
    | DefT (_, ArrT (TupleAT { elements; _ })) ->
      List.iter
        (fun (TupleElement { t; polarity = p; name = _; optional = _; reason = _ }) ->
          check_polarity cx ?trace seen tparams (Polarity.mult (polarity, p)) t)
        elements
    | DefT (_, ArrT (ROArrayAT (t, _))) -> check_polarity cx ?trace seen tparams polarity t
    | DefT (_, ObjT o) ->
      let { flags; props_tmap; proto_t; call_t; reachable_targs = _ } = o in
      check_polarity_propmap cx ?trace seen tparams polarity props_tmap;
      let dict = Obj_type.get_dict_opt flags.obj_kind in
      Base.Option.iter dict ~f:(check_polarity_dict cx ?trace seen tparams polarity);
      check_polarity cx ?trace seen tparams polarity proto_t;
      Base.Option.iter call_t ~f:(check_polarity_call cx ?trace seen tparams polarity)
    | NamespaceT { values_type; types_tmap } ->
      check_polarity cx ?trace seen tparams polarity values_type;
      check_polarity_propmap cx ?trace seen tparams polarity types_tmap
    | UnionT (_, rep) ->
      List.iter (check_polarity cx ?trace seen tparams polarity) (UnionRep.members rep)
    | IntersectionT (_, rep) ->
      List.iter (check_polarity cx ?trace seen tparams polarity) (InterRep.members rep)
    | DefT (_, PolyT { tparams = tps; t_out = t; _ }) ->
      (* We might encounter a polymorphic function type or method inside of an
       * annotation. A newly introduced type parameter's bound or default might
       * refer to one of the tparams we're looking for. *)
      let tparams =
        Nel.fold_left
          (fun acc tp ->
            check_polarity_typeparam cx ?trace seen acc polarity tp;
            Subst_name.Map.add tp.name tp acc)
          tparams
          tps
      in
      check_polarity cx ?trace seen tparams polarity t
    | ThisTypeAppT (_, _, _, None) ->
      (* Perhaps surprisingly, there is nothing to do here. This type is used
       * specifically for the extends clause of a class declaration. The root
       * type of the extended class is looked up from the environment, and will
       * not contain any type parameters in scope -- only concrete types. *)
      ()
    | ThisTypeAppT (_, type_, _, Some targs)
    | TypeAppT { reason = _; use_op = _; type_; targs; from_value = _; use_desc = _ } ->
      (* Type arguments in a typeapp might contain a GenericT, but the root type
       * which defines the type parameters is not necessarily resolved at this
       * point. We need to know the polarity of the type parameters in order to
       * know the position of any found GenericTs. This constraint will continue
       * checking the type args once the root type is resolved. *)
      let reason = reason_of_t type_ in
      Flow.flow_opt cx ?trace (type_, VarianceCheckT (reason, tparams, targs, polarity))
    | DefT (_, ReactAbstractComponentT { config; instance; renders; component_kind = _ }) ->
      check_polarity cx ?trace seen tparams (Polarity.inv polarity) config;
      check_polarity cx ?trace seen tparams polarity instance;
      check_polarity cx ?trace seen tparams polarity renders
    | DefT (_, RendersT (NominalRenders { renders_id = _; renders_name = _; renders_super })) ->
      check_polarity cx ?trace seen tparams polarity renders_super
    | DefT (_, RendersT (StructuralRenders { renders_variant = _; renders_structural_type = t })) ->
      check_polarity cx ?trace seen tparams polarity t
    | KeysT (_, t) -> check_polarity cx ?trace seen tparams Polarity.Positive t
    | EvalT (t, TypeDestructorT (use_op, r, ReadOnlyType), id) ->
      if Eval.Set.mem id seen then
        ()
      else
        let out =
          Tvar.mk_no_wrap_where cx r (fun tvar ->
              let trace = Base.Option.value trace ~default:Trace.dummy_trace in
              Flow.eval_destructor cx ~trace use_op r t ReadOnlyType tvar
          )
        in
        let seen = Eval.Set.add id seen in
        Flow.possible_concrete_types_for_inspection cx r out
        |> Base.List.iter ~f:(check_polarity cx ?trace seen tparams Polarity.Positive)
    (* TODO *)
    | CustomFunT _
    | EvalT _ ->
      ()
    (* We only expect types which can appear in annotations. *)
    | (InternalT _ | DefT (_, TypeT _) | OpaqueT _ | ThisInstanceT _ | ModuleT _ | MatchingPropT _)
      as t ->
      raise (UnexpectedType (Debug_js.dump_t cx t))

  and check_polarity_propmap cx ?trace ?(skip_ctor = false) seen tparams polarity id =
    let pmap = Context.find_props cx id in
    NameUtils.Map.iter
      (fun x p ->
        if skip_ctor && x = Reason.OrdinaryName "constructor" then
          ()
        else
          check_polarity_prop cx ?trace seen tparams polarity p)
      pmap

  and check_polarity_prop cx ?trace seen tparams polarity = function
    | Field { type_; polarity = p; _ } ->
      check_polarity cx ?trace seen tparams (Polarity.mult (polarity, p)) type_
    | Get { type_; _ } -> check_polarity cx ?trace seen tparams polarity type_
    | Set { type_; _ } -> check_polarity cx ?trace seen tparams (Polarity.inv polarity) type_
    | GetSet { get_type; set_type; _ } ->
      check_polarity cx ?trace seen tparams polarity get_type;
      check_polarity cx ?trace seen tparams (Polarity.inv polarity) set_type
    | Method { type_; _ } -> check_polarity cx ?trace seen tparams polarity type_

  and check_polarity_dict cx ?trace seen tparams polarity d =
    let { dict_name = _; key; value; dict_polarity } = d in
    check_polarity cx ?trace seen tparams Polarity.Neutral key;
    check_polarity cx ?trace seen tparams (Polarity.mult (polarity, dict_polarity)) value

  and check_polarity_call cx ?trace seen tparams polarity id =
    let t = Context.find_call cx id in
    check_polarity cx ?trace seen tparams polarity t

  and check_polarity_typeparam cx ?trace seen tparams polarity tp =
    let { reason = _; name = _; bound; polarity = tp_polarity; default; is_this = _ } = tp in
    let check_mult =
      let polarity = Polarity.mult (polarity, tp_polarity) in
      check_polarity cx ?trace seen tparams polarity
    in
    check_mult bound;
    Base.Option.iter ~f:check_mult default

  let check_polarity cx ?trace tparams polarity t =
    check_polarity cx ?trace Eval.Set.empty tparams polarity t
end
