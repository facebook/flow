(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

module Kit (Flow : Flow_common.S) : Flow_common.CHECK_POLARITY = struct
  (* TODO: flesh this out *)
  let rec check_polarity cx ?trace polarity = function
    (* base case *)
    | BoundT (reason, name, tp_polarity) ->
      if not (Polarity.compat (tp_polarity, polarity)) then
        Flow.add_output
          cx
          ?trace
          (Error_message.EPolarityMismatch
             { reason; name; expected_polarity = tp_polarity; actual_polarity = polarity })
    | OpenT _
    | DefT (_, _, NumT _)
    | DefT (_, _, StrT _)
    | DefT (_, _, BoolT _)
    | DefT (_, _, EmptyT _)
    | DefT (_, _, MixedT _)
    | AnyT _
    | DefT (_, _, NullT)
    | DefT (_, _, VoidT)
    | DefT (_, _, SingletonStrT _)
    | DefT (_, _, SingletonNumT _)
    | DefT (_, _, SingletonBoolT _)
    | DefT (_, _, CharSetT _)
    | DefT (_, _, EnumObjectT _)
    | DefT (_, _, EnumT _) ->
      ()
    | ExistsT _ -> ()
    | InternalT (OptionalChainVoidT _) -> ()
    | OptionalT { type_ = t; _ }
    | ExactT (_, t)
    | MaybeT (_, t)
    | ReposT (_, t)
    | InternalT (ReposUpperT (_, t)) ->
      check_polarity cx ?trace polarity t
    | DefT (_, _, ClassT t) -> check_polarity cx ?trace polarity t
    | DefT (_, _, TypeT (_, t)) -> check_polarity cx ?trace polarity t
    | DefT (_, _, InstanceT (static, super, _, instance)) ->
      check_polarity cx ?trace polarity static;
      check_polarity cx ?trace polarity super;
      check_polarity_propmap cx ?trace polarity instance.own_props;
      check_polarity_propmap cx ?trace ~skip_ctor:true polarity instance.proto_props
    | DefT (_, _, FunT (_, _, func)) ->
      let f = check_polarity cx ?trace (Polarity.inv polarity) in
      List.iter (fun (_, t) -> f t) func.params;
      Option.iter ~f:(fun (_, _, t) -> f t) func.rest_param;
      check_polarity cx ?trace polarity func.return_t
    | DefT (_, _, ArrT (ArrayAT (elemt, _))) -> check_polarity cx ?trace Polarity.Neutral elemt
    | DefT (_, _, ArrT (TupleAT (_, tuple_types))) ->
      List.iter (check_polarity cx ?trace Polarity.Neutral) tuple_types
    | DefT (_, _, ArrT (ROArrayAT elemt)) -> check_polarity cx ?trace polarity elemt
    | DefT (_, _, ObjT obj) ->
      check_polarity_propmap cx ?trace polarity obj.props_tmap;
      (match obj.dict_t with
      | Some { key; value; dict_polarity; _ } ->
        check_polarity cx ?trace Polarity.Neutral key;
        check_polarity cx ?trace (Polarity.mult (polarity, dict_polarity)) value
      | None -> ())
    | DefT (_, _, IdxWrapper obj) -> check_polarity cx ?trace polarity obj
    | UnionT (_, rep) -> List.iter (check_polarity cx ?trace polarity) (UnionRep.members rep)
    | IntersectionT (_, rep) ->
      List.iter (check_polarity cx ?trace polarity) (InterRep.members rep)
    | DefT (_, _, PolyT { tparams = xs; t_out = t; _ }) ->
      Nel.iter (check_polarity_typeparam cx ?trace polarity) xs;
      check_polarity cx ?trace polarity t
    | ThisTypeAppT (_, c, _, None) -> check_polarity cx ?trace Polarity.Positive c
    | ThisTypeAppT (_, c, _, Some ts)
    | TypeAppT (_, _, c, ts) ->
      check_polarity cx ?trace Polarity.Positive c;
      check_polarity_typeapp cx ?trace polarity c ts
    | DefT (_, _, ReactAbstractComponentT { config; instance }) ->
      check_polarity cx ?trace Polarity.Negative config;
      check_polarity cx ?trace Polarity.Positive instance
    | OpaqueT (_, opaquetype) ->
      Option.iter ~f:(check_polarity cx ?trace polarity) opaquetype.underlying_t;
      Option.iter ~f:(check_polarity cx ?trace polarity) opaquetype.super_t
    | ShapeT t -> check_polarity cx ?trace polarity t
    | KeysT (_, t) -> check_polarity cx ?trace Polarity.Positive t
    | ThisClassT _
    | ModuleT _
    | AnnotT _
    | MatchingPropT _
    | NullProtoT _
    | ObjProtoT _
    | FunProtoT _
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _
    | EvalT _
    | InternalT (ExtendsT _)
    | InternalT (ChoiceKitT _)
    | TypeDestructorTriggerT _
    | CustomFunT _
    | OpenPredT _
    | MergedT _ ->
      (* TODO *)
      ()

  and check_polarity_propmap cx ?trace ?(skip_ctor = false) polarity id =
    let pmap = Context.find_props cx id in
    SMap.iter
      (fun x p ->
        if skip_ctor && x = "constructor" then
          ()
        else
          check_polarity_prop cx ?trace polarity p)
      pmap

  and check_polarity_prop cx ?trace polarity = function
    | Field (_, t, p) -> check_polarity cx ?trace (Polarity.mult (polarity, p)) t
    | Get (_, t) -> check_polarity cx ?trace polarity t
    | Set (_, t) -> check_polarity cx ?trace (Polarity.inv polarity) t
    | GetSet (_, t1, _, t2) ->
      check_polarity cx ?trace polarity t1;
      check_polarity cx ?trace (Polarity.inv polarity) t2
    | Method (_, t) -> check_polarity cx ?trace polarity t

  and check_polarity_typeparam cx ?trace polarity tp =
    let polarity = Polarity.mult (polarity, tp.polarity) in
    check_polarity cx ?trace polarity tp.bound;
    Option.iter ~f:(check_polarity cx ?trace polarity) tp.default

  and check_polarity_typeapp cx ?trace polarity c ts =
    let reason =
      Reason.update_desc_reason (fun desc -> Reason.RVarianceCheck desc) (reason_of_t c)
    in
    Flow.flow_opt cx ?trace (c, VarianceCheckT (reason, ts, polarity))
end
