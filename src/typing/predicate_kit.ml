(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open TypeUtil
open Reason
open Flow_js_utils
module SpeculationKit = Speculation_kit.Make (Flow_js.FlowJs)
open Flow_js.FlowJs

type instanceof_rhs =
  | TypeOperand of Type.t
  | InternalExtendsOperand of reason * Type.t * Type.t

type prop_guard =
  | PropGuardTruthy
  | PropGuardNotTruthy
  | PropGuardMaybe
  | PropGuardNotMaybe
  | PropGuardNull
  | PropGuardNotNull
  | PropGuardVoid
  | PropGuardNotVoid

let concretization_variant_of_predicate = function
  | MaybeP
  | NotP MaybeP
  | TruthyP
  | NotP TruthyP
  | LatentP _
  | NotP (LatentP _) ->
    ConcretizeKeepOptimizedUnions
  | _ -> ConcretizeForGeneralPredicateTest

type predicate_result_mut =
  | PredicateResultCollector of {
      collector: TypeCollector.t;
      changed: bool ref;
    }

(* Simple wrapper to protect against using non-concretized types in type-guard
 * filtering operations (see `intersect` below). *)
module ConcretizedType : sig
  type t

  val for_all_concrete_ts : Context.t -> f:(t -> bool) -> Type.t -> bool

  val unwrap : t -> Type.t

  val wrap_unsafe : Type.t -> t
end = struct
  type t = C of Type.t

  let unwrap (C t) = t

  let for_all_concrete_ts cx ~f t =
    possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t
    |> Base.List.for_all ~f:(fun t -> f (C t))

  let wrap_unsafe t = C t
end

let report_changes_to_input (PredicateResultCollector { collector = _; changed }) = changed := true

let report_filtering_result_to_predicate_result
    (Type_filter.TypeFilterResult { type_; changed })
    (PredicateResultCollector { collector; changed = changed_ref }) =
  TypeCollector.add collector type_;
  changed_ref := !changed_ref || changed

let report_unchanged_filtering_result_to_predicate_result type_ =
  report_filtering_result_to_predicate_result
    (Type_filter.TypeFilterResult { type_; changed = false })

let report_changed_filtering_result_to_predicate_result type_ =
  report_filtering_result_to_predicate_result
    (Type_filter.TypeFilterResult { type_; changed = true })

let rec concretize_and_run_predicate
    cx trace l variant result_collector ~predicate_no_concretization =
  let reason = reason_of_t l in
  possible_concrete_types_for_predicate cx reason ~predicate_concretizer_variant:variant l
  |> Base.List.iter ~f:(function
         | GenericT { bound; name; reason; id; no_infer } ->
           let bound_type_collector = TypeCollector.create () in
           let changed = ref false in
           let bound_result_collector =
             PredicateResultCollector { collector = bound_type_collector; changed }
           in
           concretize_and_run_predicate
             cx
             trace
             (reposition_reason cx reason bound)
             variant
             bound_result_collector
             ~predicate_no_concretization;
           let changed = !changed in
           if changed then report_changes_to_input result_collector;
           TypeCollector.iter bound_type_collector ~f:(fun bound ->
               let type_ = GenericT { reason = reason_of_t bound; name; bound; no_infer; id } in
               report_filtering_result_to_predicate_result
                 (Type_filter.TypeFilterResult { type_; changed })
                 result_collector
           )
         | l -> predicate_no_concretization cx trace result_collector l
         )

and concretize_binary_rhs_and_run_binary_predicate cx trace l r sense b tvar =
  let reason = reason_of_t r in
  let variant =
    match b with
    | InstanceofTest -> ConcretizeRHSForInstanceOfPredicateTest
    | SentinelProp _ -> ConcretizeRHSForLiteralPredicateTest
    | EqTest -> ConcretizeRHSForLiteralPredicateTest
  in
  possible_concrete_types_for_predicate cx reason ~predicate_concretizer_variant:variant r
  |> Base.List.iter ~f:(fun r -> binary_predicate cx trace sense b l r tvar)

(* t - predicate output recipient (normally a tvar)
   l - incoming concrete LB (predicate input)
   result - guard result in case of success
   p - predicate *)
and predicate_no_concretization cx trace result_collector l ~p =
  match p with
  (************************)
  (* deconstruction of && *)
  (************************)
  | AndP (p1, p2) ->
    let intermediate_type_collector = TypeCollector.create () in
    let changed = ref false in
    let intermediate_result_collector =
      PredicateResultCollector { collector = intermediate_type_collector; changed }
    in
    concretize_and_run_predicate
      cx
      trace
      l
      (concretization_variant_of_predicate p1)
      intermediate_result_collector
      ~predicate_no_concretization:(predicate_no_concretization ~p:p1);
    if !changed then report_changes_to_input result_collector;
    TypeCollector.iter intermediate_type_collector ~f:(fun t ->
        concretize_and_run_predicate
          cx
          trace
          t
          (concretization_variant_of_predicate p2)
          result_collector
          ~predicate_no_concretization:(predicate_no_concretization ~p:p2)
    )
  (************************)
  (* deconstruction of || *)
  (************************)
  | OrP (p1, p2) ->
    concretize_and_run_predicate
      cx
      trace
      l
      (concretization_variant_of_predicate p1)
      result_collector
      ~predicate_no_concretization:(predicate_no_concretization ~p:p1);
    concretize_and_run_predicate
      cx
      trace
      l
      (concretization_variant_of_predicate p2)
      result_collector
      ~predicate_no_concretization:(predicate_no_concretization ~p:p2)
  (*********************************)
  (* deconstruction of binary test *)
  (*********************************)
  | BinaryP (b, r) ->
    concretize_binary_rhs_and_run_binary_predicate cx trace l r true b result_collector
  | NotP (BinaryP (b, r)) ->
    concretize_binary_rhs_and_run_binary_predicate cx trace l r false b result_collector
  (***********************)
  (* typeof _ ~ "boolean" *)
  (***********************)
  | BoolP loc ->
    report_filtering_result_to_predicate_result (Type_filter.boolean loc l) result_collector
  | NotP (BoolP _) ->
    report_filtering_result_to_predicate_result (Type_filter.not_boolean l) result_collector
  (***********************)
  (* typeof _ ~ "string" *)
  (***********************)
  | StrP loc ->
    report_filtering_result_to_predicate_result (Type_filter.string loc l) result_collector
  | NotP (StrP _) ->
    report_filtering_result_to_predicate_result (Type_filter.not_string l) result_collector
  (***********************)
  (* typeof _ ~ "symbol" *)
  (***********************)
  | SymbolP loc ->
    report_filtering_result_to_predicate_result (Type_filter.symbol loc l) result_collector
  | NotP (SymbolP _) ->
    report_filtering_result_to_predicate_result (Type_filter.not_symbol l) result_collector
  (*********************)
  (* _ ~ "some string" *)
  (*********************)
  | SingletonStrP (expected_loc, _sense, lit) ->
    let filtered_str = Type_filter.string_literal expected_loc (OrdinaryName lit) l in
    report_filtering_result_to_predicate_result filtered_str result_collector
  | NotP (SingletonStrP (_, _, lit)) ->
    let filtered_str = Type_filter.not_string_literal (OrdinaryName lit) l in
    report_filtering_result_to_predicate_result filtered_str result_collector
  (*********************)
  (* _ ~ some number n *)
  (*********************)
  | SingletonNumP (expected_loc, _sense, lit) ->
    let filtered_num = Type_filter.number_literal expected_loc lit l in
    report_filtering_result_to_predicate_result filtered_num result_collector
  | NotP (SingletonNumP (_, _, lit)) ->
    let filtered_num = Type_filter.not_number_literal lit l in
    report_filtering_result_to_predicate_result filtered_num result_collector
  (***********************)
  (* typeof _ ~ "number" *)
  (***********************)
  | NumP loc ->
    report_filtering_result_to_predicate_result (Type_filter.number loc l) result_collector
  | NotP (NumP _) ->
    report_filtering_result_to_predicate_result (Type_filter.not_number l) result_collector
  (*********************)
  (* _ ~ some bigint n *)
  (*********************)
  | SingletonBigIntP (expected_loc, _sense, lit) ->
    let filtered_bigint = Type_filter.bigint_literal expected_loc lit l in
    report_filtering_result_to_predicate_result filtered_bigint result_collector
  | NotP (SingletonBigIntP (_, _, lit)) ->
    let filtered_bigint = Type_filter.not_bigint_literal lit l in
    report_filtering_result_to_predicate_result filtered_bigint result_collector
  (***********************)
  (* typeof _ ~ "bigint" *)
  (***********************)
  | BigIntP loc ->
    report_filtering_result_to_predicate_result (Type_filter.bigint loc l) result_collector
  | NotP (BigIntP _) ->
    report_filtering_result_to_predicate_result (Type_filter.not_bigint l) result_collector
  (***********************)
  (* typeof _ ~ "function" *)
  (***********************)
  | FunP -> report_filtering_result_to_predicate_result (Type_filter.function_ l) result_collector
  | NotP FunP ->
    report_filtering_result_to_predicate_result (Type_filter.not_function l) result_collector
  (***********************)
  (* typeof _ ~ "object" *)
  (***********************)
  | ObjP -> report_filtering_result_to_predicate_result (Type_filter.object_ cx l) result_collector
  | NotP ObjP ->
    report_filtering_result_to_predicate_result (Type_filter.not_object l) result_collector
  (*******************)
  (* Array.isArray _ *)
  (*******************)
  | ArrP -> report_filtering_result_to_predicate_result (Type_filter.array l) result_collector
  | NotP ArrP ->
    report_filtering_result_to_predicate_result (Type_filter.not_array l) result_collector
  (*******************)
  (* array length *)
  (*******************)
  | ArrLenP { op; n } ->
    report_filtering_result_to_predicate_result
      (Type_filter.array_length ~sense:true ~op ~n l)
      result_collector
  | NotP (ArrLenP { op; n }) ->
    report_filtering_result_to_predicate_result
      (Type_filter.array_length ~sense:false ~op ~n l)
      result_collector
  (***********************)
  (* typeof _ ~ "undefined" *)
  (***********************)
  | VoidP ->
    let filtered = Type_filter.undefined l in
    report_filtering_result_to_predicate_result filtered result_collector
  | NotP VoidP ->
    let filtered = Type_filter.not_undefined cx l in
    report_filtering_result_to_predicate_result filtered result_collector
  (********)
  (* null *)
  (********)
  | NullP ->
    let filtered = Type_filter.null l in
    report_filtering_result_to_predicate_result filtered result_collector
  | NotP NullP ->
    let filtered = Type_filter.not_null cx l in
    report_filtering_result_to_predicate_result filtered result_collector
  (*********)
  (* maybe *)
  (*********)
  | MaybeP ->
    let filtered = Type_filter.maybe cx l in
    report_filtering_result_to_predicate_result filtered result_collector
  | NotP MaybeP ->
    let filtered = Type_filter.not_maybe cx l in
    report_filtering_result_to_predicate_result filtered result_collector
  (********)
  (* true *)
  (********)
  | SingletonBoolP (_, true) ->
    let filtered = Type_filter.true_ l in
    report_filtering_result_to_predicate_result filtered result_collector
  | NotP (SingletonBoolP (_, true)) ->
    let filtered = Type_filter.not_true l in
    report_filtering_result_to_predicate_result filtered result_collector
  (*********)
  (* false *)
  (*********)
  | SingletonBoolP (_, false) ->
    let filtered = Type_filter.false_ l in
    report_filtering_result_to_predicate_result filtered result_collector
  | NotP (SingletonBoolP (_, false)) ->
    let filtered = Type_filter.not_false l in
    report_filtering_result_to_predicate_result filtered result_collector
  (************************)
  (* truthyness *)
  (************************)
  | TruthyP ->
    let filtered = Type_filter.truthy cx l in
    report_filtering_result_to_predicate_result filtered result_collector
  | NotP TruthyP ->
    let filtered = Type_filter.not_truthy cx l in
    report_filtering_result_to_predicate_result filtered result_collector
  | PropExistsP { propname; reason = _ } -> prop_exists_test cx propname true l result_collector
  | NotP (PropExistsP { propname; reason = _ }) ->
    prop_exists_test cx propname false l result_collector
  | PropTruthyP (key, r) -> prop_truthy_test cx trace key r true l result_collector
  | NotP (PropTruthyP (key, r)) -> prop_truthy_test cx trace key r false l result_collector
  | PropNonMaybeP (key, r) -> prop_non_maybe_test cx trace key r true l result_collector
  | NotP (PropNonMaybeP (key, r)) -> prop_non_maybe_test cx trace key r false l result_collector
  | PropIsExactlyNullP (key, r) -> prop_is_exactly_null_test cx trace key r true l result_collector
  | NotP (PropIsExactlyNullP (key, r)) ->
    prop_is_exactly_null_test cx trace key r false l result_collector
  | PropNonVoidP (key, r) -> prop_non_void_test cx trace key r true l result_collector
  | NotP (PropNonVoidP (key, r)) -> prop_non_void_test cx trace key r false l result_collector
  (* classical logic i guess *)
  | NotP (NotP p) -> predicate_no_concretization cx trace result_collector l ~p
  | NotP (AndP (p1, p2)) ->
    predicate_no_concretization cx trace result_collector l ~p:(OrP (NotP p1, NotP p2))
  | NotP (OrP (p1, p2)) ->
    predicate_no_concretization cx trace result_collector l ~p:(AndP (NotP p1, NotP p2))
  (********************)
  (* Latent predicate *)
  (********************)
  | LatentP ((lazy (use_op, loc, fun_t, targs, argts)), idx) ->
    let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
    call_latent_param_pred
      cx
      trace
      fun_t
      ~use_op
      ~reason
      ~targs
      ~argts
      ~sense:true
      ~idx
      l
      result_collector
  | NotP (LatentP ((lazy (use_op, loc, fun_t, targs, argts)), idx)) ->
    let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
    call_latent_param_pred
      cx
      trace
      fun_t
      ~use_op
      ~reason
      ~targs
      ~argts
      ~sense:false
      ~idx
      l
      result_collector
  | LatentThisP (lazy (use_op, loc, fun_t, targs, argts)) ->
    let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
    call_latent_this_pred
      cx
      trace
      fun_t
      ~use_op
      ~reason
      ~targs
      ~argts
      ~sense:true
      l
      result_collector
  | NotP (LatentThisP (lazy (use_op, loc, fun_t, targs, argts))) ->
    let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
    call_latent_this_pred
      cx
      trace
      fun_t
      ~use_op
      ~reason
      ~targs
      ~argts
      ~sense:false
      l
      result_collector
  (**************)
  (* Impossible *)
  (**************)
  | NotP ImpossibleP ->
    report_filtering_result_to_predicate_result (Type_filter.unchanged_result l) result_collector
  | ImpossibleP ->
    report_filtering_result_to_predicate_result (Type_filter.empty l) result_collector

(* call_latent_pred connects a predicate function with information available
 * at a call-site appearing in a conditional position (e.g. `if (pred(x))`).
 * [tin] is the incoming type of `x` and [tout] the refined result in the then-
 * branch. Since at the time of processing the call we do not know yet the
 * function's formal parameters, [idx] is the index of the argument that gets
 * refined. *)
and call_latent_pred
    cx trace fun_t ~use_op ~reason ~targs ~argts ~sense ~is_target tin result_collector =
  let ts = possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t fun_t) fun_t in
  Base.List.iter ts ~f:(function
      | IntersectionT (r, rep) ->
        Base.List.map (InterRep.members rep) ~f:(fun t () ->
            call_latent_pred
              cx
              trace
              t
              ~use_op
              ~reason
              ~targs
              ~argts
              ~sense
              ~is_target
              tin
              result_collector
        )
        |> SpeculationKit.try_custom cx ~use_op ~no_match_error_loc:(loc_of_reason r)
      (* Calls to functions appearing in predicate refinement contexts dispatch
          to this case. Here, the callee function type holds the predicate
          that will refine the incoming `unrefined_t` and flow a filtered
          (refined) version of this type into `fresh_t`.

          Problematic cases (e.g. when the refining index is out of bounds w.r.t.
          `params`) raise errors, but also propagate the unrefined types (as if the
          refinement never took place).
      *)
      | DefT
          ( _,
            FunT
              ( _,
                {
                  params;
                  type_guard =
                    Some (TypeGuard { one_sided; param_name = (_, param_name); type_guard; _ });
                  _;
                }
              )
          ) -> begin
        (* TODO: for the moment we only support simple keys (empty projection)
           that exactly correspond to the function's parameters *)
        if is_target param_name params then
          let filter_result =
            if sense then
              let type_ = intersect cx tin (reposition_reason cx ~trace reason type_guard) in
              Type_filter.TypeFilterResult { type_; changed = type_ != tin }
            else if not one_sided then
              type_guard_diff cx tin (reposition_reason cx ~trace reason type_guard)
            else
              (* Do not refine else branch on one-sided type-guard *)
              Type_filter.TypeFilterResult { type_ = tin; changed = false }
          in
          report_filtering_result_to_predicate_result filter_result result_collector
        else
          (* This is not the refined parameter. *)
          report_unchanged_filtering_result_to_predicate_result tin result_collector
      end
      | DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }) as fun_t ->
        let tvar = (reason, Tvar.mk_no_wrap cx reason) in
        let calltype = mk_functioncalltype reason targs argts tvar in
        let check =
          lazy
            (Implicit_instantiation_check.of_call
               fun_t
               (tparams_loc, ids, t)
               unknown_use
               reason
               calltype
            )
        in
        let lparts = (reason_tapp, tparams_loc, ids, t) in
        let uparts = (use_op, reason, calltype.call_targs, Type.hint_unavailable) in
        let t_ = instantiate_poly_call_or_new cx trace lparts uparts check in
        call_latent_pred
          cx
          trace
          t_
          ~use_op
          ~reason
          ~targs:None
          ~argts
          ~sense
          ~is_target
          tin
          result_collector
      (* Fall through all the remaining cases *)
      | _ -> report_unchanged_filtering_result_to_predicate_result tin result_collector
      )

and call_latent_param_pred ~idx =
  call_latent_pred ~is_target:(fun type_guard_ame params ->
      Base.List.exists idx ~f:(fun i ->
          match Base.List.nth params i with
          | Some (Some name, _) -> name = type_guard_ame
          | _ -> false
      )
  )

and call_latent_this_pred =
  call_latent_pred ~is_target:(fun type_guard_name _params -> type_guard_name = "this")

(* This utility is expected to be used when we a variable of type [t1] is refined
 * with the use of a type guard function with type `(x: mixed) => x is t2`.
 * t1 is already concretized by the time it reaches this point. Type t2, on the
 * other hand, is not, since it is coming directly from the annotation. This is why
 * we concretize it first, before attempting any comparisons. *)
and intersect =
  let module C = ConcretizedType in
  let is_any cx t =
    let ts = possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t in
    Base.List.exists ts ~f:(function
        | AnyT _ -> true
        | _ -> false
        )
  in
  let is_null t =
    match C.unwrap t with
    | DefT (_, NullT) -> true
    | _ -> false
  in
  let is_void t =
    match C.unwrap t with
    | DefT (_, VoidT) -> true
    | _ -> false
  in
  let tags_of_t cx t = Type_filter.tag_of_t cx (C.unwrap t) in
  let tags_differ cx t1 t2 =
    match (tags_of_t cx t1, tags_of_t cx t2) with
    | (Some t1_tags, Some t2_tags) -> not (Type_filter.tags_overlap t1_tags t2_tags)
    | _ -> false
  in
  (* Singleton primitive types are not captured in the tag overlap checks. We are
   * handling a few cases here explicitly. *)
  let ground_types_differ t1 t2 =
    match (C.unwrap t1, C.unwrap t2) with
    | (DefT (_, SingletonStrT { value = v1; _ }), DefT (_, SingletonStrT { value = v2; _ })) ->
      v1 <> v2
    | (DefT (_, SingletonNumT { value = v1; _ }), DefT (_, SingletonNumT { value = v2; _ })) ->
      v1 <> v2
    | (DefT (_, SingletonBoolT { value = v1; _ }), DefT (_, SingletonBoolT { value = v2; _ })) ->
      v1 <> v2
    | (_, _) -> false
  in
  let rec type_tags_differ cx ~depth = function
    | (t1 :: rest1, t2 :: rest2) ->
      C.for_all_concrete_ts cx t1 ~f:(fun t1 -> types_differ cx ~depth t1 t2)
      || type_tags_differ cx ~depth (rest1, rest2)
    | _ -> false
  (* C<T> has no overlap with C<S> iff T and S have no overlap *)
  and instance_tags_differ cx ~depth t1 t2 =
    match (C.unwrap t1, C.unwrap t2) with
    | ( DefT (_, InstanceT { inst = { inst_kind = ClassKind; type_args = ts1; _ } as inst1; _ }),
        DefT (_, InstanceT { inst = { inst_kind = ClassKind; type_args = ts2; _ } as inst2; _ })
      )
      when Flow_js_utils.is_same_instance_type inst1 inst2 ->
      let ts1 = Base.List.map ts1 ~f:(fun (_, _, t, _) -> t) in
      let ts2 = Base.List.map ts2 ~f:(fun (_, _, t, _) -> t) in
      type_tags_differ cx ~depth (ts1, ts2)
    | _ -> false
  and types_differ cx ~depth t1 t2 =
    (* To prevent infinite recursion, we use a simple depth mechanism. *)
    if depth > 2 then
      false
    else
      let depth = depth + 1 in
      C.for_all_concrete_ts cx t2 ~f:(fun t2 ->
          tags_differ cx t1 t2 || ground_types_differ t1 t2 || instance_tags_differ cx ~depth t1 t2
      )
  in
  let try_intersect cx reason1 t1_conc t2 =
    let t1 = C.unwrap t1_conc in
    if types_differ cx ~depth:0 t1_conc t2 then
      let r = update_desc_reason invalidate_rtype_alias reason1 in
      Some (DefT (r, EmptyT))
    else if is_any cx t1 then
      Some t2
    else if is_any cx t2 then
      (* Filter out null and void types from the input if comparing with any *)
      if is_null (C.wrap_unsafe t1) || is_void (C.wrap_unsafe t1) then
        let r = update_desc_reason invalidate_rtype_alias reason1 in
        Some (DefT (r, EmptyT))
      else
        Some t1
    else if TypeUtil.quick_subtype t1 t2 || speculative_subtyping_succeeds cx t1 t2 then
      Some t1
    else if TypeUtil.quick_subtype t2 t1 || speculative_subtyping_succeeds cx t2 t1 then
      Some t2
    else
      match t1 with
      | OpaqueT (r, ({ upper_t; underlying_t; _ } as opaquetype)) ->
        (* Apply the refinement on super and underlying type of opaque type.
         * Preserve opaque_id to retain compatibility with original type. *)
        let upper_t =
          Some (Base.Option.value_map upper_t ~default:t2 ~f:(fun t -> intersect cx t t2))
        in
        let underlying_t = Base.Option.map ~f:(fun t -> intersect cx t t2) underlying_t in
        Some (OpaqueT (r, { opaquetype with underlying_t; upper_t }))
      | _ -> None
  in
  fun cx t1 t2 ->
    let reason1 = TypeUtil.reason_of_t t1 in
    (* Pre-processing of t1 has concretized it up to optimized unions. It is
     * important to keep it this way to prevent expensive checks right away.
     * Consider for example the code:
     *
     *   declare var x: T;
     *   declare var foo: (x: mixed) => x is T
     *   if (foo(x)) {}
     *
     * where T is a really large enum-like union. `try_intersect` will quickly
     * return `t1` as the result here, without us having to try to fully
     * concretize `t1`.
     *)
    (* Input t1 is already concretized as input to the predicate mechanism *)
    match try_intersect cx reason1 (C.wrap_unsafe t1) t2 with
    | Some t -> t
    | None ->
      (* No definitive refinement found. We fall back to more expensive
       * concretization that breaks up all unions (including optimized ones). *)
      possible_concrete_types_for_inspection cx reason1 t1
      |> Base.List.map ~f:(fun t1 ->
             (* t1 was just concretized *)
             match try_intersect cx reason1 (C.wrap_unsafe t1) t2 with
             | Some t -> t
             | None ->
               let r = update_desc_reason invalidate_rtype_alias reason1 in
               IntersectionT (r, InterRep.make t2 t1 [])
         )
      |> TypeUtil.union_of_ts (update_desc_reason invalidate_rtype_alias reason1)

(* This utility is expected to be used when negating the refinement of a type [t1]
 * with a type guard `x is t2`. The only case considered here is that of t1 <: t2.
 * This means that the positive branch will always be taken, and so we are left with
 * `empty` in the negated case. *)
and type_guard_diff cx t1 t2 =
  let reason1 = TypeUtil.reason_of_t t1 in
  if TypeUtil.quick_subtype t1 t2 || speculative_subtyping_succeeds cx t1 t2 then
    let r = update_desc_reason invalidate_rtype_alias reason1 in
    Type_filter.TypeFilterResult { type_ = DefT (r, EmptyT); changed = true }
  else
    let t1s_conc = possible_concrete_types_for_inspection cx reason1 t1 in
    let (ts_rev, changed) =
      Base.List.fold t1s_conc ~init:([], false) ~f:(fun (acc, changed) t1 ->
          if TypeUtil.quick_subtype t1 t2 || speculative_subtyping_succeeds cx t1 t2 then
            (acc, changed)
          else
            (t1 :: acc, true)
      )
    in
    let r1 = update_desc_reason invalidate_rtype_alias reason1 in
    Type_filter.TypeFilterResult { type_ = TypeUtil.union_of_ts r1 (List.rev ts_rev); changed }

and prop_exists_test cx key sense obj result_collector =
  match has_prop cx (OrdinaryName key) obj with
  | Some has ->
    if has = sense then
      report_unchanged_filtering_result_to_predicate_result obj result_collector
    else
      report_changes_to_input result_collector
  | None -> report_unchanged_filtering_result_to_predicate_result obj result_collector

(**
 * If an object has an own or non-own prop, representing `'key' in obj`.
 * Returns `None` if it is unknown whether the object has the prop (for example
 * due to inexact objects).
 *)
and has_prop cx key obj =
  let all_have_prop xs =
    Base.List.fold xs ~init:(Some true) ~f:(fun acc x ->
        match (acc, x) with
        | (None, _)
        | (_, None) ->
          None
        | (Some a, Some b) -> Some (a && b)
    )
  in
  let some_has_prop xs =
    Base.List.fold xs ~init:(Some false) ~f:(fun acc x ->
        match (acc, x) with
        | (Some true, None)
        | (None, Some true) ->
          Some true
        | (_, None)
        | (None, _) ->
          None
        | (Some a, Some b) -> Some (a || b)
    )
  in
  let find_key ~exact ~super ~props_list key =
    let current_has_prop =
      Base.List.map props_list ~f:(fun props ->
          match Context.get_prop cx props key with
          | Some prop ->
            (match prop with
            | Field { type_; _ } when Slice_utils.is_prop_optional type_ ->
              (* If a field is optional, it is unknown whether it exists. *)
              None
            | _ -> Some true)
          | None -> Some false
      )
      |> some_has_prop
    in
    match current_has_prop with
    | Some true -> Some true
    | Some false
    | None ->
      let super_ts = possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t super) super in
      let super_has_prop = Base.List.map super_ts ~f:(has_prop cx key) |> all_have_prop in
      (match super_has_prop with
      | Some true -> Some true
      | Some false when not exact -> None
      | Some false -> current_has_prop
      | None -> None)
  in
  match obj with
  | DefT (_, ObjT { flags; props_tmap; proto_t; _ }) ->
    find_key ~exact:(Obj_type.is_exact flags.obj_kind) ~super:proto_t ~props_list:[props_tmap] key
  | DefT (_, InstanceT { super; inst = { own_props; proto_props; _ }; _ }) ->
    find_key ~exact:false ~super ~props_list:[own_props; proto_props] key
  | NullProtoT _ -> Some false
  | ObjProtoT _ -> Some (is_object_prototype_method key)
  | FunProtoT _ -> Some (is_function_prototype key)
  | IntersectionT (reason, rep) ->
    InterRep.members rep
    |> Base.List.map ~f:(fun t ->
           let ts = possible_concrete_types_for_inspection cx reason t in
           Base.List.map ts ~f:(has_prop cx key) |> all_have_prop
       )
    |> some_has_prop
  | _ -> None

and prop_truthy_test cx trace key reason sense obj result_collector =
  prop_exists_test_generic
    key
    reason
    cx
    trace
    result_collector
    obj
    sense
    (PropGuardTruthy, PropGuardNotTruthy)
    obj

and prop_non_maybe_test cx trace key reason sense obj result_collector =
  prop_exists_test_generic
    key
    reason
    cx
    trace
    result_collector
    obj
    sense
    (PropGuardNotMaybe, PropGuardMaybe)
    obj

and prop_is_exactly_null_test cx trace key reason sense obj result_collector =
  prop_exists_test_generic
    key
    reason
    cx
    trace
    result_collector
    obj
    sense
    (PropGuardNull, PropGuardNotNull)
    obj

and prop_non_void_test cx trace key reason sense obj result_collector =
  prop_exists_test_generic
    key
    reason
    cx
    trace
    result_collector
    obj
    sense
    (PropGuardNotVoid, PropGuardVoid)
    obj

and prop_exists_test_generic key reason cx trace result_collector orig_obj sense (pred, not_pred) =
  function
  | DefT (_, ObjT { flags; props_tmap; _ }) as obj ->
    (match Context.get_prop cx props_tmap (OrdinaryName key) with
    | Some p ->
      (match Property.read_t p with
      | Some t ->
        (* prop is present on object type *)
        let pred =
          if sense then
            pred
          else
            not_pred
        in
        concretize_and_guard_prop cx t pred orig_obj result_collector
      | None ->
        (* prop cannot be read *)
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector;
        add_output
          cx
          (Error_message.EPropNotReadable
             { reason_prop = reason; prop_name = Some (OrdinaryName key); use_op = unknown_use }
          ))
    | None when Obj_type.is_exact flags.obj_kind ->
      (* prop is absent from exact object type *)
      if sense then
        report_changes_to_input result_collector
      else
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector
    | None ->
      (* prop is absent from inexact object type *)
      (* TODO: possibly unsound to filter out orig_obj here, but if we don't,
         case elimination based on prop existence checking doesn't work for
         (disjoint unions of) intersections of objects, where the prop appears
         in a different branch of the intersection. It is easy to avoid this
         unsoundness with slightly more work, but will wait until a
         refactoring of property lookup lands to revisit. Tracked by
         #11301092. *)
      if orig_obj = obj then
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector)
  | DefT (_, ArrT (TupleAT { elements; _ })) when is_str_intlike key ->
    let i = int_of_string key in
    (match Base.List.nth elements i with
    | Some (TupleElement { t; polarity; name; _ }) ->
      if Polarity.compat (polarity, Polarity.Positive) then
        let pred =
          if sense then
            pred
          else
            not_pred
        in
        concretize_and_guard_prop cx t pred orig_obj result_collector
      else (
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector;
        add_output
          cx
          (Error_message.ETupleElementNotReadable { use_op = unknown_use; reason; index = i; name })
      )
    | None ->
      (* Element is absent from tuple type. *)
      if sense then
        report_changes_to_input result_collector
      else
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector)
  | IntersectionT (reason, rep) ->
    (* For an intersection of object types, try the test for each object type in
       turn, while recording the original intersection so that we end up with
       the right refinement. See the comment on the implementation of
       IntersectionPreprocessKit for more details. *)
    InterRep.members rep
    |> List.iter (fun obj ->
           let f =
             prop_exists_test_generic
               key
               reason
               cx
               trace
               result_collector
               orig_obj
               sense
               (pred, not_pred)
           in
           possible_concrete_types_for_inspection cx reason obj |> List.iter f
       )
  | _ -> report_unchanged_filtering_result_to_predicate_result orig_obj result_collector

and binary_predicate cx trace sense test left right result_collector =
  match test with
  | InstanceofTest -> instanceof_test cx trace result_collector (sense, left, TypeOperand right)
  | SentinelProp key -> sentinel_prop_test key cx trace result_collector (sense, left, right)
  | EqTest -> eq_test cx trace result_collector (sense, left, right)

and instanceof_test cx trace result_collector = function
  (* instanceof on an ArrT is a special case since we treat ArrT as its own
     type, rather than an InstanceT of the Array builtin class. So, we resolve
     the ArrT to an InstanceT of Array, and redo the instanceof check. We do
     it at this stage instead of simply converting (ArrT, InstanceofP c)
     to (InstanceT(Array), InstanceofP c) because this allows c to be resolved
     first. *)
  | (true, (DefT (reason, ArrT arrtype) as arr), TypeOperand (DefT (r, ClassT a))) ->
    let elemt = elemt_of_arrtype arrtype in
    let right = InternalExtendsOperand (update_desc_reason (fun desc -> RExtends desc) r, arr, a) in
    let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
    concretize_and_run_predicate
      cx
      trace
      arrt
      ConcretizeForGeneralPredicateTest
      result_collector
      ~predicate_no_concretization:(fun cx trace tvar arrt ->
        instanceof_test cx trace tvar (true, arrt, right)
    )
  | (false, (DefT (reason, ArrT arrtype) as arr), TypeOperand (DefT (r, ClassT a))) ->
    let elemt = elemt_of_arrtype arrtype in
    let right = InternalExtendsOperand (update_desc_reason (fun desc -> RExtends desc) r, arr, a) in
    let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
    concretize_and_run_predicate
      cx
      trace
      arrt
      ConcretizeForGeneralPredicateTest
      result_collector
      ~predicate_no_concretization:(fun cx trace tvar arrt ->
        instanceof_test cx trace tvar (false, arrt, right)
    )
  (* Suppose that we have an instance x of class C, and we check whether x is
     `instanceof` class A. To decide what the appropriate refinement for x
     should be, we need to decide whether C extends A, choosing either C or A
     based on the result. Thus, we generate a constraint to decide whether C
     extends A (while remembering C), which may recursively generate further
     constraints to decide super(C) extends A, and so on, until we hit the root
     class. (As a technical tool, we use Extends(_, _) to perform this
     recursion; it is also used elsewhere for running similar recursive
     subclass decisions.) **)
  | (true, (DefT (_, InstanceT _) as c), TypeOperand (DefT (r, ClassT a))) ->
    instanceof_test
      cx
      trace
      result_collector
      (true, c, InternalExtendsOperand (update_desc_reason (fun desc -> RExtends desc) r, c, a))
  (* If C is a subclass of A, then don't refine the type of x. Otherwise,
     refine the type of x to A. (In general, the type of x should be refined to
     C & A, but that's hard to compute.) **)
  | ( true,
      DefT (reason, InstanceT { super = super_c; inst = instance_c; _ }),
      (InternalExtendsOperand (_, c, DefT (_, InstanceT { inst = instance_a; _ })) as right)
    ) ->
    (* TODO: intersection *)
    if is_same_instance_type instance_a instance_c then
      report_unchanged_filtering_result_to_predicate_result c result_collector
    else
      (* Recursively check whether super(C) extends A, with enough context. **)
      concretize_and_run_predicate
        cx
        trace
        (reposition_reason cx ~trace reason super_c)
        ConcretizeForGeneralPredicateTest
        result_collector
        ~predicate_no_concretization:(fun cx trace tvar l ->
          instanceof_test cx trace tvar (true, l, right)
      )
  (* If we are checking `instanceof Object` or `instanceof Function`, objects
     with `ObjProtoT` or `FunProtoT` should pass. *)
  | (true, ObjProtoT reason, (InternalExtendsOperand _ as right)) ->
    let obj_proto = get_builtin_type cx ~trace reason ~use_desc:true "Object" in
    concretize_and_run_predicate
      cx
      trace
      obj_proto
      ConcretizeForGeneralPredicateTest
      result_collector
      ~predicate_no_concretization:(fun cx trace tvar l ->
        instanceof_test cx trace tvar (true, l, right)
    )
  | (true, FunProtoT reason, (InternalExtendsOperand _ as right)) ->
    let fun_proto = get_builtin_type cx ~trace reason ~use_desc:true "Function" in
    concretize_and_run_predicate
      cx
      trace
      fun_proto
      ConcretizeForGeneralPredicateTest
      result_collector
      ~predicate_no_concretization:(fun cx trace tvar l ->
        instanceof_test cx trace tvar (true, l, right)
    )
  (* We hit the root class, so C is not a subclass of A **)
  | (true, DefT (_, NullT), InternalExtendsOperand (r, _, a)) ->
    report_changed_filtering_result_to_predicate_result
      (reposition cx ~trace (loc_of_reason r) a)
      result_collector
  (* If we're refining `mixed` or `any` with instanceof A, then flow A to the result *)
  | (true, (DefT (_, MixedT _) | AnyT _), TypeOperand (DefT (class_reason, ClassT a))) ->
    let desc = reason_of_t a |> desc_of_reason in
    let loc = loc_of_reason class_reason in
    report_changed_filtering_result_to_predicate_result
      (reposition cx ~trace ~desc loc a)
      result_collector
  (* Prune the type when any other `instanceof` check succeeds (since this is
     impossible). *)
  | (true, _, _) -> report_changes_to_input result_collector
  (* Like above, now suppose that we have an instance x of class C, and we
     check whether x is _not_ `instanceof` class A. To decide what the
     appropriate refinement for x should be, we need to decide whether C
     extends A, choosing either nothing or C based on the result. **)
  | ( false,
      (DefT (_, InstanceT _) as c),
      TypeOperand (DefT (r, ClassT (DefT (_, InstanceT _) as a)))
    ) ->
    instanceof_test
      cx
      trace
      result_collector
      (false, c, InternalExtendsOperand (update_desc_reason (fun desc -> RExtends desc) r, c, a))
  (* If C is a subclass of A, then do nothing, since this check cannot
     succeed. Otherwise, don't refine the type of x. **)
  | ( false,
      DefT (reason, InstanceT { super = super_c; inst = instance_c; _ }),
      (InternalExtendsOperand (_, _, DefT (_, InstanceT { inst = instance_a; _ })) as right)
    ) ->
    if is_same_instance_type instance_a instance_c then
      report_changes_to_input result_collector
    else
      concretize_and_run_predicate
        cx
        trace
        (reposition_reason cx ~trace reason super_c)
        ConcretizeForGeneralPredicateTest
        result_collector
        ~predicate_no_concretization:(fun cx trace tvar l ->
          instanceof_test cx trace tvar (false, l, right)
      )
  | (false, ObjProtoT _, InternalExtendsOperand (r, c, _)) ->
    (* We hit the root class, so C is not a subclass of A.
     * In this case, we will refine the input to C **)
    report_changed_filtering_result_to_predicate_result
      (reposition cx ~trace (loc_of_reason r) c)
      result_collector
  (* Don't refine the type when any other `instanceof` check fails. **)
  | (false, left, _) -> report_unchanged_filtering_result_to_predicate_result left result_collector

and sentinel_prop_test key cx trace result_collector (sense, obj, t) =
  sentinel_prop_test_generic key cx trace result_collector obj (sense, obj, t)

and sentinel_prop_test_generic key cx trace result_collector orig_obj =
  let desc_of_sentinel sentinel =
    match sentinel with
    | UnionEnum.(One (Str s)) -> RStringLit s
    | UnionEnum.(One (Num (_, n))) -> RNumberLit n
    | UnionEnum.(One (Bool b)) -> RBooleanLit b
    | UnionEnum.(One (BigInt (_, n))) -> RBigIntLit n
    | UnionEnum.(One Null) -> RNull
    | UnionEnum.(One Void) -> RVoid
    | UnionEnum.(Many _enums) -> RUnionEnum
  in

  (* Evaluate a refinement predicate of the form

     obj.key eq value

     where eq is === or !==.

     * key is key
     * (sense, obj, value) are the sense of the test, obj and value as above,
     respectively.

     As with other predicate filters, the goal is to statically determine when
     the predicate is definitely satisfied and when it is definitely
     unsatisfied, and narrow the possible types of obj under those conditions,
     while not narrowing in all other cases.

     In this case, the predicate is definitely satisfied (respectively,
     definitely unsatisfied) when the type of the key property in the type obj
     can be statically verified as having (respectively, not having) value as
     its only inhabitant.

     When satisfied, type obj flows to the recipient type result (in other
     words, we allow all such types in the refined type for obj).

     Otherwise, nothing flows to type result (in other words, we don't allow
     any such type in the refined type for obj).

     Overall the filtering process is somewhat tricky to understand. Refer to
     the predicate function and its callers to understand how the context is
     set up so that filtering ultimately only depends on what flows to
     result. **)
  let flow_sentinel_obj sense props_tmap obj sentinel =
    match Context.get_prop cx props_tmap (OrdinaryName key) with
    | Some p ->
      (match Property.read_t p with
      | Some t ->
        let reason =
          let desc = RMatchingProp (key, desc_of_sentinel sentinel) in
          replace_desc_reason desc (reason_of_t orig_obj)
        in
        concretize_and_run_sentinel_prop_test
          cx
          trace
          reason
          ~orig_obj
          ~sense
          ~sentinel
          t
          result_collector
      | None ->
        let reason_obj = reason_of_t obj in
        add_output
          cx
          (Error_message.EPropNotReadable
             { reason_prop = reason_obj; prop_name = Some (OrdinaryName key); use_op = unknown_use }
          ))
    | None ->
      (* TODO: possibly unsound to filter out orig_obj here, but if we
         don't, case elimination based on sentinel prop checking doesn't
         work for (disjoint unions of) intersections of objects, where the
         sentinel prop and the payload appear in different branches of the
         intersection. It is easy to avoid this unsoundness with slightly
         more work, but will wait until a refactoring of property lookup
         lands to revisit. Tracked by #11301092. *)
      if orig_obj = obj then
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector
  in
  let flow_sentinel_tuple sense elements tuple sentinel =
    let i = int_of_string key in
    match Base.List.nth elements i with
    | Some (TupleElement { t; polarity; name; _ }) ->
      if Polarity.compat (polarity, Polarity.Positive) then
        let reason =
          let desc = RMatchingProp (key, desc_of_sentinel sentinel) in
          replace_desc_reason desc (reason_of_t orig_obj)
        in
        concretize_and_run_sentinel_prop_test
          cx
          trace
          reason
          ~orig_obj
          ~sense
          ~sentinel
          t
          result_collector
      else
        add_output
          cx
          (Error_message.ETupleElementNotReadable
             { use_op = unknown_use; reason = reason_of_t tuple; index = i; name }
          )
    | None ->
      if orig_obj = tuple then
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector
  in
  let sentinel_of_literal = function
    | DefT (_, SingletonStrT { value; _ }) -> Some UnionEnum.(One (Str value))
    | DefT (_, SingletonNumT { value; _ }) -> Some UnionEnum.(One (Num value))
    | DefT (_, SingletonBoolT { value; _ }) -> Some UnionEnum.(One (Bool value))
    | DefT (_, SingletonBigIntT { value; _ }) -> Some UnionEnum.(One (BigInt value))
    | DefT (_, VoidT) -> Some UnionEnum.(One Void)
    | DefT (_, NullT) -> Some UnionEnum.(One Null)
    | UnionT (_, rep) -> begin
      match UnionRep.check_enum rep with
      | Some enums -> Some UnionEnum.(Many enums)
      | None -> None
    end
    | _ -> None
  in
  fun (sense, obj, t) ->
    match sentinel_of_literal t with
    | Some s -> begin
      match obj with
      (* obj.key ===/!== literal value *)
      | DefT (_, ObjT { props_tmap; _ }) -> flow_sentinel_obj sense props_tmap obj s
      (* instance.key ===/!== literal value *)
      | DefT (_, InstanceT { inst = { own_props; _ }; _ }) ->
        (* TODO: add test for sentinel test on implements *)
        flow_sentinel_obj sense own_props obj s
      (* tuple.length ===/!== literal value *)
      | DefT (reason, ArrT (TupleAT { elem_t = _; elements = _; arity; inexact; react_dro = _ }))
        when key = "length" ->
        let input = tuple_length reason ~inexact arity in
        concretize_and_run_sentinel_prop_test
          cx
          trace
          reason
          ~orig_obj
          ~sense
          ~sentinel:s
          input
          result_collector
      | DefT (_, ArrT (TupleAT { elements; _ })) when is_str_intlike key ->
        flow_sentinel_tuple sense elements obj s
      | IntersectionT (reason, rep) ->
        (* For an intersection of object types, try the test for each object
           type in turn, while recording the original intersection so that we
           end up with the right refinement. See the comment on the
           implementation of IntersectionPreprocessKit for more details. *)
        InterRep.members rep
        |> List.iter (fun obj ->
               let f l =
                 sentinel_prop_test_generic key cx trace result_collector orig_obj (sense, l, t)
               in
               possible_concrete_types_for_inspection cx reason obj |> List.iter f
           )
      | _ ->
        (* not enough info to refine *)
        report_unchanged_filtering_result_to_predicate_result orig_obj result_collector
    end
    | None ->
      (* not enough info to refine *)
      report_unchanged_filtering_result_to_predicate_result orig_obj result_collector

and concretize_and_run_sentinel_prop_test
    cx trace reason ~orig_obj ~sense ~sentinel input result_collector =
  possible_concrete_types_for_sentinel_prop_test cx reason input
  |> Base.List.iter ~f:(function
         | UnionT (r, rep) ->
           let l = orig_obj in
           (* we have the check l.key === sentinel where l.key is a union *)
           if sense then
             match sentinel with
             | UnionEnum.One enum ->
               let def =
                 match enum with
                 | UnionEnum.Str v -> SingletonStrT { from_annot = true; value = v }
                 | UnionEnum.Num v -> SingletonNumT { from_annot = true; value = v }
                 | UnionEnum.Bool v -> SingletonBoolT { from_annot = true; value = v }
                 | UnionEnum.BigInt v -> SingletonBigIntT { from_annot = true; value = v }
                 | UnionEnum.Void -> VoidT
                 | UnionEnum.Null -> NullT
               in
               (match
                  UnionRep.quick_mem_enum ~quick_subtype:TypeUtil.quick_subtype (DefT (r, def)) rep
                with
               | UnionRep.No ->
                 (* provably unreachable, so prune *)
                 report_changes_to_input result_collector
               | UnionRep.Yes ->
                 report_unchanged_filtering_result_to_predicate_result l result_collector
               | UnionRep.Conditional _
               | UnionRep.Unknown ->
                 (* inconclusive: the union is not concretized *)
                 Base.List.iter (UnionRep.members rep) ~f:(fun l ->
                     concretize_and_run_sentinel_prop_test
                       cx
                       trace
                       reason
                       ~orig_obj
                       ~sense
                       ~sentinel
                       l
                       result_collector
                 ))
             | UnionEnum.Many enums ->
               let acc =
                 UnionEnumSet.fold
                   (fun enum acc ->
                     let def =
                       match enum with
                       | UnionEnum.Str v -> SingletonStrT { from_annot = true; value = v }
                       | UnionEnum.Num v -> SingletonNumT { from_annot = true; value = v }
                       | UnionEnum.Bool v -> SingletonBoolT { from_annot = true; value = v }
                       | UnionEnum.BigInt v -> SingletonBigIntT { from_annot = true; value = v }
                       | UnionEnum.Void -> VoidT
                       | UnionEnum.Null -> NullT
                     in
                     UnionRep.join_quick_mem_results
                       ( acc,
                         UnionRep.quick_mem_enum
                           ~quick_subtype:TypeUtil.quick_subtype
                           (DefT (r, def))
                           rep
                       ))
                   enums
                   UnionRep.No
               in
               begin
                 match acc with
                 | UnionRep.No ->
                   (* provably unreachable, so prune *)
                   report_changes_to_input result_collector
                 | UnionRep.Yes ->
                   report_unchanged_filtering_result_to_predicate_result l result_collector
                 | UnionRep.Conditional _
                 | UnionRep.Unknown ->
                   (* inconclusive: the union is not concretized *)
                   Base.List.iter (UnionRep.members rep) ~f:(fun l ->
                       concretize_and_run_sentinel_prop_test
                         cx
                         trace
                         reason
                         ~orig_obj
                         ~sense
                         ~sentinel
                         l
                         result_collector
                   )
               end
           else
             (* for l.key !== sentinel where l.key is a union, we can't really prove
                that the check is guaranteed to fail (assuming the union doesn't
                degenerate to a singleton) *)
             report_unchanged_filtering_result_to_predicate_result l result_collector
         | l ->
           let t = Type_filter.sentinel_refinement l reason orig_obj sense sentinel in
           report_filtering_result_to_predicate_result t result_collector
         )

and eq_test cx _trace result_collector (sense, left, right) =
  let expected_loc = loc_of_t right in
  match right with
  | DefT (_, SingletonStrT { value; _ }) ->
    let filtered =
      if sense then
        Type_filter.string_literal expected_loc value left
      else
        Type_filter.not_string_literal value left
    in
    report_filtering_result_to_predicate_result filtered result_collector
  | DefT (_, SingletonNumT { value; _ }) ->
    let filtered =
      if sense then
        Type_filter.number_literal expected_loc value left
      else
        Type_filter.not_number_literal value left
    in
    report_filtering_result_to_predicate_result filtered result_collector
  | DefT (_, SingletonBoolT { value = true; _ }) ->
    let filtered =
      if sense then
        Type_filter.true_ left
      else
        Type_filter.not_true left
    in
    report_filtering_result_to_predicate_result filtered result_collector
  | DefT (_, SingletonBoolT { value = false; _ }) ->
    let filtered =
      if sense then
        Type_filter.false_ left
      else
        Type_filter.not_false left
    in
    report_filtering_result_to_predicate_result filtered result_collector
  | DefT (_, SingletonBigIntT { value; _ }) ->
    let filtered =
      if sense then
        Type_filter.bigint_literal expected_loc value left
      else
        Type_filter.not_bigint_literal value left
    in
    report_filtering_result_to_predicate_result filtered result_collector
  | DefT (_, VoidT) ->
    let filtered =
      if sense then
        Type_filter.undefined left
      else
        Type_filter.not_undefined cx left
    in
    report_filtering_result_to_predicate_result filtered result_collector
  | DefT (_, NullT) ->
    let filtered =
      if sense then
        Type_filter.null left
      else
        Type_filter.not_null cx left
    in
    report_filtering_result_to_predicate_result filtered result_collector
  | _ -> report_unchanged_filtering_result_to_predicate_result left result_collector

(**********)
(* guards *)
(**********)
and concretize_and_guard_prop cx source pred orig_obj result_collector =
  let all_changed =
    possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t source) source
    |> List.for_all (fun t -> guard_prop cx t pred)
  in
  if all_changed then
    report_changes_to_input result_collector
  else
    report_unchanged_filtering_result_to_predicate_result orig_obj result_collector

and guard_prop cx source pred =
  match pred with
  | PropGuardTruthy -> begin
    match Type_filter.truthy cx source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end
  | PropGuardNotTruthy -> begin
    match Type_filter.not_truthy cx source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end
  | PropGuardMaybe -> begin
    match Type_filter.maybe cx source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end
  | PropGuardNotMaybe -> begin
    match Type_filter.not_maybe cx source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end
  | PropGuardNull -> begin
    match Type_filter.null source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end
  | PropGuardNotNull -> begin
    match Type_filter.not_null cx source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end
  | PropGuardVoid -> begin
    match Type_filter.undefined source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end
  | PropGuardNotVoid -> begin
    match Type_filter.not_undefined cx source with
    | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed } -> changed
    | _ -> false
  end

type predicate_result =
  | TypeChanged of Type.t
  | TypeUnchanged of Type.t

let run_predicate_track_changes cx t p result_reason =
  let collector = TypeCollector.create () in
  let changed = ref false in
  let result_collector = PredicateResultCollector { collector; changed } in
  concretize_and_run_predicate
    cx
    DepthTrace.unit_trace
    t
    (concretization_variant_of_predicate p)
    ~predicate_no_concretization:(predicate_no_concretization ~p)
    result_collector;
  let t =
    TypeCollector.collect collector
    |> List.map (Tvar_resolver.resolved_t cx)
    |> TypeUtil.union_of_ts result_reason
  in
  if !changed then
    TypeChanged t
  else
    TypeUnchanged t

let run_predicate_for_filtering cx t p tout =
  let collector = TypeCollector.create () in
  let changed = ref false in
  let result_collector = PredicateResultCollector { collector; changed } in
  concretize_and_run_predicate
    cx
    DepthTrace.unit_trace
    t
    (concretization_variant_of_predicate p)
    ~predicate_no_concretization:(predicate_no_concretization ~p)
    result_collector;
  TypeCollector.iter collector ~f:(fun t -> Flow_js.flow_t cx (t, OpenT tout))
