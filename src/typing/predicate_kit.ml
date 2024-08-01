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

let concretization_variant_of_predicate = function
  | MaybeP
  | NotP MaybeP
  | ExistsP
  | NotP ExistsP ->
    ConcretizeForMaybeOrExistPredicateTest
  | _ -> ConcretizeForGeneralPredicateTest

let rec concretize_and_run_predicate cx trace l variant tvar ~predicate_no_concretization =
  let reason = reason_of_t l in
  let id = Tvar.mk_no_wrap cx reason in
  rec_flow cx trace (l, PredicateT (variant, (reason, id)));
  Flow_js_utils.types_of cx (Context.find_graph cx id)
  |> Base.List.iter ~f:(function
         | GenericT { bound; name; reason; id; no_infer } ->
           let bound_tvar_id = Tvar.mk_no_wrap cx reason in
           let bound_tvar = (reason, bound_tvar_id) in
           concretize_and_run_predicate
             cx
             trace
             (reposition_reason cx reason bound)
             variant
             bound_tvar
             ~predicate_no_concretization;
           Flow_js_utils.possible_types cx bound_tvar_id
           |> Base.List.iter ~f:(fun bound ->
                  rec_flow_t
                    cx
                    trace
                    ~use_op:unknown_use
                    (GenericT { reason = reason_of_t bound; name; bound; no_infer; id }, OpenT tvar)
              )
         | l -> predicate_no_concretization cx trace tvar l
         )

and concretize_binary_rhs_and_run_binary_predicate cx trace l r sense b tvar =
  let reason = reason_of_t r in
  let id = Tvar.mk_no_wrap cx reason in
  let variant =
    match b with
    | InstanceofTest -> ConcretizeRHSForInstanceOfPredicateTest
    | SentinelProp _ -> ConcretizeRHSForSentinelPropPredicateTest
  in
  rec_flow cx trace (r, PredicateT (variant, (reason, id)));
  Flow_js_utils.types_of cx (Context.find_graph cx id)
  |> Base.List.iter ~f:(fun r -> binary_predicate cx trace sense b l r tvar)

(* t - predicate output recipient (normally a tvar)
   l - incoming concrete LB (predicate input)
   result - guard result in case of success
   p - predicate *)
and predicate_no_concretization cx trace t l ~p =
  match p with
  (************************)
  (* deconstruction of && *)
  (************************)
  | AndP (p1, p2) ->
    let reason = replace_desc_reason RAnd (fst t) in
    let tvar = (reason, Tvar.mk_no_wrap cx reason) in
    concretize_and_run_predicate
      cx
      trace
      l
      (concretization_variant_of_predicate p1)
      tvar
      ~predicate_no_concretization:(predicate_no_concretization ~p:p1);
    concretize_and_run_predicate
      cx
      trace
      (OpenT tvar)
      (concretization_variant_of_predicate p2)
      t
      ~predicate_no_concretization:(predicate_no_concretization ~p:p2)
  (************************)
  (* deconstruction of || *)
  (************************)
  | OrP (p1, p2) ->
    concretize_and_run_predicate
      cx
      trace
      l
      (concretization_variant_of_predicate p1)
      t
      ~predicate_no_concretization:(predicate_no_concretization ~p:p1);
    concretize_and_run_predicate
      cx
      trace
      l
      (concretization_variant_of_predicate p2)
      t
      ~predicate_no_concretization:(predicate_no_concretization ~p:p2)
  (*********************************)
  (* deconstruction of binary test *)
  (*********************************)
  | BinaryP (b, r) -> concretize_binary_rhs_and_run_binary_predicate cx trace l r true b t
  | NotP (BinaryP (b, r)) -> concretize_binary_rhs_and_run_binary_predicate cx trace l r false b t
  (***********************)
  (* typeof _ ~ "boolean" *)
  (***********************)
  | BoolP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.boolean loc l, OpenT t)
  | NotP (BoolP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_boolean l, OpenT t)
  (***********************)
  (* typeof _ ~ "string" *)
  (***********************)
  | StrP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.string loc l, OpenT t)
  | NotP (StrP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_string l, OpenT t)
  (***********************)
  (* typeof _ ~ "symbol" *)
  (***********************)
  | SymbolP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.symbol loc l, OpenT t)
  | NotP (SymbolP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_symbol l, OpenT t)
  (*********************)
  (* _ ~ "some string" *)
  (*********************)
  | SingletonStrP (expected_loc, sense, lit) ->
    let filtered_str = Type_filter.string_literal expected_loc sense (OrdinaryName lit) l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered_str, OpenT t)
  | NotP (SingletonStrP (_, _, lit)) ->
    let filtered_str = Type_filter.not_string_literal (OrdinaryName lit) l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered_str, OpenT t)
  (*********************)
  (* _ ~ some number n *)
  (*********************)
  | SingletonNumP (expected_loc, sense, lit) ->
    let filtered_num = Type_filter.number_literal expected_loc sense lit l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered_num, OpenT t)
  | NotP (SingletonNumP (_, _, lit)) ->
    let filtered_num = Type_filter.not_number_literal lit l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered_num, OpenT t)
  (***********************)
  (* typeof _ ~ "number" *)
  (***********************)
  | NumP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.number loc l, OpenT t)
  | NotP (NumP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_number l, OpenT t)
  (*********************)
  (* _ ~ some bigint n *)
  (*********************)
  | SingletonBigIntP (expected_loc, sense, lit) ->
    let filtered_bigint = Type_filter.bigint_literal expected_loc sense lit l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered_bigint, OpenT t)
  | NotP (SingletonBigIntP (_, _, lit)) ->
    let filtered_bigint = Type_filter.not_bigint_literal lit l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered_bigint, OpenT t)
  (***********************)
  (* typeof _ ~ "bigint" *)
  (***********************)
  | BigIntP loc -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.bigint loc l, OpenT t)
  | NotP (BigIntP _) -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_bigint l, OpenT t)
  (***********************)
  (* typeof _ ~ "function" *)
  (***********************)
  | FunP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.function_ l, OpenT t)
  | NotP FunP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_function l, OpenT t)
  (***********************)
  (* typeof _ ~ "object" *)
  (***********************)
  | ObjP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.object_ cx l, OpenT t)
  | NotP ObjP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_object l, OpenT t)
  (*******************)
  (* Array.isArray _ *)
  (*******************)
  | ArrP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.array l, OpenT t)
  | NotP ArrP -> rec_flow_t cx trace ~use_op:unknown_use (Type_filter.not_array l, OpenT t)
  (***********************)
  (* typeof _ ~ "undefined" *)
  (***********************)
  | VoidP ->
    let filtered = Type_filter.undefined l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  | NotP VoidP ->
    let filtered = Type_filter.not_undefined cx l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  (********)
  (* null *)
  (********)
  | NullP ->
    let filtered = Type_filter.null l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  | NotP NullP ->
    let filtered = Type_filter.not_null cx l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  (*********)
  (* maybe *)
  (*********)
  | MaybeP ->
    let filtered = Type_filter.maybe cx l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  | NotP MaybeP ->
    let filtered = Type_filter.not_maybe cx l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  (********)
  (* true *)
  (********)
  | SingletonBoolP (_, true) ->
    let filtered = Type_filter.true_ l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  | NotP (SingletonBoolP (_, true)) ->
    let filtered = Type_filter.not_true l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  (*********)
  (* false *)
  (*********)
  | SingletonBoolP (_, false) ->
    let filtered = Type_filter.false_ l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  | NotP (SingletonBoolP (_, false)) ->
    let filtered = Type_filter.not_false l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  (************************)
  (* truthyness *)
  (************************)
  | ExistsP ->
    let filtered = Type_filter.exists cx l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  | NotP ExistsP ->
    let filtered = Type_filter.not_exists cx l in
    rec_flow_t cx trace ~use_op:unknown_use (filtered, OpenT t)
  | PropExistsP (key, r) -> prop_exists_test cx trace key r true l t
  | NotP (PropExistsP (key, r)) -> prop_exists_test cx trace key r false l t
  | PropNonMaybeP (key, r) -> prop_non_maybe_test cx trace key r true l t
  | NotP (PropNonMaybeP (key, r)) -> prop_non_maybe_test cx trace key r false l t
  (* classical logic i guess *)
  | NotP (NotP p) -> predicate_no_concretization cx trace t l ~p
  | NotP (AndP (p1, p2)) -> predicate_no_concretization cx trace t l ~p:(OrP (NotP p1, NotP p2))
  | NotP (OrP (p1, p2)) -> predicate_no_concretization cx trace t l ~p:(AndP (NotP p1, NotP p2))
  (********************)
  (* Latent predicate *)
  (********************)
  | LatentP ((lazy (use_op, loc, fun_t, targs, argts)), idx) ->
    let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
    call_latent_pred cx trace fun_t ~use_op ~reason ~targs ~argts ~sense:true ~idx l t
  | NotP (LatentP ((lazy (use_op, loc, fun_t, targs, argts)), idx)) ->
    let reason = mk_reason (RFunctionCall (desc_of_t fun_t)) loc in
    call_latent_pred cx trace fun_t ~use_op ~reason ~targs ~argts ~sense:false ~idx l t

(* call_latent_pred connects a predicate function with information available
 * at a call-site appearing in a conditional position (e.g. `if (pred(x))`).
 * [tin] is the incoming type of `x` and [tout] the refined result in the then-
 * branch. Since at the time of processing the call we do not know yet the
 * function's formal parameters, [idx] is the index of the argument that gets
 * refined. *)
and call_latent_pred cx trace fun_t ~use_op ~reason ~targs ~argts ~sense ~idx tin tout =
  let ts = possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t fun_t) fun_t in
  Base.List.iter ts ~f:(function
      | IntersectionT (r, rep) ->
        Base.List.map (InterRep.members rep) ~f:(fun t () ->
            call_latent_pred cx trace t ~use_op ~reason ~targs ~argts ~sense ~idx tin tout
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
      | DefT (_, FunT (_, { params; predicate = Some p; _ })) -> begin
        (* TODO: for the moment we only support simple keys (empty projection)
           that exactly correspond to the function's parameters *)
        match (Base.List.nth params idx, p) with
        | (None, _)
        | (Some (None, _), _) ->
          let msg = Error_message.(EInternal (loc_of_reason reason, MissingPredicateParam idx)) in
          add_output cx msg;
          rec_flow_t ~use_op:unknown_use cx trace (tin, OpenT tout)
        | (Some (Some name, _), PredBased (_, (lazy (pmap, nmap)))) ->
          let key = (OrdinaryName name, []) in
          let preds =
            if sense then
              pmap
            else
              nmap
          in
          begin
            match Key_map.find_opt key preds with
            | Some p ->
              concretize_and_run_predicate
                cx
                trace
                tin
                (concretization_variant_of_predicate p)
                tout
                ~predicate_no_concretization:(predicate_no_concretization ~p)
            | None -> rec_flow_t ~use_op:unknown_use cx trace (tin, OpenT tout)
          end
        | ( Some (Some name, _),
            TypeGuardBased { reason = _; one_sided; param_name = (_, param_name); type_guard }
          ) ->
          let t =
            if param_name <> name then
              (* This is not the refined parameter. *)
              tin
            else if sense then
              intersect cx tin type_guard
            else if not one_sided then
              type_guard_diff cx tin type_guard
            else
              (* Do not refine else branch on one-sided type-guard *)
              tin
          in
          rec_flow_t ~use_op:unknown_use cx trace (t, OpenT tout)
      end
      | DefT (reason_tapp, PolyT { tparams_loc; tparams = ids; t_out = t; _ }) as fun_t ->
        let tvar = (reason, Tvar.mk_no_wrap cx reason) in
        let calltype = mk_functioncalltype ~call_kind:RegularCallKind reason targs argts tvar in
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
        call_latent_pred cx trace t_ ~use_op ~reason ~targs:None ~argts ~sense ~idx tin tout
      (* Fall through all the remaining cases *)
      | _ -> rec_flow_t ~use_op:unknown_use cx trace (tin, OpenT tout)
      )

(* This utility is expected to be used when we a variable of type [t1] is refined
 * with the use of a type guard function with type `(x: mixed) => x is t2`.
 * t1 is already concretized by the time it reaches this point. Type t2, on the
 * other hand, is not, since it is coming directly from the annotation. This is why
 * we concretize it first, before attempting any comparisons. *)
and intersect cx t1 t2 =
  let module TSet = Type_filter.TypeTagSet in
  let quick_subtype = TypeUtil.quick_subtype in
  let t1_tags = Type_filter.tag_of_t cx t1 in
  let t2_tags =
    t2
    |> possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t2)
    |> Base.List.map ~f:(Type_filter.tag_of_t cx)
    |> Base.Option.all
    |> Base.Option.map ~f:(Base.List.fold ~init:TSet.empty ~f:TSet.union)
  in
  let is_any t =
    let ts = possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t in
    List.exists Type.is_any ts
  in
  let is_null t =
    match possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t with
    | [DefT (_, NullT)] -> true
    | _ -> false
  in
  let is_void t =
    match possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t t) t with
    | [DefT (_, VoidT)] -> true
    | _ -> false
  in
  match (t1_tags, t2_tags) with
  | (Some t1_tags, Some t2_tags) when not (Type_filter.tags_overlap t1_tags t2_tags) ->
    let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
    DefT (r, EmptyT)
  | _ ->
    if is_any t1 then
      t2
    else if is_any t2 then
      (* Filter out null and void types from the input if comparing with any *)
      if is_null t1 || is_void t1 then
        let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
        DefT (r, EmptyT)
      else
        t1
    else if quick_subtype t1 t2 then
      t1
    else if quick_subtype t2 t1 then
      t2
    else if speculative_subtyping_succeeds cx t1 t2 then
      t1
    else if speculative_subtyping_succeeds cx t2 t1 then
      t2
    else
      let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
      IntersectionT (r, InterRep.make t2 t1 [])

(* This utility is expected to be used when negating the refinement of a type [t1]
 * with a type guard `x is t2`. The only case considered here is that of t1 <: t2.
 * This means that the positive branch will always be taken, and so we are left with
 * `empty` in the negated case. *)
and type_guard_diff cx t1 t2 =
  if TypeUtil.quick_subtype t1 t2 || speculative_subtyping_succeeds cx t1 t2 then
    let r = update_desc_reason invalidate_rtype_alias (TypeUtil.reason_of_t t1) in
    DefT (r, EmptyT)
  else
    t1

and prop_exists_test cx trace key reason sense obj result =
  prop_exists_test_generic key reason cx trace result obj sense (ExistsP, NotP ExistsP) obj

and prop_non_maybe_test cx trace key reason sense obj result =
  prop_exists_test_generic key reason cx trace result obj sense (NotP MaybeP, MaybeP) obj

and prop_exists_test_generic key reason cx trace result orig_obj sense (pred, not_pred) = function
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
        possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t) t
        |> List.iter (fun t -> guard cx trace t pred orig_obj result)
      | None ->
        (* prop cannot be read *)
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result);
        add_output
          cx
          (Error_message.EPropNotReadable
             { reason_prop = reason; prop_name = Some (OrdinaryName key); use_op = unknown_use }
          ))
    | None when Obj_type.is_exact flags.obj_kind ->
      (* prop is absent from exact object type *)
      if sense then
        ()
      else
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
    | None ->
      (* prop is absent from inexact object type *)
      (* TODO: possibly unsound to filter out orig_obj here, but if we don't,
         case elimination based on prop existence checking doesn't work for
         (disjoint unions of) intersections of objects, where the prop appears
         in a different branch of the intersection. It is easy to avoid this
         unsoundness with slightly more work, but will wait until a
         refactoring of property lookup lands to revisit. Tracked by
         #11301092. *)
      if orig_obj = obj then rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result))
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
        possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t) t
        |> List.iter (fun t -> guard cx trace t pred orig_obj result)
      else (
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result);
        add_output
          cx
          (Error_message.ETupleElementNotReadable { use_op = unknown_use; reason; index = i; name })
      )
    | None ->
      (* Element is absent from tuple type. *)
      if sense then
        ()
      else
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result))
  | IntersectionT (_, rep) ->
    (* For an intersection of object types, try the test for each object type in
       turn, while recording the original intersection so that we end up with
       the right refinement. See the comment on the implementation of
       IntersectionPreprocessKit for more details. *)
    let reason = fst result in
    InterRep.members rep
    |> List.iter (fun obj ->
           let f =
             prop_exists_test_generic key reason cx trace result orig_obj sense (pred, not_pred)
           in
           possible_concrete_types_for_inspection cx reason obj |> List.iter f
       )
  | _ -> rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)

and binary_predicate cx trace sense test left right result =
  match test with
  | InstanceofTest -> instanceof_test cx trace result (sense, left, TypeOperand right)
  | SentinelProp key -> sentinel_prop_test key cx trace result (sense, left, right)

and instanceof_test cx trace result = function
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
      result
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
      result
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
      result
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
      rec_flow_t cx trace ~use_op:unknown_use (c, OpenT result)
    else
      (* Recursively check whether super(C) extends A, with enough context. **)
      concretize_and_run_predicate
        cx
        trace
        (reposition_reason cx ~trace reason super_c)
        ConcretizeForGeneralPredicateTest
        result
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
      result
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
      result
      ~predicate_no_concretization:(fun cx trace tvar l ->
        instanceof_test cx trace tvar (true, l, right)
    )
  (* We hit the root class, so C is not a subclass of A **)
  | (true, DefT (_, NullT), InternalExtendsOperand (r, _, a)) ->
    rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace (loc_of_reason r) a, OpenT result)
  (* If we're refining `mixed` or `any` with instanceof A, then flow A to the result *)
  | (true, (DefT (_, MixedT _) | AnyT _), TypeOperand (DefT (class_reason, ClassT a))) ->
    let desc = reason_of_t a |> desc_of_reason in
    let loc = loc_of_reason class_reason in
    rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace ~desc loc a, OpenT result)
  (* Prune the type when any other `instanceof` check succeeds (since this is
     impossible). *)
  | (true, _, _) -> ()
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
      result
      (false, c, InternalExtendsOperand (update_desc_reason (fun desc -> RExtends desc) r, c, a))
  (* If C is a subclass of A, then do nothing, since this check cannot
     succeed. Otherwise, don't refine the type of x. **)
  | ( false,
      DefT (reason, InstanceT { super = super_c; inst = instance_c; _ }),
      (InternalExtendsOperand (_, _, DefT (_, InstanceT { inst = instance_a; _ })) as right)
    ) ->
    if is_same_instance_type instance_a instance_c then
      ()
    else
      concretize_and_run_predicate
        cx
        trace
        (reposition_reason cx ~trace reason super_c)
        ConcretizeForGeneralPredicateTest
        result
        ~predicate_no_concretization:(fun cx trace tvar l ->
          instanceof_test cx trace tvar (false, l, right)
      )
  | (false, ObjProtoT _, InternalExtendsOperand (r, c, _)) ->
    (* We hit the root class, so C is not a subclass of A **)
    rec_flow_t cx trace ~use_op:unknown_use (reposition cx ~trace (loc_of_reason r) c, OpenT result)
  (* Don't refine the type when any other `instanceof` check fails. **)
  | (false, left, _) -> rec_flow_t cx trace ~use_op:unknown_use (left, OpenT result)

and sentinel_prop_test key cx trace result (sense, obj, t) =
  sentinel_prop_test_generic key cx trace result obj (sense, obj, t)

and sentinel_prop_test_generic key cx trace result orig_obj =
  let desc_of_sentinel sentinel =
    match sentinel with
    | UnionEnum.(One (Str s)) -> RStringLit s
    | UnionEnum.(One (Num n)) -> RNumberLit (string_of_float n)
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
          replace_desc_reason desc (fst result)
        in
        concretize_and_run_sentinel_prop_test cx trace reason ~orig_obj ~sense ~sentinel t result
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
      if orig_obj = obj then rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
  in
  let flow_sentinel_tuple sense elements tuple sentinel =
    let i = int_of_string key in
    match Base.List.nth elements i with
    | Some (TupleElement { t; polarity; name; _ }) ->
      if Polarity.compat (polarity, Polarity.Positive) then
        let reason =
          let desc = RMatchingProp (key, desc_of_sentinel sentinel) in
          replace_desc_reason desc (fst result)
        in
        concretize_and_run_sentinel_prop_test cx trace reason ~orig_obj ~sense ~sentinel t result
      else
        add_output
          cx
          (Error_message.ETupleElementNotReadable
             { use_op = unknown_use; reason = reason_of_t tuple; index = i; name }
          )
    | None ->
      if orig_obj = tuple then rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
  in
  let sentinel_of_literal = function
    | DefT (_, StrT (Literal (_, value)))
    | DefT (_, SingletonStrT value) ->
      Some UnionEnum.(One (Str value))
    | DefT (_, NumT (Literal (_, (value, _))))
    | DefT (_, SingletonNumT (value, _)) ->
      Some UnionEnum.(One (Num value))
    | DefT (_, BoolT (Some value))
    | DefT (_, SingletonBoolT value) ->
      Some UnionEnum.(One (Bool value))
    | DefT (_, BigIntT (Literal (_, value)))
    | DefT (_, SingletonBigIntT value) ->
      Some UnionEnum.(One (BigInt value))
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
        let r = replace_desc_reason (RMatchingProp (key, desc_of_sentinel s)) (fst result) in
        let input = tuple_length reason ~inexact arity in
        concretize_and_run_sentinel_prop_test cx trace r ~orig_obj ~sense ~sentinel:s input result
      | DefT (_, ArrT (TupleAT { elements; _ })) when is_str_intlike key ->
        flow_sentinel_tuple sense elements obj s
      | IntersectionT (_, rep) ->
        (* For an intersection of object types, try the test for each object
           type in turn, while recording the original intersection so that we
           end up with the right refinement. See the comment on the
           implementation of IntersectionPreprocessKit for more details. *)
        let reason = fst result in
        InterRep.members rep
        |> List.iter (fun obj ->
               let f l = sentinel_prop_test_generic key cx trace result orig_obj (sense, l, t) in
               possible_concrete_types_for_inspection cx reason obj |> List.iter f
           )
      | _ ->
        (* not enough info to refine *)
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
    end
    | None ->
      (* not enough info to refine *)
      rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)

and concretize_and_run_sentinel_prop_test cx trace reason ~orig_obj ~sense ~sentinel input result =
  let id = Tvar.mk_no_wrap cx reason in
  rec_flow cx trace (input, SentinelPropTestT (reason, id));
  Flow_js_utils.types_of cx (Context.find_graph cx id)
  |> Base.List.iter ~f:(function
         | UnionT (r, rep) ->
           let l = orig_obj in
           (* we have the check l.key === sentinel where l.key is a union *)
           if sense then
             match sentinel with
             | UnionEnum.One enum ->
               let def =
                 match enum with
                 | UnionEnum.Str v -> SingletonStrT v
                 | UnionEnum.Num v -> SingletonNumT (v, string_of_float v)
                 | UnionEnum.Bool v -> SingletonBoolT v
                 | UnionEnum.BigInt v -> SingletonBigIntT v
                 | UnionEnum.Void -> VoidT
                 | UnionEnum.Null -> NullT
               in
               (match
                  UnionRep.quick_mem_enum ~quick_subtype:TypeUtil.quick_subtype (DefT (r, def)) rep
                with
               | UnionRep.No -> () (* provably unreachable, so prune *)
               | UnionRep.Yes -> rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
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
                       result
                 ))
             | UnionEnum.Many enums ->
               let acc =
                 UnionEnumSet.fold
                   (fun enum acc ->
                     let def =
                       match enum with
                       | UnionEnum.Str v -> SingletonStrT v
                       | UnionEnum.Num v -> SingletonNumT (v, string_of_float v)
                       | UnionEnum.Bool v -> SingletonBoolT v
                       | UnionEnum.BigInt v -> SingletonBigIntT v
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
                 | UnionRep.No -> () (* provably unreachable, so prune *)
                 | UnionRep.Yes -> rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
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
                         result
                   )
               end
           else
             (* for l.key !== sentinel where l.key is a union, we can't really prove
                that the check is guaranteed to fail (assuming the union doesn't
                degenerate to a singleton) *)
             rec_flow_t ~use_op:unknown_use cx trace (l, OpenT result)
         | l ->
           let t = Type_filter.sentinel_refinement l reason orig_obj sense sentinel in
           rec_flow_t cx trace ~use_op:unknown_use (t, OpenT result)
         )

(**********)
(* guards *)
(**********)
and guard cx trace source pred result sink =
  match pred with
  | ExistsP -> begin
    match Type_filter.exists cx source with
    | DefT (_, EmptyT) -> ()
    | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
  end
  | NotP ExistsP -> begin
    match Type_filter.not_exists cx source with
    | DefT (_, EmptyT) -> ()
    | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
  end
  | MaybeP -> begin
    match Type_filter.maybe cx source with
    | DefT (_, EmptyT) -> ()
    | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
  end
  | NotP MaybeP -> begin
    match Type_filter.not_maybe cx source with
    | DefT (_, EmptyT) -> ()
    | _ -> rec_flow_t cx trace ~use_op:unknown_use (result, OpenT sink)
  end
  | NotP (NotP p) -> guard cx trace source p result sink
  | _ ->
    let loc = loc_of_reason (fst sink) in
    let pred_str = string_of_predicate pred in
    add_output cx Error_message.(EInternal (loc, UnsupportedGuardPredicate pred_str))

let predicate cx t p =
  concretize_and_run_predicate
    cx
    DepthTrace.unit_trace
    t
    (concretization_variant_of_predicate p)
    ~predicate_no_concretization:(predicate_no_concretization ~p)
