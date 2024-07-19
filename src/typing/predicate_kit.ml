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

module type S = sig
  val predicate : Context.t -> DepthTrace.t -> tvar -> Type.t -> predicate -> unit

  val prop_exists_test_generic :
    string ->
    reason ->
    Context.t ->
    DepthTrace.t ->
    tvar ->
    Type.t ->
    bool ->
    predicate * predicate ->
    Type.t ->
    unit

  val sentinel_prop_test_generic :
    string -> Context.t -> DepthTrace.t -> tvar -> Type.t -> bool * Type.t * Type.t -> unit
end

module Make (Flow : Flow_common.S) : S = struct
  module SpeculationKit = Speculation_kit.Make (Flow)
  open Flow

  (* t - predicate output recipient (normally a tvar)
     l - incoming concrete LB (predicate input)
     result - guard result in case of success
     p - predicate *)
  let rec predicate cx trace t l p =
    match p with
    (************************)
    (* deconstruction of && *)
    (************************)
    | AndP (p1, p2) ->
      let reason = replace_desc_reason RAnd (fst t) in
      let tvar = (reason, Tvar.mk_no_wrap cx reason) in
      rec_flow cx trace (l, PredicateT (p1, tvar));
      rec_flow cx trace (OpenT tvar, PredicateT (p2, t))
    (************************)
    (* deconstruction of || *)
    (************************)
    | OrP (p1, p2) ->
      rec_flow cx trace (l, PredicateT (p1, t));
      rec_flow cx trace (l, PredicateT (p2, t))
    (*********************************)
    (* deconstruction of binary test *)
    (*********************************)

    (* when left is evaluated, store it and evaluate right *)
    | LeftP (b, r) -> rec_flow cx trace (r, PredicateT (RightP (b, l), t))
    | NotP (LeftP (b, r)) -> rec_flow cx trace (r, PredicateT (NotP (RightP (b, l)), t))
    (* when right is evaluated, call appropriate handler *)
    | RightP (b, actual_l) ->
      let r = l in
      let l = actual_l in
      binary_predicate cx trace true b l r t
    | NotP (RightP (b, actual_l)) ->
      let r = l in
      let l = actual_l in
      binary_predicate cx trace false b l r t
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
    | NotP (NotP p) -> predicate cx trace t l p
    | NotP (AndP (p1, p2)) -> predicate cx trace t l (OrP (NotP p1, NotP p2))
    | NotP (OrP (p1, p2)) -> predicate cx trace t l (AndP (NotP p1, NotP p2))
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
        | DefT (_, FunT (_, { params; predicate = Some predicate; _ })) -> begin
          (* TODO: for the moment we only support simple keys (empty projection)
             that exactly correspond to the function's parameters *)
          match (Base.List.nth params idx, predicate) with
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
              | Some p -> rec_flow cx trace (tin, PredicateT (p, tout))
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
   * Because this kind of refinement is expressed through a PredicateT constraint,
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
          rec_flow cx trace (t, GuardT (pred, orig_obj, result))
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
          rec_flow cx trace (t, GuardT (pred, orig_obj, result))
        else (
          rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result);
          add_output
            cx
            (Error_message.ETupleElementNotReadable
               { use_op = unknown_use; reason; index = i; name }
            )
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
             rec_flow
               cx
               trace
               ( obj,
                 PreprocessKitT
                   (reason, PropExistsTest (sense, key, reason, orig_obj, result, (pred, not_pred)))
               )
         )
    | _ -> rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)

  and binary_predicate cx trace sense test left right result =
    let handler =
      match test with
      | InstanceofTest -> instanceof_test
      | SentinelProp key -> sentinel_prop_test key
    in
    handler cx trace result (sense, left, right)

  and instanceof_test cx trace result = function
    (* instanceof on an ArrT is a special case since we treat ArrT as its own
       type, rather than an InstanceT of the Array builtin class. So, we resolve
       the ArrT to an InstanceT of Array, and redo the instanceof check. We do
       it at this stage instead of simply converting (ArrT, InstanceofP c)
       to (InstanceT(Array), InstanceofP c) because this allows c to be resolved
       first. *)
    | (true, (DefT (reason, ArrT arrtype) as arr), DefT (r, ClassT a)) ->
      let elemt = elemt_of_arrtype arrtype in
      let right = InternalT (ExtendsT (update_desc_reason (fun desc -> RExtends desc) r, arr, a)) in
      let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
      rec_flow cx trace (arrt, PredicateT (LeftP (InstanceofTest, right), result))
    | (false, (DefT (reason, ArrT arrtype) as arr), DefT (r, ClassT a)) ->
      let elemt = elemt_of_arrtype arrtype in
      let right = InternalT (ExtendsT (update_desc_reason (fun desc -> RExtends desc) r, arr, a)) in
      let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
      let pred = NotP (LeftP (InstanceofTest, right)) in
      rec_flow cx trace (arrt, PredicateT (pred, result))
    (* Suppose that we have an instance x of class C, and we check whether x is
       `instanceof` class A. To decide what the appropriate refinement for x
       should be, we need to decide whether C extends A, choosing either C or A
       based on the result. Thus, we generate a constraint to decide whether C
       extends A (while remembering C), which may recursively generate further
       constraints to decide super(C) extends A, and so on, until we hit the root
       class. (As a technical tool, we use Extends(_, _) to perform this
       recursion; it is also used elsewhere for running similar recursive
       subclass decisions.) **)
    | (true, (DefT (_, InstanceT _) as c), DefT (r, ClassT a)) ->
      predicate
        cx
        trace
        result
        (InternalT (ExtendsT (update_desc_reason (fun desc -> RExtends desc) r, c, a)))
        (RightP (InstanceofTest, c))
    (* If C is a subclass of A, then don't refine the type of x. Otherwise,
       refine the type of x to A. (In general, the type of x should be refined to
       C & A, but that's hard to compute.) **)
    | ( true,
        DefT (reason, InstanceT { super = super_c; inst = instance_c; _ }),
        (InternalT (ExtendsT (_, c, DefT (_, InstanceT { inst = instance_a; _ }))) as right)
      ) ->
      (* TODO: intersection *)
      if is_same_instance_type instance_a instance_c then
        rec_flow_t cx trace ~use_op:unknown_use (c, OpenT result)
      else
        (* Recursively check whether super(C) extends A, with enough context. **)
        let pred = LeftP (InstanceofTest, right) in
        let u = PredicateT (pred, result) in
        rec_flow cx trace (super_c, ReposLowerT (reason, false, u))
    (* If we are checking `instanceof Object` or `instanceof Function`, objects
       with `ObjProtoT` or `FunProtoT` should pass. *)
    | (true, ObjProtoT reason, (InternalT (ExtendsT _) as right)) ->
      let obj_proto = get_builtin_type cx ~trace reason ~use_desc:true "Object" in
      rec_flow cx trace (obj_proto, PredicateT (LeftP (InstanceofTest, right), result))
    | (true, FunProtoT reason, (InternalT (ExtendsT _) as right)) ->
      let fun_proto = get_builtin_type cx ~trace reason ~use_desc:true "Function" in
      rec_flow cx trace (fun_proto, PredicateT (LeftP (InstanceofTest, right), result))
    (* We hit the root class, so C is not a subclass of A **)
    | (true, DefT (_, NullT), InternalT (ExtendsT (r, _, a))) ->
      rec_flow_t
        cx
        trace
        ~use_op:unknown_use
        (reposition cx ~trace (loc_of_reason r) a, OpenT result)
    (* If we're refining `mixed` or `any` with instanceof A, then flow A to the result *)
    | (true, (DefT (_, MixedT _) | AnyT _), DefT (class_reason, ClassT a)) ->
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
    | (false, (DefT (_, InstanceT _) as c), DefT (r, ClassT (DefT (_, InstanceT _) as a))) ->
      predicate
        cx
        trace
        result
        (InternalT (ExtendsT (update_desc_reason (fun desc -> RExtends desc) r, c, a)))
        (NotP (RightP (InstanceofTest, c)))
    (* If C is a subclass of A, then do nothing, since this check cannot
       succeed. Otherwise, don't refine the type of x. **)
    | ( false,
        DefT (reason, InstanceT { super = super_c; inst = instance_c; _ }),
        (InternalT (ExtendsT (_, _, DefT (_, InstanceT { inst = instance_a; _ }))) as right)
      ) ->
      if is_same_instance_type instance_a instance_c then
        ()
      else
        let u = PredicateT (NotP (LeftP (InstanceofTest, right)), result) in
        rec_flow cx trace (super_c, ReposLowerT (reason, false, u))
    | (false, ObjProtoT _, InternalT (ExtendsT (r, c, _))) ->
      (* We hit the root class, so C is not a subclass of A **)
      rec_flow_t
        cx
        trace
        ~use_op:unknown_use
        (reposition cx ~trace (loc_of_reason r) c, OpenT result)
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
          let test = SentinelPropTestT (reason, orig_obj, sense, sentinel, result) in
          rec_flow cx trace (t, test)
        | None ->
          let reason_obj = reason_of_t obj in
          add_output
            cx
            (Error_message.EPropNotReadable
               {
                 reason_prop = reason_obj;
                 prop_name = Some (OrdinaryName key);
                 use_op = unknown_use;
               }
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
          let test = SentinelPropTestT (reason, orig_obj, sense, sentinel, result) in
          rec_flow cx trace (t, test)
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
          let test =
            let desc = RMatchingProp (key, desc_of_sentinel s) in
            let r = replace_desc_reason desc (fst result) in
            SentinelPropTestT (r, orig_obj, sense, s, result)
          in
          rec_flow cx trace (tuple_length reason ~inexact arity, test)
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
                 rec_flow
                   cx
                   trace
                   (obj, PreprocessKitT (reason, SentinelPropTest (sense, key, t, orig_obj, result)))
             )
        | _ ->
          (* not enough info to refine *)
          rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
      end
      | None ->
        (* not enough info to refine *)
        rec_flow_t cx trace ~use_op:unknown_use (orig_obj, OpenT result)
end
