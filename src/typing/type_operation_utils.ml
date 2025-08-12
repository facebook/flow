(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Flow = Flow_js
module Ast = Flow_ast
open Enclosing_context
open Reason
open Type

module DistributeUnionIntersection = struct
  (* For a type t, run the check defined by check_base.
   * This function will break down the unions in t. When it encounters an intersection,
   * the check can pass as long as the check can pass on one member of intersection *)
  let rec distribute cx ?use_op ~break_up_union ~get_no_match_error_loc ~check_base t =
    let ts = break_up_union cx (TypeUtil.reason_of_t t) t in
    Base.List.iter ts ~f:(function
        | IntersectionT (r, rep) ->
          Base.List.map (InterRep.members rep) ~f:(fun t () ->
              distribute cx ?use_op ~break_up_union ~get_no_match_error_loc ~check_base t
          )
          |> Speculation_flow.try_custom cx ?use_op ~no_match_error_loc:(get_no_match_error_loc r)
        | t -> check_base cx t
        )

  (* For a pair of type (t1, t2), run the check defined by check_base.
   * This function will break down the unions in t1 and t2. When it encounters an intersection,
   * the check can pass as long as the check can pass on one member of intersection *)
  let rec distribute_2 cx ?use_op ~break_up_union ~get_no_match_error_loc ~check_base (t1, t2) =
    let t1s = break_up_union cx (TypeUtil.reason_of_t t1) t1 in
    let t2s = break_up_union cx (TypeUtil.reason_of_t t2) t2 in
    Base.List.cartesian_product t1s t2s
    |> Base.List.iter ~f:(function
           | (IntersectionT (r1, rep1), IntersectionT (r2, rep2)) ->
             let cases =
               Base.List.cartesian_product (InterRep.members rep1) (InterRep.members rep2)
               |> Base.List.map ~f:(fun pair () ->
                      distribute_2
                        cx
                        ?use_op
                        ~break_up_union
                        ~get_no_match_error_loc
                        ~check_base
                        pair
                  )
             in
             Speculation_flow.try_custom
               cx
               ?use_op
               ~no_match_error_loc:(get_no_match_error_loc r1 r2)
               cases
           | (IntersectionT (r1, rep1), t2) ->
             let cases =
               Base.List.map (InterRep.members rep1) ~f:(fun t1 () ->
                   distribute_2
                     cx
                     ?use_op
                     ~break_up_union
                     ~get_no_match_error_loc
                     ~check_base
                     (t1, t2)
               )
             in
             Speculation_flow.try_custom
               cx
               ?use_op
               ~no_match_error_loc:(get_no_match_error_loc r1 (TypeUtil.reason_of_t t2))
               cases
           | (t1, IntersectionT (r2, rep2)) ->
             let cases =
               Base.List.map (InterRep.members rep2) ~f:(fun t2 () ->
                   distribute_2
                     cx
                     ?use_op
                     ~break_up_union
                     ~get_no_match_error_loc
                     ~check_base
                     (t1, t2)
               )
             in
             Speculation_flow.try_custom
               cx
               ?use_op
               ~no_match_error_loc:(get_no_match_error_loc (TypeUtil.reason_of_t t1) r2)
               cases
           | (t1, t2) -> check_base cx (t1, t2)
           )
end

module Operators = struct
  let arith cx reason kind t1 t2 =
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
        DistributeUnionIntersection.distribute_2
          cx
          ~break_up_union:Flow.possible_concrete_types_for_operators_checking
          ~get_no_match_error_loc:(fun _ _ -> loc_of_reason reason)
          ~check_base:(fun cx (t1, t2) ->
            Flow.flow_t cx (Flow_js_utils.flow_arith cx reason t1 t2 kind, tout))
          (t1, t2)
    )

  let check_comparator =
    let check_base cx = function
      | (DefT (_, (StrGeneralT _ | SingletonStrT _)), DefT (_, (StrGeneralT _ | SingletonStrT _)))
      | (DefT (_, (NumGeneralT _ | SingletonNumT _)), DefT (_, (NumGeneralT _ | SingletonNumT _)))
      | (DefT (_, BigIntGeneralT _), DefT (_, BigIntGeneralT _))
      | (DefT (_, EmptyT), _)
      | (_, DefT (_, EmptyT))
      | (AnyT _, _)
      | (_, AnyT _) ->
        ()
      | (l, r) when Flow_js_utils.is_date l && Flow_js_utils.is_date r -> ()
      | (l, r) ->
        let (r1, r2) =
          Flow_error.ordered_reasons (TypeUtil.reason_of_t l, TypeUtil.reason_of_t r)
        in
        Flow_js_utils.add_output
          cx
          (Error_message.EComparison { r1; r2; loc_opt = None; strict_comparison_opt = None })
    in
    fun cx t1 t2 ->
      DistributeUnionIntersection.distribute_2
        cx
        ~break_up_union:Flow.possible_concrete_types_for_operators_checking
        ~get_no_match_error_loc:(fun r1 r2 ->
          Flow_error.ordered_reasons (r1, r2) |> fst |> loc_of_reason)
        ~check_base
        (t1, t2)

  let eq_needs_concretization = function
    | OpenT _
    | GenericT _
    | EvalT _
    | TypeAppT _
    | OptionalT _
    | UnionT _
    | MaybeT _
    | AnnotT _
    | KeysT _
    | NullProtoT _ ->
      true
    | _ -> false

  let check_eq =
    let open Flow_js_utils in
    let get_no_match_error_loc r_pair =
      r_pair |> Flow_error.ordered_reasons |> fst |> loc_of_reason
    in
    let will_fail_check_if_unmatched = function
      | DefT
          ( _,
            ( NumGeneralT _ | StrGeneralT _ | BoolGeneralT | SingletonNumT _ | SingletonStrT _
            | SingletonBoolT _ | SymbolT | EnumObjectT _ | EnumValueT _ )
          ) ->
        true
      | _ -> false
    in
    let is_always_allowed_one_side_t = function
      | AnyT _
      | DefT (_, EmptyT)
      | DefT (_, MixedT _)
      | DefT (_, VoidT)
      | DefT (_, NullT) ->
        true
      | _ -> false
    in
    (* If we allow `==` on these two types. *)
    let equatable = function
      | (DefT (_, (NumGeneralT _ | SingletonNumT _)), DefT (_, (NumGeneralT _ | SingletonNumT _)))
      | ( (DefT (_, (StrGeneralT _ | SingletonStrT _)) | StrUtilT _),
          (DefT (_, (StrGeneralT _ | SingletonStrT _)) | StrUtilT _)
        )
      | (DefT (_, (BoolGeneralT | SingletonBoolT _)), DefT (_, (BoolGeneralT | SingletonBoolT _)))
      | (DefT (_, SymbolT), DefT (_, SymbolT)) ->
        true
      | (t1, t2) -> (not (will_fail_check_if_unmatched t1)) && not (will_fail_check_if_unmatched t2)
    in
    let rec distribute cx (t1, t2) =
      if is_always_allowed_one_side_t t1 || is_always_allowed_one_side_t t2 then
        ()
      else
        match (t1, t2) with
        | (IntersectionT (r1, rep1), t2) ->
          let cases =
            Base.List.map (InterRep.members rep1) ~f:(fun t1 () -> distribute cx (t1, t2))
          in
          Speculation_flow.try_custom
            cx
            ~no_match_error_loc:(get_no_match_error_loc (r1, TypeUtil.reason_of_t t2))
            cases
        | (t1, IntersectionT (r2, rep2)) ->
          let cases =
            Base.List.map (InterRep.members rep2) ~f:(fun t2 () -> distribute cx (t1, t2))
          in
          Speculation_flow.try_custom
            cx
            ~no_match_error_loc:(get_no_match_error_loc (TypeUtil.reason_of_t t1, r2))
            cases
        | (t1, t2) when eq_needs_concretization t1 ->
          Base.List.iter
            (Flow.possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t1) t1)
            ~f:(fun t1 -> distribute cx (t1, t2))
        | (t1, t2) when eq_needs_concretization t2 ->
          Base.List.iter
            (Flow.possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t2) t2)
            ~f:(fun t2 -> distribute cx (t1, t2))
        | (l, r) ->
          if equatable (l, r) then
            ()
          else
            let reasons =
              FlowError.ordered_reasons (TypeUtil.reason_of_t l, TypeUtil.reason_of_t r)
            in
            add_output cx (Error_message.ENonStrictEqualityComparison reasons)
    in
    distribute

  let check_strict_eq =
    let open Flow_js_utils in
    let get_no_match_error_loc r_pair =
      r_pair |> Flow_error.ordered_reasons |> fst |> loc_of_reason
    in
    let not_possible_enum_after_concretization = function
      | DefT (_, EnumValueT _)
      | DefT (_, EnumObjectT _)
      | IntersectionT _ ->
        false
      | _ -> true
    in
    let strict_equatable_error cx encl_ctx (l, r) =
      let comparison_error =
        let open TypeUtil in
        lazy
          (match encl_ctx with
          | SwitchTestContext { case_test_loc; switch_discriminant_loc } ->
            let use_op =
              Op
                (SwitchRefinementCheck
                   { test = case_test_loc; discriminant = switch_discriminant_loc }
                )
            in
            Error_message.EIncompatibleWithUseOp
              {
                reason_lower = reason_of_t l;
                reason_upper = reason_of_t r;
                use_op;
                explanation = None;
              }
          | NoContext
          | IndexContext
          | OtherTestContext
          | JsxTitleNameContext
          | JsxAttrOrChildrenContext
          | LiteralTestContext
          | MatchPattern
          | StrictComparison ->
            let (r1, r2) = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
            Error_message.EComparison { r1; r2; loc_opt = None; strict_comparison_opt = None })
      in
      match (l, r) with
      (* We allow comparison between enums and enum values with the same id. *)
      | ( DefT (_, EnumObjectT { enum_info = ConcreteEnum { enum_id = id1; _ }; _ }),
          DefT (_, EnumObjectT { enum_info = ConcreteEnum { enum_id = id2; _ }; _ })
        )
      | ( DefT (_, EnumValueT (ConcreteEnum { enum_id = id1; _ })),
          DefT (_, EnumValueT (ConcreteEnum { enum_id = id2; _ }))
        )
        when ALoc.equal_id id1 id2 ->
        None
      (* We allow comparison between abstract and concrete enums and enum values. *)
      | (DefT (_, EnumObjectT _), DefT (_, EnumObjectT { enum_info = AbstractEnum _; _ }))
      | (DefT (_, EnumObjectT { enum_info = AbstractEnum _; _ }), DefT (_, EnumObjectT _))
      | (DefT (_, EnumValueT _), DefT (_, EnumValueT (AbstractEnum _)))
      | (DefT (_, EnumValueT (AbstractEnum _)), DefT (_, EnumValueT _)) ->
        None
      (* We allow the comparison of enums to null and void outside of switches. *)
      | (DefT (_, EnumValueT _), DefT (_, (NullT | VoidT)))
      | (DefT (_, (NullT | VoidT)), DefT (_, EnumValueT _)) -> begin
        match encl_ctx with
        | SwitchTestContext _ -> Some (Lazy.force comparison_error)
        | NoContext
        | IndexContext
        | OtherTestContext
        | JsxTitleNameContext
        | JsxAttrOrChildrenContext
        | LiteralTestContext
        | MatchPattern
        | StrictComparison ->
          None
      end
      (* We don't allow the comparison of enums and other types in general. *)
      | (DefT (_, EnumValueT _), _)
      | (_, DefT (_, EnumValueT _))
      | (DefT (_, EnumObjectT _), _)
      | (_, DefT (_, EnumObjectT _)) ->
        if
          Context.enable_invalid_comparison_general cx
          && Context.enable_invalid_comparison_null_check cx
        then
          None
        else
          Some (Lazy.force comparison_error)
      (* We don't check other strict equality comparisons. *)
      | _ -> None
    in
    let rec distribute cx ~encl_ctx (t1, t2) =
      match (t1, t2) with
      | (DefT (_, EmptyT), _)
      | (_, DefT (_, EmptyT))
      | (AnyT _, _)
      | (_, AnyT _) ->
        ()
      | (IntersectionT (r1, rep1), t2) ->
        let cases =
          Base.List.map (InterRep.members rep1) ~f:(fun t1 () -> distribute cx ~encl_ctx (t1, t2))
        in
        Speculation_flow.try_custom
          cx
          ~no_match_error_loc:(get_no_match_error_loc (r1, TypeUtil.reason_of_t t2))
          cases
      | (t1, IntersectionT (r2, rep2)) ->
        let cases =
          Base.List.map (InterRep.members rep2) ~f:(fun t2 () -> distribute cx ~encl_ctx (t1, t2))
        in
        Speculation_flow.try_custom
          cx
          ~no_match_error_loc:(get_no_match_error_loc (TypeUtil.reason_of_t t1, r2))
          cases
      | (t1, t2) ->
        let t1_needs_concretization = eq_needs_concretization t1 in
        let t2_needs_concretization = eq_needs_concretization t2 in
        if (not t1_needs_concretization) && not t2_needs_concretization then
          match strict_equatable_error cx encl_ctx (t1, t2) with
          | Some error -> add_output cx error
          | None -> ()
        else
          let t1s =
            if t1_needs_concretization then
              Flow.possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t1) t1
            else
              [t1]
          in
          let t2s =
            if t2_needs_concretization then
              Flow.possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t2) t2
            else
              [t2]
          in
          (* The strict_eq check will only complain when there is enum on one side. Therefore,
           * when there are no enums, we only need to check anything. *)
          if
            List.for_all not_possible_enum_after_concretization t1s
            && List.for_all not_possible_enum_after_concretization t2s
          then
            ()
          else
            Base.List.iter t1s ~f:(fun t1 ->
                Base.List.iter t2s ~f:(fun t2 -> distribute cx ~encl_ctx (t1, t2))
            )
    in
    (fun ~encl_ctx -> distribute ~encl_ctx)

  let unary_arith cx reason kind t =
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
        DistributeUnionIntersection.distribute
          cx
          ~break_up_union:Flow.possible_concrete_types_for_operators_checking
          ~get_no_match_error_loc:loc_of_reason
          ~check_base:(fun cx t ->
            Flow.flow_t cx (Flow_js_utils.flow_unary_arith cx t reason kind, tout))
          t
    )

  let mk_tvar_and_resolve_to_logical_union cx reason f =
    let id = Tvar.mk_no_wrap cx reason in
    let tout = (reason, id) in
    f tout;
    match
      Flow_js_utils.merge_tvar_opt ~filter_empty:true ~union_kind:UnionRep.LogicalKind cx reason id
    with
    | Some t -> t
    | None -> Tvar_resolver.default_no_lowers reason

  let logical_and cx reason left right =
    mk_tvar_and_resolve_to_logical_union cx reason (fun tout ->
        Flow.possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t left) left
        |> Base.List.iter ~f:(fun left ->
               begin
                 match left with
                 | DefT (reason, (NumGeneralT _ | SingletonNumT _)) ->
                   Flow_js_utils.add_output
                     cx
                     (Error_message.ESketchyNumberLint (Lints.SketchyNumberAnd, reason))
                 | _ -> ()
               end;

               (* a falsy && b ~> a
                  a truthy && b ~> b
                  a && b ~> a falsy | b *)
               match Type_filter.truthy cx left with
               | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } ->
                 (* falsy *)
                 Predicate_kit.run_predicate_for_filtering cx left (NotP TruthyP) tout
               | _ ->
                 (match Type_filter.not_truthy cx left with
                 | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } ->
                   (* truthy *)
                   Flow.flow cx (right, UseT (unknown_use, OpenT tout))
                 | _ ->
                   Predicate_kit.run_predicate_for_filtering cx left (NotP TruthyP) tout;
                   Flow.flow cx (right, UseT (unknown_use, OpenT tout)))
           )
    )

  let logical_or cx reason left right =
    mk_tvar_and_resolve_to_logical_union cx reason (fun tout ->
        Flow.possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t left) left
        |> Base.List.iter ~f:(fun left ->
               (* a truthy || b ~> a
                  a falsy || b ~> b
                  a || b ~> a truthy | b *)
               match Type_filter.not_truthy cx left with
               | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } ->
                 (* truthy *)
                 Predicate_kit.run_predicate_for_filtering cx left TruthyP tout
               | _ ->
                 (match Type_filter.truthy cx left with
                 | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } ->
                   (* falsy *)
                   Flow.flow cx (right, UseT (unknown_use, OpenT tout))
                 | _ ->
                   Predicate_kit.run_predicate_for_filtering cx left TruthyP tout;
                   Flow.flow cx (right, UseT (unknown_use, OpenT tout)))
           )
    )

  let logical_nullish_coalesce cx reason left right =
    mk_tvar_and_resolve_to_logical_union cx reason (fun tout ->
        Flow.possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t left) left
        |> Base.List.iter ~f:(fun left ->
               match Type_filter.maybe cx left with
               | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ }
               (* This `AnyT` case is required to have similar behavior to the other logical operators. *)
               | Type_filter.TypeFilterResult { type_ = AnyT _; changed = _ } ->
                 (* not-nullish *)
                 Predicate_kit.run_predicate_for_filtering cx left (NotP MaybeP) tout
               | _ ->
                 (match Type_filter.not_maybe cx left with
                 | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } ->
                   (* nullish *)
                   Flow.flow cx (right, UseT (unknown_use, OpenT tout))
                 | _ ->
                   Predicate_kit.run_predicate_for_filtering cx left (NotP MaybeP) tout;
                   Flow.flow cx (right, UseT (unknown_use, OpenT tout)))
           )
    )

  let unary_not =
    let f reason = function
      | AnyT (_, src) -> AnyT.why src reason
      | DefT (_, BoolGeneralT)
      | DefT (_, StrGeneralT AnyLiteral)
      | DefT (_, NumGeneralT AnyLiteral) ->
        BoolModuleT.at (loc_of_reason reason)
      (* !x when x is falsy *)
      | DefT (_, SingletonBoolT { value = false; _ })
      | DefT (_, SingletonStrT { value = OrdinaryName ""; _ })
      | DefT (_, SingletonNumT { value = (0., _); _ })
      | DefT (_, NullT)
      | DefT (_, VoidT) ->
        let reason = replace_desc_reason (RBooleanLit true) reason in
        DefT (reason, SingletonBoolT { value = true; from_annot = false })
      (* !x when x is truthy *)
      | _ ->
        let reason = replace_desc_reason (RBooleanLit false) reason in
        DefT (reason, SingletonBoolT { value = false; from_annot = false })
    in
    fun cx reason t ->
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
          DistributeUnionIntersection.distribute
            cx
            t
            ~break_up_union:Flow.possible_concrete_types_for_inspection
            ~get_no_match_error_loc:loc_of_reason
            ~check_base:(fun cx t -> Flow.flow_t cx (f reason t, tout)
          )
      )

  let non_maybe =
    let f = function
      | DefT (r, NullT)
      | DefT (r, VoidT) ->
        EmptyT.why r
      | DefT (r, MixedT (Mixed_everything | Mixed_non_null | Mixed_non_void)) ->
        DefT (r, MixedT Mixed_non_maybe)
      | t -> t
    in
    fun cx reason t ->
      Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
          DistributeUnionIntersection.distribute
            cx
            t
            ~break_up_union:Flow.possible_concrete_types_for_inspection
            ~get_no_match_error_loc:loc_of_reason
            ~check_base:(fun cx t -> Flow.flow_t cx (f t, tout)
          )
      )
end

module Promise = struct
  let await cx reason t =
    (* await distributes over union: await (Promise<T> | void) = T | void *)
    match
      Flow.possible_concrete_types_for_inspection cx reason t
      |> List.map (Flow.FlowJs.ImplicitInstantiationKit.run_await cx ~use_op:unknown_use ~reason)
    with
    | [] -> EmptyT.why reason
    | [t] -> t
    | t0 :: t1 :: ts -> UnionT (reason, UnionRep.make t0 t1 ts)
end

module SpecialCasedFunctions = struct
  let object_assign cx use_op reason target_t rest_arg_ts =
    (* What is this EVIL cache doing? Consider the following example:

       ```
       declare const inter: {foo: string, ...} & {bar: number, ...};
       Object.assign(inter, inter);
       ```

       Of course, we want it to pass, because it should be equivalent to

       ```
       declare const inter: {foo: string, bar: number, ...};
       Object.assign(inter, inter);
       ```

       which should pass.

       However, without the cache, the above code will be treated like

       - Try to assign `{foo: string, ...} & {bar: number, ...}` to `{foo: string, ...}`
         - Assign `{foo: string, ...}` to `{foo: string, ...}` -> Good
         - Assign `{bar: number, ...}` to `{foo: string, ...}` -> Failed
         - This branch of speculation has failed
       - Try to assign `{foo: string, ...} & {bar: number, ...}` to `{bar: number, ...}`
         - Assign `{foo: string, ...}` to `{bar: number, ...}` -> Failed
         - This branch of speculation has failed
       - Speculation failed!

       The cache is trying to "fix" this by trying to assign each member of intersection only once,
       so the above example becomes something like

       - Try to assign `{foo: string, ...} & {bar: number, ...}` to `{foo: string, ...}`
         - Assign `{foo: string, ...}` to `{foo: string, ...}` -> Good
         - Assign `{bar: number, ...}` to `{foo: string, ...}` -> Failed
         - This branch of speculation has failed
       - Try to assign `{foo: string, ...} & {bar: number, ...}` to `{bar: number, ...}`
         - Assign `{foo: string, ...}` to `{bar: number, ...}` -> Cache hit
         - Assign `{bar: number, ...}` to `{bar: number, ...}` -> Cache hit
         - This branch of speculation has succeed by doing nothing...
       - Speculation succeeded!

       This is a very bad fix, since it's easily broken by the following example:

       ```
       declare const inter: {foo: string, ...} & {bar: number, ...};
       declare const inter2: {bar: number, ...} & {foo: string, ...};
       Object.assign(inter, inter2);
       ```

       Since `Object.assign` support is on the chopping block, it might not make sense to fix
       it in a proper way, but the least we can do is to localize the broken cache. The cache used
       to be global one.
    *)
    let fix_cache = ref TypeMap.empty in
    let open TypeUtil in
    let rec assign_from l use_op reason_op to_obj t kind =
      match to_obj with
      | AnyT _ ->
        (* Special case any. Otherwise this will lead to confusing errors when
         * any transforms to an object type. *)
        Flow.flow_t cx (to_obj, t)
      | to_obj ->
        Base.List.iter
          (Flow.possible_concrete_types_for_object_assign cx (TypeUtil.reason_of_t l) l)
          ~f:(fun l -> assign_from_after_concretization l use_op reason_op to_obj t kind)
    and assign_from_after_concretization l use_op reason_op to_obj t kind =
      match (l, kind) with
      (* assign_from copies multiple properties from its incoming LB.
         Here we simulate a merged object type by iterating over the
         entire intersection. *)
      | (IntersectionT (_, rep), kind) ->
        let tvar =
          List.fold_left
            (fun tout t ->
              let tvar =
                match TypeMap.find_opt t !fix_cache with
                | Some tvar -> tvar
                | None ->
                  Tvar.mk_where cx reason_op (fun tvar ->
                      fix_cache := TypeMap.add t tvar !fix_cache;
                      assign_from t use_op reason_op to_obj tvar kind
                  )
              in
              Flow.flow_t cx (tvar, tout);
              tvar)
            (Tvar.mk cx reason_op)
            (InterRep.members rep)
        in
        Flow.flow_t cx (tvar, t)
      | (DefT (lreason, ObjT { props_tmap = mapr; flags; _ }), ObjAssign _) ->
        Context.iter_props cx mapr (fun name p ->
            (* move the reason to the call site instead of the definition, so
               that it is in the same scope as the Object.assign, so that
               strictness rules apply. *)
            let reason_prop =
              lreason
              |> update_desc_reason (fun desc -> RPropertyOf (name, desc))
              |> repos_reason (loc_of_reason reason_op)
            in
            match Property.read_t p with
            | Some t ->
              let propref = mk_named_prop ~reason:reason_prop name in

              let t =
                Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason_prop (fun tout ->
                    Flow.flow cx (t, FilterOptionalT (unknown_use, tout))
                )
              in
              Flow.flow cx (to_obj, SetPropT (use_op, reason_prop, propref, Assign, Normal, t, None))
            | None ->
              Flow_js_utils.add_output
                cx
                (Error_message.EPropNotReadable { reason_prop; prop_name = Some name; use_op })
        );
        (match flags.obj_kind with
        | Indexed _ -> Flow.flow_t cx (AnyT.make Untyped reason_op, t)
        | Exact
        | Inexact ->
          Flow.flow_t cx (to_obj, t))
      | (DefT (lreason, InstanceT { inst = { own_props; proto_props; _ }; _ }), ObjAssign _) ->
        let own_props = Context.find_props cx own_props in
        let proto_props = Context.find_props cx proto_props in
        let props = NameUtils.Map.union own_props proto_props in
        props
        |> NameUtils.Map.iter (fun name p ->
               match Property.read_t p with
               | Some t ->
                 let propref = mk_named_prop ~reason:reason_op name in
                 Flow.flow
                   cx
                   (to_obj, SetPropT (use_op, reason_op, propref, Assign, Normal, t, None))
               | None ->
                 Flow_js_utils.add_output
                   cx
                   (Error_message.EPropNotReadable
                      { reason_prop = lreason; prop_name = Some name; use_op }
                   )
           );
        Flow.flow_t cx (to_obj, t)
      (* AnyT has every prop, each one typed as `any`, so spreading it into an
         existing object destroys all of the keys, turning the result into an
         AnyT as well. TODO: wait for `to_obj` to be resolved, and then call
         `SetPropT (_, _, _, AnyT, _)` on all of its props. *)
      | (AnyT (_, src), ObjAssign _) -> Flow.flow_t cx (AnyT.make src reason, t)
      | (AnyT _, _) -> Flow.flow_t cx (l, t)
      | (ObjProtoT _, ObjAssign _) -> Flow.flow_t cx (to_obj, t)
      (* Object.assign semantics *)
      | (DefT (_, (NullT | VoidT)), ObjAssign _) -> Flow.flow_t cx (to_obj, t)
      (* {...mixed} is the equivalent of {...{[string]: mixed}} *)
      | (DefT (reason, MixedT _), ObjAssign _) ->
        let dict =
          {
            dict_name = None;
            key = StrModuleT.make reason;
            value = l;
            dict_polarity = Polarity.Neutral;
          }
        in
        let o = Obj_type.mk_with_proto cx reason (ObjProtoT reason) ~obj_kind:(Indexed dict) in
        assign_from_after_concretization o use_op reason_op to_obj t kind
      | (DefT (reason_arr, ArrT arrtype), ObjSpreadAssign) -> begin
        match arrtype with
        | ArrayAT { elem_t; tuple_view = None; react_dro = _ }
        | ArrayAT { elem_t; tuple_view = Some (TupleView { inexact = true; _ }); react_dro = _ }
        | ROArrayAT (elem_t, _) ->
          (* Object.assign(o, ...Array<x>) -> Object.assign(o, x) *)
          assign_from elem_t use_op reason_op to_obj t default_obj_assign_kind
        | TupleAT { elements; _ } ->
          (* Object.assign(o, ...[x,y,z]) -> Object.assign(o, x, y, z) *)
          List.iteri
            (fun n (TupleElement { t = from; polarity; name; optional = _; reason = _ }) ->
              if not @@ Polarity.compat (polarity, Polarity.Positive) then
                Flow_js_utils.add_output
                  cx
                  (Error_message.ETupleElementNotReadable
                     { use_op; reason = reason_arr; index = n; name }
                  );
              assign_from from use_op reason_op to_obj t default_obj_assign_kind)
            elements
        | ArrayAT { tuple_view = Some (TupleView { elements; arity = _; inexact = _ }); _ } ->
          (* Object.assign(o, ...[x,y,z]) -> Object.assign(o, x, y, z) *)
          let ts = tuple_ts_of_elements elements in
          List.iter
            (fun from -> assign_from from use_op reason_op to_obj t default_obj_assign_kind)
            ts
      end
      | (GenericT { reason; bound; _ }, _) ->
        assign_from (Flow.reposition cx (loc_of_reason reason) bound) use_op reason_op to_obj t kind
      | (l, _) ->
        Flow_js_utils.add_output
          cx
          (Error_message.EIncompatible
             {
               lower = (reason_of_t l, None);
               upper =
                 ( reason_op,
                   match kind with
                   | ObjSpreadAssign -> Error_message.IncompatibleObjAssignFromTSpread
                   | ObjAssign _ -> Error_message.IncompatibleObjAssignFromT
                 );
               use_op = Some use_op;
             }
          );
        Flow.flow_t cx (AnyT.error reason_op, t)
    in
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
        let result =
          List.fold_left
            (fun result that ->
              let (that, kind) =
                match that with
                | Arg t -> (t, default_obj_assign_kind)
                | SpreadArg t ->
                  (* If someone does Object.assign({}, ...Array<obj>) we can treat it like
                     Object.assign({}, obj). *)
                  (t, ObjSpreadAssign)
              in
              let use_op = Op (ObjectChain { op = reason }) in
              Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun t ->
                  Base.List.iter
                    (Flow.possible_concrete_types_for_object_assign cx reason result)
                    ~f:(fun l ->
                      match l with
                      | IntersectionT (r, rep) ->
                        (* This is insufficient to deal with nested intersections.
                         * However, it's unlikely to cause issues, and we should instead
                         * focus our energy on killing `Object.assign` support instead. *)
                        Base.List.map (InterRep.members rep) ~f:(fun to_obj () ->
                            assign_from that use_op reason to_obj t kind
                        )
                        |> Speculation_flow.try_custom
                             cx
                             ~use_op
                             ~no_match_error_loc:(loc_of_reason r)
                      | to_obj -> assign_from that use_op reason to_obj t kind
                  )
              ))
            target_t
            rest_arg_ts
        in
        Flow.flow cx (Flow.reposition cx (loc_of_reason reason) result, UseT (use_op, tout))
    )
end

module TypeAssertions = struct
  open Flow_js_utils
  open TypeUtil

  let assert_binary_in_lhs cx t =
    DistributeUnionIntersection.distribute
      cx
      t
      ~break_up_union:Flow.possible_concrete_types_for_operators_checking
      ~get_no_match_error_loc:loc_of_reason
      ~check_base:(fun cx -> function
      | AnyT _ -> ()
      (* the left-hand side of a `(x in y)` expression is a string or number
         TODO: also, symbols *)
      | DefT (_, StrGeneralT _)
      | DefT (_, SingletonStrT _) ->
        ()
      | DefT (_, NumGeneralT _)
      | DefT (_, SingletonNumT _) ->
        ()
      | l -> add_output cx (Error_message.EBinaryInLHS (reason_of_t l))
    )

  let assert_binary_in_rhs cx t =
    DistributeUnionIntersection.distribute
      cx
      t
      ~break_up_union:Flow.possible_concrete_types_for_operators_checking
      ~get_no_match_error_loc:loc_of_reason
      ~check_base:(fun cx -> function
      | AnyT _
      (* the right-hand side of a `(x in y)` expression must be object-like *)
      | DefT (_, ArrT _) ->
        ()
      | l when object_like l -> ()
      | l -> add_output cx (Error_message.EBinaryInRHS (reason_of_t l))
    )

  let assert_export_is_type cx name t =
    let reason = TypeUtil.reason_of_t t in
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
        let t =
          t
          |> Flow.singleton_concretize_type_for_imports_exports cx reason
          |> Flow_js_utils.AssertExportIsTypeTKit.on_concrete_type cx name
        in
        Flow.flow_t cx (t, tout)
    )

  let assert_for_in_rhs cx t =
    DistributeUnionIntersection.distribute
      cx
      t
      ~break_up_union:Flow.possible_concrete_types_for_operators_checking
      ~get_no_match_error_loc:loc_of_reason
      ~check_base:(fun cx -> function
      | l when object_like l -> ()
      | AnyT _
      | ObjProtoT _ ->
        ()
      (* null/undefined are allowed *)
      | DefT (_, (NullT | VoidT)) -> ()
      | DefT (enum_reason, EnumObjectT _) ->
        add_output cx (Error_message.EEnumNotIterable { reason = enum_reason; for_in = true })
      | l -> add_output cx (Error_message.EForInRHS (TypeUtil.reason_of_t l))
    )

  let assert_non_component_like_base =
    let check_base ~def_loc ~use_reason cx = function
      | DefT (reason_type, MixedT _) ->
        add_output
          cx
          Error_message.(
            EReactIntrinsicOverlap
              { use = use_reason; def = def_loc; type_ = loc_of_reason reason_type; mixed = true }
          )
      | DefT
          ( reason_type,
            (ObjT { call_t = Some _; _ } | FunT _ | ClassT _ | ReactAbstractComponentT _)
          ) ->
        add_output
          cx
          Error_message.(
            EReactIntrinsicOverlap
              { use = use_reason; def = def_loc; type_ = loc_of_reason reason_type; mixed = false }
          )
      | _ -> ()
    in
    fun cx def_loc use_reason t ->
      DistributeUnionIntersection.distribute
        cx
        t
        ~break_up_union:Flow.possible_concrete_types_for_operators_checking
        ~get_no_match_error_loc:(fun _ -> loc_of_reason use_reason)
        ~check_base:(check_base ~def_loc ~use_reason)

  let assert_instanceof_rhs cx t =
    DistributeUnionIntersection.distribute
      cx
      t
      ~break_up_union:Flow.possible_concrete_types_for_operators_checking
      ~get_no_match_error_loc:loc_of_reason
      ~check_base:(fun cx -> function
      (********************)
      (* `instanceof` RHS *)
      (* right side of an `instanceof` binary expression must be an object *)
      (********************)
      | l when object_like l -> ()
      | DefT (_, ArrT _) ->
        (* arrays are objects too, but not in `object_like` *)
        ()
      | AnyT _ -> ()
      | l -> add_output cx (Error_message.EInstanceofRHS (reason_of_t l))
    )

  let assert_iterable cx loc ~async ~use_op t targs_to_infer =
    Flow.possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t) t
    |> Base.List.iter ~f:(function
           | DefT (enum_reason, EnumObjectT _) ->
             add_output cx (Error_message.EEnumNotIterable { reason = enum_reason; for_in = false });
             let any = AnyT.at (AnyError None) loc in
             Base.List.iter targs_to_infer ~f:(fun t -> Flow.unify cx ~use_op any t)
           | AnyT (reason, src) ->
             let src = any_mod_src_keep_placeholder (AnyError None) src in
             Base.List.iter targs_to_infer ~f:(fun t ->
                 Flow.unify cx ~use_op (AnyT.why src reason) t
             )
           | l ->
             let iterable =
               if async then
                 Flow.get_builtin_typeapp
                   cx
                   (mk_reason (RCustom "async iteration expected on AsyncIterable") loc)
                   "$IterableOrAsyncIterableInternal"
                   (l :: targs_to_infer)
               else
                 Flow.get_builtin_typeapp
                   cx
                   (mk_reason (RCustom "iteration expected on Iterable") loc)
                   "$Iterable"
                   targs_to_infer
             in
             Flow.flow cx (l, UseT (use_op, iterable))
           )

  let non_exhaustive cx ts =
    Base.List.exists ts ~f:(fun t ->
        Flow.possible_concrete_types_for_inspection cx (reason_of_t t) t
        |> Base.List.is_empty
        |> not
    )

  let assert_operator_receiver_base cx ~op_reason ~obj_reason obj prop =
    match (obj, prop) with
    | (AnyT _, _)
    | (DefT (_, (EmptyT | NullT | VoidT)), _)
    | (DefT (_, ArrT (ROArrayAT _ | ArrayAT _)), _) ->
      ()
    | (DefT (_, ObjT { props_tmap; _ }), Some prop) when Context.has_prop cx props_tmap prop ->
      Flow_js_utils.add_output
        cx
        Error_message.(
          EIllegalAssertOperator { op = op_reason; obj = obj_reason; specialized = true }
        )
    | (DefT (_, ObjT { flags = { obj_kind = Indexed _; _ }; _ }), _) -> ()
    | _ ->
      Flow_js_utils.add_output
        cx
        Error_message.(
          EIllegalAssertOperator { op = op_reason; obj = obj_reason; specialized = true }
        )

  let check_specialized_assert_operator_property cx ~op_reason ~obj_reason t prop =
    DistributeUnionIntersection.distribute
      cx
      t
      ~break_up_union:Flow.possible_concrete_types_for_operators_checking
      ~get_no_match_error_loc:loc_of_reason
      ~check_base:(fun cx t ->
        assert_operator_receiver_base cx ~op_reason ~obj_reason t (Some (Reason.OrdinaryName prop))
    )

  let check_specialized_assert_operator_lookup =
    let check_base cx ~op_reason ~obj_reason (obj, prop) =
      match prop with
      | DefT (_, SingletonStrT { value; _ }) ->
        assert_operator_receiver_base cx ~op_reason ~obj_reason obj (Some value)
      | _ -> assert_operator_receiver_base cx ~op_reason ~obj_reason obj None
    in
    fun cx ~op_reason ~obj_reason t1 t2 ->
      DistributeUnionIntersection.distribute_2
        cx
        ~break_up_union:Flow.possible_concrete_types_for_operators_checking
        ~get_no_match_error_loc:(fun r1 r2 ->
          Flow_error.ordered_reasons (r1, r2) |> fst |> loc_of_reason)
        ~check_base:(fun cx -> check_base cx ~op_reason ~obj_reason)
        (t1, t2)

  let check_assert_operator_implicitly_nullable =
    let valid_target = function
      | AnyT _
      | DefT (_, (ObjT { flags = { obj_kind = Indexed _; _ }; _ } | ArrT (ArrayAT _ | ROArrayAT _)))
        ->
        true
      | _ -> false
    in
    fun cx t ->
      let ts = Flow.possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t) t in
      Base.List.map ~f:valid_target ts |> Base.List.fold ~f:( || ) ~init:false

  let check_assert_operator_nullable =
    let valid_target = function
      | AnyT _
      | DefT (_, (NullT | VoidT | MixedT (Mixed_everything | Mixed_non_null | Mixed_non_void))) ->
        true
      | _ -> false
    in
    fun cx t ->
      let ts = Flow.possible_concrete_types_for_operators_checking cx (TypeUtil.reason_of_t t) t in
      Base.List.map ~f:valid_target ts |> Base.List.fold ~f:( || ) ~init:false
end
