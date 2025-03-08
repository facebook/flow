(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Flow = Flow_js
module Ast = Flow_ast
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
      | ( DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)),
          DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _))
        )
      | ( DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)),
          DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _))
        )
      | ( DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _)),
          DefT (_, (BigIntGeneralT _ | BigIntT_UNSOUND _))
        )
      | (DefT (_, EmptyT), _)
      | (_, DefT (_, EmptyT))
      | (AnyT _, _)
      | (_, AnyT _) ->
        ()
      | (l, r) when Flow_js_utils.is_date l && Flow_js_utils.is_date r -> ()
      | (l, r) ->
        let reasons = Flow_error.ordered_reasons (TypeUtil.reason_of_t l, TypeUtil.reason_of_t r) in
        Flow_js_utils.add_output cx (Error_message.EComparison reasons)
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
            ( NumGeneralT _ | NumT_UNSOUND _ | StrGeneralT _ | StrT_UNSOUND _ | BoolGeneralT
            | BoolT_UNSOUND _ | SingletonNumT _ | SingletonStrT _ | SingletonBoolT _ | SymbolT
            | EnumObjectT _ | EnumValueT _ )
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
      | ( DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _)),
          DefT (_, (NumGeneralT _ | NumT_UNSOUND _ | SingletonNumT _))
        )
      | ( (DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)) | StrUtilT _),
          (DefT (_, (StrGeneralT _ | StrT_UNSOUND _ | SingletonStrT _)) | StrUtilT _)
        )
      | ( DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _)),
          DefT (_, (BoolGeneralT | BoolT_UNSOUND _ | SingletonBoolT _))
        )
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
    let strict_equatable_error encl_ctx (l, r) =
      let comparison_error =
        let open TypeUtil in
        lazy
          (match encl_ctx with
          | SwitchTest { case_test_loc; switch_discriminant_loc } ->
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
          | OtherTest ->
            let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
            Error_message.EComparison reasons)
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
        | SwitchTest _ -> Some (Lazy.force comparison_error)
        | NoContext
        | IndexContext
        | OtherTest ->
          None
      end
      (* We don't allow the comparison of enums and other types in general. *)
      | (DefT (_, EnumValueT _), _)
      | (_, DefT (_, EnumValueT _))
      | (DefT (_, EnumObjectT _), _)
      | (_, DefT (_, EnumObjectT _)) ->
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
          match strict_equatable_error encl_ctx (t1, t2) with
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

  let logical_and cx reason left right =
    Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun tout ->
        Flow.possible_concrete_types_for_inspection cx (TypeUtil.reason_of_t left) left
        |> Base.List.iter ~f:(fun left ->
               begin
                 match left with
                 | DefT (reason, (NumGeneralT _ | NumT_UNSOUND _)) ->
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
    Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun tout ->
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
    Tvar_resolver.mk_tvar_and_fully_resolve_no_wrap_where cx reason (fun tout ->
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
      | DefT (_, BoolT_UNSOUND false)
      | DefT (_, SingletonBoolT { value = false; _ })
      | DefT (_, StrT_UNSOUND (_, OrdinaryName ""))
      | DefT (_, SingletonStrT { value = OrdinaryName ""; _ })
      | DefT (_, NumT_UNSOUND (_, (0., _)))
      | DefT (_, SingletonNumT { value = (0., _); _ })
      | DefT (_, NullT)
      | DefT (_, VoidT) ->
        let reason = replace_desc_reason (RBooleanLit true) reason in
        DefT (reason, BoolT_UNSOUND true)
      (* !x when x is truthy *)
      | _ ->
        let reason = replace_desc_reason (RBooleanLit false) reason in
        DefT (reason, BoolT_UNSOUND false)
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
      | DefT (_, StrT_UNSOUND _)
      | DefT (_, SingletonStrT _) ->
        ()
      | DefT (_, NumGeneralT _)
      | DefT (_, NumT_UNSOUND _)
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
end
