(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module ALocMap = Loc_collections.ALocMap
module SpeculationKit = Speculation_kit.Make (Flow_js.FlowJs)
module Ast = Flow_ast

let force_lazy_tvars cx =
  Context.post_component_tvar_forcing_states cx
  |> Base.List.iter ~f:(fun s -> ignore @@ Context.force_fully_resolved_tvar cx s)

let detect_sketchy_null_checks cx tast =
  Exists_marker.mark cx tast;
  let add_error ~loc ~null_loc kind falsy_loc =
    Error_message.ESketchyNullLint { kind; loc; null_loc; falsy_loc } |> Flow_js.add_output cx
  in
  let detect_function exists_excuses loc exists_check =
    ExistsCheck.(
      let exists_excuse =
        Loc_collections.ALocMap.find_opt loc exists_excuses |> Base.Option.value ~default:empty
      in
      match exists_check.null_loc with
      | None -> ()
      | Some null_loc ->
        let add_error = add_error ~loc ~null_loc in
        if Base.Option.is_none exists_excuse.bool_loc then
          Base.Option.iter exists_check.bool_loc ~f:(add_error Lints.SketchyNullBool);
        if Base.Option.is_none exists_excuse.number_loc then
          Base.Option.iter exists_check.number_loc ~f:(add_error Lints.SketchyNullNumber);
        if Base.Option.is_none exists_excuse.bigint_loc then
          Base.Option.iter exists_check.bigint_loc ~f:(add_error Lints.SketchyNullBigInt);
        if Base.Option.is_none exists_excuse.string_loc then
          Base.Option.iter exists_check.string_loc ~f:(add_error Lints.SketchyNullString);
        if Base.Option.is_none exists_excuse.mixed_loc then
          Base.Option.iter exists_check.mixed_loc ~f:(add_error Lints.SketchyNullMixed);
        if Base.Option.is_none exists_excuse.enum_bool_loc then
          Base.Option.iter exists_check.enum_bool_loc ~f:(add_error Lints.SketchyNullEnumBool);
        if Base.Option.is_none exists_excuse.enum_number_loc then
          Base.Option.iter exists_check.enum_number_loc ~f:(add_error Lints.SketchyNullEnumNumber);
        if Base.Option.is_none exists_excuse.enum_bigint_loc then
          Base.Option.iter exists_check.enum_bigint_loc ~f:(add_error Lints.SketchyNullEnumBigInt);
        if Base.Option.is_none exists_excuse.enum_string_loc then
          Base.Option.iter exists_check.enum_string_loc ~f:(add_error Lints.SketchyNullEnumString);
        ()
    )
  in
  let exists_checks =
    let open Loc_collections in
    let open ExistsCheck in
    let checks = Context.exists_checks cx in
    if not @@ ALocMap.is_empty checks then
      let rec make_checks seen cur_checks loc t =
        let open Type in
        let open TypeUtil in
        let open Reason in
        match t with
        | AnnotT (_, t, _) -> make_checks seen cur_checks loc t
        | OpenT (_, id) when ISet.mem id seen -> cur_checks
        | OpenT (_, id) ->
          Context.find_resolved cx t
          |> Base.Option.value_map
               ~f:(make_checks (ISet.add id seen) cur_checks loc)
               ~default:cur_checks
        (* Ignore AnyTs for sketchy null checks; otherwise they'd always trigger the lint. *)
        | AnyT _ -> cur_checks
        | GenericT { bound = t; _ }
        | OpaqueT (_, { underlying_t = Some t; _ })
        | OpaqueT (_, { underlying_t = None; upper_t = Some t; _ }) ->
          make_checks seen cur_checks loc t
        | MaybeT (r, t) ->
          let acc = make_checks seen cur_checks loc t in
          let acc = make_checks seen acc loc (NullT.why r) in
          make_checks seen acc loc (VoidT.why r)
        | OptionalT { reason = r; type_ = t; _ } ->
          let acc = make_checks seen cur_checks loc t in
          make_checks seen acc loc (VoidT.why r)
        | UnionT (_, rep) ->
          UnionRep.members rep
          |> Base.List.fold ~f:(fun acc t -> make_checks seen acc loc t) ~init:cur_checks
        | _ ->
          let t_loc =
            let reason = reason_of_t t in
            match annot_loc_of_reason reason with
            | Some loc -> Some loc
            | None -> Some (def_loc_of_reason reason)
          in
          let exists_check =
            ALocMap.find_opt loc cur_checks |> Base.Option.value ~default:ExistsCheck.empty
          in
          let exists_check =
            match Type_filter.maybe cx t with
            | Type_filter.TypeFilterResult { type_ = DefT (_, EmptyT); changed = _ } -> exists_check
            | _ -> { exists_check with null_loc = t_loc }
          in
          let type_of_filtering_result (Type_filter.TypeFilterResult { type_; changed = _ }) =
            type_
          in
          let exists_check =
            match
              t
              |> Type_filter.not_truthy cx
              |> type_of_filtering_result
              |> Type_filter.not_maybe cx
              |> type_of_filtering_result
            with
            | DefT (_, BoolGeneralT)
            | DefT (_, SingletonBoolT _) ->
              { exists_check with bool_loc = t_loc }
            | DefT (_, StrGeneralT _)
            | DefT (_, SingletonStrT _) ->
              { exists_check with string_loc = t_loc }
            | DefT (_, NumGeneralT _)
            | DefT (_, SingletonNumT _) ->
              { exists_check with number_loc = t_loc }
            | DefT (_, BigIntGeneralT _) -> { exists_check with bigint_loc = t_loc }
            | DefT (_, MixedT _) -> { exists_check with mixed_loc = t_loc }
            | DefT
                ( _,
                  EnumValueT
                    (ConcreteEnum
                      { representation_t = DefT (_, (BoolGeneralT | SingletonBoolT _)); _ }
                      )
                )
            | DefT
                ( _,
                  EnumValueT
                    (AbstractEnum
                      { representation_t = DefT (_, (BoolGeneralT | SingletonBoolT _)); _ }
                      )
                ) ->
              { exists_check with enum_bool_loc = t_loc }
            | DefT
                ( _,
                  EnumValueT
                    (ConcreteEnum
                      { representation_t = DefT (_, (StrGeneralT _ | SingletonStrT _)); _ }
                      )
                )
            | DefT
                ( _,
                  EnumValueT
                    (AbstractEnum
                      { representation_t = DefT (_, (StrGeneralT _ | SingletonStrT _)); _ }
                      )
                ) ->
              { exists_check with enum_string_loc = t_loc }
            | DefT
                ( _,
                  EnumValueT
                    (ConcreteEnum
                      { representation_t = DefT (_, (NumGeneralT _ | SingletonNumT _)); _ }
                      )
                )
            | DefT
                ( _,
                  EnumValueT
                    (AbstractEnum
                      { representation_t = DefT (_, (NumGeneralT _ | SingletonNumT _)); _ }
                      )
                ) ->
              { exists_check with enum_number_loc = t_loc }
            | DefT
                (_, EnumValueT (ConcreteEnum { representation_t = DefT (_, BigIntGeneralT _); _ }))
            | DefT
                (_, EnumValueT (AbstractEnum { representation_t = DefT (_, BigIntGeneralT _); _ }))
              ->
              { exists_check with enum_bigint_loc = t_loc }
            | _ -> exists_check
          in
          if exists_check = ExistsCheck.empty then
            cur_checks
          else
            ALocMap.add loc exists_check cur_checks
      in

      ALocMap.fold
        (fun loc tset acc ->
          Type.TypeSet.fold (fun t acc -> make_checks ISet.empty acc loc t) tset acc)
        checks
        ALocMap.empty
    else
      ALocMap.empty
  in

  Loc_collections.ALocMap.iter (detect_function (Context.exists_excuses cx)) exists_checks

let detect_test_prop_misses cx =
  let misses = Context.test_prop_get_never_hit cx in
  Base.List.iter
    ~f:(fun (prop_name, (reason_prop, reason_obj), use_op, suggestion) ->
      Flow_js.add_output
        cx
        (Error_message.EPropNotFoundInLookup
           { prop_name; reason_prop; reason_obj; use_op; suggestion }
        ))
    misses

type truthyness_result =
  | ConstCond_Truthy of {
      constant_condition_kind: Flow_intermediate_error_types.constant_condition_kind;
      reason: Reason.t option;
    }
  | ConstCond_Falsy of {
      constant_condition_kind: Flow_intermediate_error_types.constant_condition_kind;
      reason: Reason.t option;
    }
  | ConstCond_Unknown
  | ConstCond_Unconcretized

let try_eval_concrete_type_truthyness cx t =
  let open Type in
  let open Flow_intermediate_error_types in
  match t with
  | OpenT _
  | EvalT _
  | TypeAppT _
  | GenericT _
  | AnnotT _
  | MaybeT _
  | OptionalT _
  | UnionT _ ->
    ConstCond_Unconcretized
  | DefT (reason, NumGeneralT literal) ->
    (match literal with
    | Truthy ->
      ConstCond_Truthy { constant_condition_kind = ConstCond_General; reason = Some reason }
    (* might be null if it's from member access due to flow soundness hole
         or internal flow bugs *)
    | AnyLiteral -> ConstCond_Unknown)
  | DefT (reason, StrGeneralT literal) ->
    (* we don't know the exact string literal but we might know the truthyness from refinement *)
    (match literal with
    | Truthy ->
      ConstCond_Truthy { constant_condition_kind = ConstCond_General; reason = Some reason }
      (* might be null if it's from member access due to flow soundness hole
         or internal flow bugs *)
    | AnyLiteral -> ConstCond_Unknown (* it could be any string *))
  | DefT (_, BoolGeneralT) -> ConstCond_Unknown
  | DefT (reason, BigIntGeneralT literal) ->
    (match literal with
    | Truthy ->
      ConstCond_Truthy { constant_condition_kind = ConstCond_General; reason = Some reason }
    (* might be null if it's from member access due to flow soundness hole
         or internal flow bugs *)
    | AnyLiteral -> ConstCond_Unknown)
  | DefT (_, EmptyT) -> ConstCond_Unknown
  | DefT (_, MixedT Mixed_everything) -> ConstCond_Unknown
  | DefT (_, MixedT Mixed_truthy) -> ConstCond_Unknown
  | DefT (_, MixedT Mixed_non_maybe) -> ConstCond_Unknown
  | DefT (_, MixedT Mixed_non_null) -> ConstCond_Unknown
  | DefT (_, MixedT Mixed_non_void) -> ConstCond_Unknown
  | DefT (_, MixedT Mixed_function) -> ConstCond_Unknown
  | DefT (reason, NullT)
  | DefT (reason, VoidT) ->
    if Context.enable_constant_condition_null_void cx then
      ConstCond_Falsy { constant_condition_kind = ConstCond_General; reason = Some reason }
    else
      ConstCond_Unknown
  | DefT (_, SymbolT) -> ConstCond_Unknown
  | DefT (reason, FunT _) ->
    if Context.enable_constant_condition_function cx then
      ConstCond_Truthy { constant_condition_kind = UncalledFunction; reason = Some reason }
    else
      ConstCond_Unknown
  | DefT (_, ObjT _) -> ConstCond_Unknown
  | DefT (_, ArrT _) -> ConstCond_Unknown
  | DefT (_, ClassT _) -> ConstCond_Unknown
  | DefT (reason, InstanceT { inst; _ }) ->
    let { class_id; _ } = inst in
    if Flow_js_utils.is_builtin_class_id "Promise" class_id cx then
      ConstCond_Truthy { constant_condition_kind = UnawaitedPromise; reason = Some reason }
    else
      ConstCond_Unknown
  | DefT (reason, SingletonStrT { value; _ }) ->
    if Reason.display_string_of_name value = "" then
      ConstCond_Falsy { constant_condition_kind = ConstCond_General; reason = Some reason }
    else
      ConstCond_Unknown
  | DefT (_, NumericStrKeyT _) -> ConstCond_Unknown
  | DefT (reason, SingletonNumT { value; _ }) ->
    let (number_value, _) = value in
    if number_value = 0. then
      ConstCond_Falsy { constant_condition_kind = ConstCond_General; reason = Some reason }
    else
      ConstCond_Unknown
  | DefT (reason, SingletonBoolT { value; _ }) ->
    if not (Context.enable_constant_condition_boolean_literal cx) then
      ConstCond_Unknown
    else if value then
      ConstCond_Truthy { constant_condition_kind = ConstCond_General; reason = Some reason }
    else
      ConstCond_Falsy { constant_condition_kind = ConstCond_General; reason = Some reason }
  | DefT (reason, SingletonBigIntT { value; _ }) ->
    let (bigint_value, _) = value in
    (match bigint_value with
    | Some 0L ->
      ConstCond_Falsy { constant_condition_kind = ConstCond_General; reason = Some reason }
    | _ -> ConstCond_Unknown)
  | DefT (_, TypeT _) -> ConstCond_Unknown
  | DefT (_, PolyT _) -> ConstCond_Unknown
  | DefT (_, ReactAbstractComponentT _) -> ConstCond_Unknown
  | DefT (_, RendersT _) -> ConstCond_Unknown
  | DefT (_, EnumValueT _) -> ConstCond_Unknown
  | DefT (_, EnumObjectT _) -> ConstCond_Unknown
  | ThisInstanceT _ -> ConstCond_Unknown
  | ThisTypeAppT _ -> ConstCond_Unknown
  | ObjProtoT _ -> ConstCond_Unknown
  | FunProtoT _ -> ConstCond_Unknown
  | NullProtoT _ -> ConstCond_Unknown
  | FunProtoBindT _ -> ConstCond_Unknown
  | IntersectionT _ -> ConstCond_Unknown
  | KeysT _ -> ConstCond_Unknown
  | StrUtilT _ -> ConstCond_Unknown
  | OpaqueT _ -> ConstCond_Unknown
  | NamespaceT _ -> ConstCond_Unknown
  | AnyT _ -> ConstCond_Unknown

let try_eval_type_truthyness cx t =
  let concrete_types = Flow_js.all_possible_concrete_types cx (TypeUtil.reason_of_t t) t in
  match Base.List.map concrete_types ~f:(try_eval_concrete_type_truthyness cx) with
  | [] -> ConstCond_Unknown
  | [result] -> result
  | results ->
    if
      Base.List.for_all results ~f:(function
          | ConstCond_Truthy _ -> true
          | _ -> false
          )
    then
      (* we don't pass on the error kind when there's multiple types *)
      ConstCond_Truthy
        { constant_condition_kind = Flow_intermediate_error_types.ConstCond_General; reason = None }
    else if
      Base.List.for_all results ~f:(function
          | ConstCond_Falsy _ -> true
          | _ -> false
          )
    then
      (* we don't pass on the error kind when there's multiple types *)
      ConstCond_Falsy
        { constant_condition_kind = Flow_intermediate_error_types.ConstCond_General; reason = None }
    else
      ConstCond_Unknown

type check_condition_result =
  | ConditionAllowed
  | ConditionBanned of {
      is_truthy: bool;
      show_warning: bool;
      constant_condition_kind: Flow_intermediate_error_types.constant_condition_kind;
      reason: Reason.t option;
    }

let condition_banned_and_TRUTHY =
  ConditionBanned
    {
      is_truthy = true;
      show_warning = false;
      constant_condition_kind = Flow_intermediate_error_types.ConstCond_General;
      reason = None;
    }

let condition_banned_and_FALSY =
  ConditionBanned
    {
      is_truthy = false;
      show_warning = false;
      constant_condition_kind = Flow_intermediate_error_types.ConstCond_General;
      reason = None;
    }

let condition_allowed = ConditionAllowed

let use_type_to_check_conditional cx ttype =
  (* We always show warning for errors generated from type inference *)
  match try_eval_type_truthyness cx ttype with
  | ConstCond_Truthy { constant_condition_kind; reason } ->
    ConditionBanned { is_truthy = true; show_warning = true; constant_condition_kind; reason }
  | ConstCond_Falsy { constant_condition_kind; reason } ->
    ConditionBanned { is_truthy = false; show_warning = true; constant_condition_kind; reason }
  | ConstCond_Unknown -> ConditionAllowed
  (* ConstCond_Unconcretized shouldn't happen because we call `all_possible_concrete_types` to concretize all possible types*)
  | ConstCond_Unconcretized -> ConditionAllowed

let rec check_conditional
    ?(should_report_error = true) cx e (cached_results : check_condition_result ALocMap.t ref) :
    check_condition_result =
  let should_report_error_ref = ref should_report_error in
  let ((loc, ttype), exp) = e in
  match ALocMap.find_opt loc !cached_results with
  | Some result -> result
  | None ->
    let open Ast.Expression in
    let check_condition_result =
      match exp with
      | StringLiteral lit ->
        if lit.Ast.StringLiteral.value = "" then
          condition_banned_and_FALSY
        else
          condition_banned_and_TRUTHY
      | BooleanLiteral _ ->
        (* true/false is allowed because it's likely intentional
           and could be useful for debugging / iterating *)
        condition_allowed
      | NullLiteral _ -> condition_banned_and_FALSY
      | NumberLiteral lit ->
        if lit.Ast.NumberLiteral.value <> 0. && lit.Ast.NumberLiteral.value <> 1. then
          condition_banned_and_TRUTHY
        else
          condition_allowed
      | BigIntLiteral lit ->
        (match lit.Ast.BigIntLiteral.value with
        | Some (0L | 1L) -> condition_allowed
        | _ -> condition_banned_and_TRUTHY)
      | RegExpLiteral _ -> condition_banned_and_TRUTHY
      | ModuleRefLiteral _ -> condition_banned_and_TRUTHY
      | Super _ -> condition_banned_and_TRUTHY
      | Update _ ->
        (* we don't check `++` and `--` because their value could change *)
        condition_allowed
      | Logical { Logical.operator; Logical.left; Logical.right; _ } ->
        (match operator with
        | Ast.Expression.Logical.Or
        | Ast.Expression.Logical.And ->
          let left_result = check_conditional cx left cached_results ~should_report_error:false in
          let right_result = check_conditional cx right cached_results ~should_report_error:false in
          (match (left_result, right_result) with
          | ( ConditionBanned
                {
                  is_truthy = left_truthy;
                  show_warning = left_show_warning;
                  constant_condition_kind = left_kind;
                  _;
                },
              ConditionBanned
                {
                  is_truthy = right_truthy;
                  show_warning = right_show_warning;
                  constant_condition_kind = right_kind;
                  _;
                }
            )
            when left_truthy = right_truthy ->
            ConditionBanned
              {
                is_truthy = left_truthy;
                show_warning = left_show_warning || right_show_warning;
                constant_condition_kind =
                  ( if left_kind = right_kind then
                    left_kind
                  else
                    Flow_intermediate_error_types.ConstCond_General
                  );
                reason = None;
              }
          | _ -> condition_allowed)
        (* NullishCoalesce is allowed here because the `lhs` is stored in `conditions`
           and we will check `lhs` in a separate call of check_conditional, so we don't need to
           recur to the lhs here. *)
        | Ast.Expression.Logical.NullishCoalesce -> condition_allowed)
      | TypeCast { TypeCast.expression; _ } ->
        should_report_error_ref := false;
        check_conditional cx expression cached_results ~should_report_error
      | AsConstExpression { AsConstExpression.expression; _ } ->
        should_report_error_ref := false;
        check_conditional cx expression cached_results ~should_report_error
      | TSSatisfies { TSSatisfies.expression; _ } ->
        should_report_error_ref := false;
        check_conditional cx expression cached_results ~should_report_error
      | Member _ -> condition_allowed
      | OptionalMember _ -> condition_allowed
      | Object _ -> condition_banned_and_TRUTHY
      | Array _ -> condition_banned_and_TRUTHY
      | New _ -> condition_banned_and_TRUTHY
      | Conditional { Conditional.consequent; alternate; _ } ->
        (* we don't need to check or unnest `test` here because they are unnested and checked
           in `expression` function *)
        let left_result =
          check_conditional cx consequent cached_results ~should_report_error:false
        in
        let right_result =
          check_conditional cx alternate cached_results ~should_report_error:false
        in
        (match (left_result, right_result) with
        | ( ConditionBanned
              {
                is_truthy = left_truthy;
                show_warning = left_show_warning;
                constant_condition_kind = left_kind;
                _;
              },
            ConditionBanned
              {
                is_truthy = right_truthy;
                show_warning = right_show_warning;
                constant_condition_kind = right_kind;
                _;
              }
          )
          when left_truthy = right_truthy ->
          ConditionBanned
            {
              is_truthy = left_truthy;
              show_warning = left_show_warning || right_show_warning;
              constant_condition_kind =
                ( if left_kind = right_kind then
                  left_kind
                else
                  Flow_intermediate_error_types.ConstCond_General
                );
              reason = None;
            }
        | _ -> condition_allowed)
      | Sequence { Sequence.expressions; _ } ->
        let last = Base.List.last_exn expressions in
        should_report_error_ref := false;
        check_conditional cx last cached_results ~should_report_error
      | Function _ -> condition_banned_and_TRUTHY
      | ArrowFunction _ -> condition_banned_and_TRUTHY
      | TaggedTemplate _ -> condition_banned_and_TRUTHY
      | TemplateLiteral _ -> condition_banned_and_TRUTHY
      | JSXElement _ -> condition_banned_and_TRUTHY
      | JSXFragment _ -> condition_banned_and_TRUTHY
      | Class _ -> condition_banned_and_TRUTHY
      | Yield _ -> condition_banned_and_TRUTHY
      | Import _ -> condition_banned_and_TRUTHY
      | Unary { Unary.operator; argument; _ } ->
        (match operator with
        | Ast.Expression.Unary.Not ->
          let arg_eval_result =
            check_conditional cx argument cached_results ~should_report_error:false
          in
          (match arg_eval_result with
          | ConditionBanned { is_truthy; show_warning; constant_condition_kind; reason } ->
            ConditionBanned
              { is_truthy = not is_truthy; show_warning; constant_condition_kind; reason }
          | ConditionAllowed -> ConditionAllowed)
        | Ast.Expression.Unary.Minus
        | Ast.Expression.Unary.Plus
        | Ast.Expression.Unary.BitNot
        | Ast.Expression.Unary.Typeof ->
          check_conditional cx argument cached_results ~should_report_error:false
        | Ast.Expression.Unary.Void -> condition_banned_and_FALSY
        | Ast.Expression.Unary.Delete
        | Ast.Expression.Unary.Nonnull ->
          use_type_to_check_conditional cx ttype
        | Ast.Expression.Unary.Await -> condition_allowed)
      | Assignment { Assignment.operator; left = _; right; comments = _ } ->
        (match operator with
        (* vanilla assignment (e.g. `a='test_str'`) is represented by `None` *)
        | None ->
          should_report_error_ref := false;
          check_conditional cx right cached_results ~should_report_error
          (* we don't check compound assignment (e.g. `++`, `--`, `+=`, ...) because their value could change *)
        | Some _ -> condition_allowed)
      | AsExpression _
      | MetaProperty _
      | Call _
      | OptionalCall _
      | Match _
      | Binary _
      | Identifier _
      | This _ ->
        use_type_to_check_conditional cx ttype
    in
    if !should_report_error_ref then (
      cached_results := ALocMap.add loc check_condition_result !cached_results;
      check_condition_result
    ) else
      check_condition_result

let detect_constant_conditions cx =
  let all_conditions = Context.get_all_conditions cx in
  let all_condition_results : check_condition_result ALocMap.t ref = ref ALocMap.empty in
  Base.List.iter
    ~f:(fun condition -> ignore (check_conditional cx condition all_condition_results))
    all_conditions;
  let banned_conditions =
    ALocMap.fold
      (fun loc check_condition_result acc ->
        match check_condition_result with
        | ConditionAllowed -> acc
        | ConditionBanned { is_truthy; show_warning; constant_condition_kind; reason } ->
          (loc, is_truthy, show_warning, constant_condition_kind, reason) :: acc)
      !all_condition_results
      []
  in
  Base.List.iter
    ~f:(fun (loc, is_truthy, show_warning, constant_condition_kind, reason) ->
      Flow_js_utils.add_output
        cx
        (Error_message.EConstantCondition
           { loc; is_truthy; show_warning; constant_condition_kind; reason }
        ))
    banned_conditions

type strict_comparison_result = {
  l_reason: Reason.t;
  r_reason: Reason.t;
  l_singleton_reason: Reason.t;
  r_singleton_reason: Reason.t;
  primary_loc: ALoc.t;
  kind: Flow_intermediate_error_types.strict_comparison_kind;
}

let check_strict_comparison cx all_strict_comparisons =
  Base.List.filter_map all_strict_comparisons ~f:(fun (loc, (left_ast, right_ast)) ->
      let ((_, left_t), _) = left_ast in
      let ((_, right_t), _) = right_ast in
      let l_reason = TypeUtil.reason_of_t left_t in
      let r_reason = TypeUtil.reason_of_t right_t in
      let left_conc_t = Flow_js.singleton_concrete_type_for_inspection cx l_reason left_t in
      let right_conc_t = Flow_js.singleton_concrete_type_for_inspection cx r_reason right_t in
      (* the reason after concretization will contain more information. *)
      let l_singleton_reason = TypeUtil.singleton_reason_of_t left_conc_t in
      let r_singleton_reason = TypeUtil.singleton_reason_of_t right_conc_t in
      let has_null_type cx t =
        match Type_filter.not_null cx t with
        | Type_filter.TypeFilterResult { changed = true; _ } -> true
        | _ -> false
      in
      let type_of_filtering_result (Type_filter.TypeFilterResult { type_; changed = _ }) = type_ in
      let open Type in
      let filter_maybe_and_check_is_subtyping cx left right =
        (* not_maybe will return emptyT if null or undefined is passed in.
           emptyT is a subtype of any type, so we don't need to check the AST node
           of `left` or `right`.
        *)
        let left_filtered = type_of_filtering_result (Type_filter.not_maybe cx left) in
        let right_filtered = type_of_filtering_result (Type_filter.not_maybe cx right) in
        let left_expanded = drop_generic left in
        let right_expanded = drop_generic right in
        Flow_js.FlowJs.speculative_subtyping_succeeds cx right_filtered left_expanded
        || Flow_js.FlowJs.speculative_subtyping_succeeds cx left_filtered right_expanded
      in
      let allowed = None in
      let banned =
        Some
          {
            l_reason;
            r_reason;
            l_singleton_reason;
            r_singleton_reason;
            primary_loc = loc;
            kind = Flow_intermediate_error_types.StrictComparisonGeneral;
          }
      in
      match (left_conc_t, right_conc_t) with
      | (AnyT _, _)
      | (_, AnyT _) ->
        allowed
      | (DefT (_, VoidT), _)
      | (_, DefT (_, VoidT)) ->
        allowed
      | (DefT (_, EmptyT), DefT (_, EmptyT)) -> allowed
      | (DefT (_, EmptyT), _) ->
        if Context.enable_invalid_comparison_general cx then
          Base.Option.map banned ~f:(fun banned ->
              {
                banned with
                kind = Flow_intermediate_error_types.StrictComparisonEmpty { empty_side = `Left };
              }
          )
        else
          allowed
      | (_, DefT (_, EmptyT)) ->
        if Context.enable_invalid_comparison_general cx then
          Base.Option.map banned ~f:(fun banned ->
              {
                banned with
                kind = Flow_intermediate_error_types.StrictComparisonEmpty { empty_side = `Right };
              }
          )
        else
          allowed
      | (other, DefT (_, NullT)) when not (has_null_type cx other) ->
        if Context.enable_invalid_comparison_null_check cx then
          Base.Option.map banned ~f:(fun banned ->
              {
                banned with
                kind = Flow_intermediate_error_types.StrictComparisonNull { null_side = `Right };
              }
          )
        else
          allowed
      | (DefT (_, NullT), other) when not (has_null_type cx other) ->
        if Context.enable_invalid_comparison_null_check cx then
          Base.Option.map banned ~f:(fun banned ->
              {
                banned with
                kind = Flow_intermediate_error_types.StrictComparisonNull { null_side = `Left };
              }
          )
        else
          allowed
      | _ ->
        if filter_maybe_and_check_is_subtyping cx left_conc_t right_conc_t then
          allowed
        else if Context.enable_invalid_comparison_general cx then
          banned
        else
          allowed
  )

let detect_invalid_strict_comparison cx =
  let all_strict_comparisons = Context.get_all_strict_comparisons cx in
  Base.List.iter
    ~f:(fun { l_reason; r_reason; l_singleton_reason; r_singleton_reason; primary_loc; kind } ->
      Flow_js.add_output
        cx
        (Error_message.EComparison
           {
             r1 = l_reason;
             r2 = r_reason;
             loc_opt = Some primary_loc;
             strict_comparison_opt =
               Some
                 {
                   Flow_intermediate_error_types.left_precise_reason = l_singleton_reason;
                   Flow_intermediate_error_types.right_precise_reason = r_singleton_reason;
                   Flow_intermediate_error_types.strict_comparison_kind = kind;
                 };
           }
        ))
    (check_strict_comparison cx all_strict_comparisons)

let detect_unnecessary_optional_chains cx =
  Base.List.iter
    ~f:(fun (loc, lhs_reason) ->
      Flow_js.add_output cx (Error_message.EUnnecessaryOptionalChain (loc, lhs_reason)))
    (Context.unnecessary_optional_chains cx)

let detect_unused_promises cx =
  Base.List.iter
    ~f:(fun (loc, t, async) ->
      let no_lowers r = Type.(AnyT.make Untyped r) in
      let t = Tvar_resolver.resolved_t ~no_lowers cx t in
      Flow_js.flow
        cx
        ( t,
          Type.CheckUnusedPromiseT
            { reason = Reason.mk_reason (Reason.RCustom "unused promise lint") loc; async }
        ))
    (Context.maybe_unused_promises cx)

let enforce_optimize cx loc t =
  let reason = Reason.mk_reason (Reason.RCustom "$Flow$EnforceOptimized") loc in
  let internal_t =
    let open Type in
    OpaqueT
      ( reason,
        {
          opaque_id = Opaque.InternalEnforceUnionOptimized;
          underlying_t = None;
          lower_t = None;
          upper_t = None;
          opaque_type_args = [];
          opaque_name = "InternalEnforceUnionOptimized";
        }
      )
  in
  Flow_js.flow_t cx (internal_t, t)

let check_union_opt cx = Context.iter_union_opt cx ~f:(enforce_optimize cx)

let detect_import_export_errors cx program metadata =
  Strict_es6_import_export.detect_errors cx program metadata;
  Module_exports_checker.check_program program |> Base.List.iter ~f:(Flow_js_utils.add_output cx)

let detect_non_voidable_properties cx =
  (* This function approximately checks whether VoidT can flow to the provided
   * type without actually creating the flow so as not to disturb type inference.
   * Even though this is happening post-merge, it is possible to encounter an
   * unresolved tvar, in which case it conservatively returns false.
   *)
  let rec is_voidable seen_ids =
    Type.(
      function
      | OpenT (_, id) ->
        (* tvar is recursive: conservatively assume it is non-voidable *)
        if ISet.mem id seen_ids then
          false
        else (
          match Flow_js_utils.possible_types cx id with
          (* tvar has no lower bounds: we conservatively assume it's non-voidable
           * except in the special case when it also has no upper bounds
           *)
          | [] -> Flow_js_utils.possible_uses cx id = []
          (* tvar is resolved: look at voidability of the resolved type *)
          | [t] -> is_voidable (ISet.add id seen_ids) t
          (* tvar is unresolved: conservatively assume it is non-voidable *)
          | _ -> false
        )
      (* a union is voidable if any of its members are voidable *)
      | UnionT (_, rep) -> UnionRep.members rep |> List.exists (is_voidable seen_ids)
      (* an intersection is voidable if all of its members are voidable *)
      | IntersectionT (_, rep) -> InterRep.members rep |> List.for_all (is_voidable seen_ids)
      (* trivially voidable *)
      | MaybeT _
      | DefT (_, (VoidT | MixedT (Mixed_everything | Mixed_non_null)))
      | OptionalT _
      | AnyT _ ->
        true
      (* conservatively assume all other types are non-voidable *)
      | _ -> false
    )
  in
  let check_properties (property_map : Type.Properties.id) :
      ALoc.t Property_assignment.error list SMap.t -> unit =
    let pmap = Context.find_props cx property_map in
    SMap.iter (fun name errors ->
        let should_error =
          match NameUtils.Map.find_opt (Reason.OrdinaryName name) pmap with
          | Some (Type.Field { type_ = t; _ }) -> not @@ is_voidable ISet.empty t
          | _ -> true
        in
        if should_error then
          List.iter
            (fun { Property_assignment.loc; desc } ->
              Flow_js.add_output cx (Error_message.EUninitializedInstanceProperty (loc, desc)))
            errors
    )
  in
  List.iter
    (fun {
           Context.public_property_map;
           private_property_map;
           errors = { Property_assignment.public_property_errors; private_property_errors };
         } ->
      check_properties public_property_map public_property_errors;
      check_properties private_property_map private_property_errors)
    (Context.voidable_checks cx)

let detect_matching_props_violations cx =
  let open Type in
  let peek =
    let open Type in
    let rec loop cx acc seen t =
      match t with
      | OpenT (_, id) ->
        let (root_id, constraints) = Context.find_constraints cx id in
        if ISet.mem root_id seen then
          acc
        else
          let seen = ISet.add root_id seen in
          (match constraints with
          | Constraint.Resolved t -> loop cx acc seen t
          | Constraint.FullyResolved s -> loop cx acc seen (Context.force_fully_resolved_tvar cx s)
          | Constraint.Unresolved bounds ->
            let ts = TypeMap.keys bounds.Constraint.lower in
            List.fold_left (fun a t -> loop cx a seen t) acc ts)
      | AnnotT (_, t, _) -> loop cx acc seen t
      | _ -> List.rev (t :: acc)
    in
    (fun cx t -> loop cx [] ISet.empty t)
  in
  let is_lit t =
    match drop_generic t with
    | DefT (_, (SingletonBoolT _ | SingletonStrT _ | SingletonNumT _)) -> true
    | _ -> false
  in
  let matching_props_checks =
    Base.List.filter_map (Context.matching_props cx) ~f:(fun (prop_name, sentinel, obj_t) ->
        match peek cx sentinel with
        (* Limit the check to primitive literal sentinels *)
        | [t] when is_lit t -> Some (TypeUtil.reason_of_t sentinel, prop_name, sentinel, obj_t)
        | _ -> None
    )
  in
  let step (reason, key, sentinel, obj) =
    let use_op =
      Op
        (MatchingProp
           {
             op = reason;
             obj = TypeUtil.reason_of_t obj;
             key;
             sentinel_reason = TypeUtil.reason_of_t sentinel;
           }
        )
    in
    let u =
      LookupT
        {
          reason;
          lookup_kind = NonstrictReturning (None, None);
          try_ts_on_failure = [];
          propref = TypeUtil.mk_named_prop ~reason (Reason.OrdinaryName key);
          lookup_action = MatchProp { use_op; drop_generic = true; prop_t = sentinel };
          method_accessible = true;
          ids = Some Properties.Set.empty;
          ignore_dicts = false;
        }
    in
    let obj_reason = TypeUtil.reason_of_t obj in
    (* If `obj` is a GenericT, we replace it with it's upper bound, since ultimately it will flow into
       `sentinel` rather than the other way around. *)
    let rec sentinel_check o =
      match
        Flow_js.possible_concrete_types_for_inspection cx obj_reason o
        |> Base.List.map ~f:drop_generic
        |> Base.List.bind ~f:(Flow_js.possible_concrete_types_for_inspection cx obj_reason)
        |> Base.List.filter ~f:(function
               | DefT (_, MixedT _)
               | DefT (_, NullT)
               | DefT (_, VoidT) ->
                 false
               | _ -> true
               )
      with
      | [] -> Flow_js.flow cx (EmptyT.make obj_reason, u)
      | [IntersectionT (_, rep)] -> InterRep.members rep |> Base.List.iter ~f:sentinel_check
      | [o] -> Flow_js.flow cx (o, u)
      | t0 :: t1 :: ts ->
        (* When the object type is a union, as long as one of them has a matching prop, it should be
         * good. (e.g. obj.type === 'a' when `typeof obj = {type: 'a'} | {type: 'b'}`). Therefore,
         * we turn unions into intersections below.
         * We have to invoke the speculation kit directly, so that we won't hit the logic of
         * non-speculating IntersectionT ~> LookupT handler. *)
        SpeculationKit.try_intersection
          cx
          DepthTrace.dummy_trace
          u
          obj_reason
          (InterRep.make t0 t1 ts)
    in
    sentinel_check obj
  in
  Base.List.iter ~f:step matching_props_checks

let detect_literal_subtypes =
  let open Type in
  let no_lowers _cx r = Type.Unsoundness.merged_any r in
  let rec unwrap = function
    | GenericT { bound; _ } -> unwrap bound
    | t -> t
  in
  fun cx ->
    let checks = Context.literal_subtypes cx in
    List.iter
      (fun (loc, check) ->
        let env = Context.environment cx in
        let u_def =
          match Type_env.provider_type_for_def_loc cx env loc with
          | OpenT (r, id) -> Flow_js_utils.merge_tvar ~filter_empty:true ~no_lowers cx r id
          | t -> t
        in
        let u_def = unwrap u_def in
        let l =
          match check with
          | Env_api.SingletonNum (lit_loc, num, raw) ->
            let reason = lit_loc |> Reason.(mk_reason (RNumberLit raw)) in
            DefT (reason, SingletonNumT { value = (num, raw); from_annot = false })
          | Env_api.SingletonBool (lit_loc, b) ->
            let reason = lit_loc |> Reason.(mk_reason (RBooleanLit b)) in
            DefT (reason, SingletonBoolT { value = b; from_annot = false })
          | Env_api.SingletonStr (lit_loc, str) ->
            let reason = lit_loc |> Reason.(mk_reason (RStringLit (OrdinaryName str))) in
            DefT (reason, SingletonStrT { value = Reason.OrdinaryName str; from_annot = false })
        in
        let use_op =
          Op
            (RefinementCheck
               { test = TypeUtil.reason_of_t l; discriminant = TypeUtil.reason_of_t u_def }
            )
        in
        Flow_js.flow cx (l, UseT (use_op, u_def)))
      checks

let check_polarity cx =
  Base.List.iter (Context.post_inference_polarity_checks cx) ~f:(fun (tparams, polarity, t) ->
      Check_polarity.check_polarity cx tparams polarity t
  )

let check_general_post_inference_validations cx =
  Base.List.iter (Context.post_inference_validation_flows cx) ~f:(fun pair -> Flow_js.flow cx pair)

let check_react_rules cx tast = React_rules.check_react_rules cx tast

let check_haste_provider_conflict cx tast =
  let file_options = Context.file_options cx in
  let filename = Context.file cx in
  match Files.haste_name_opt ~options:file_options filename with
  | None -> ()
  | Some haste_name ->
    (match
       let opts = Context.projects_options cx in
       Flow_projects.projects_bitset_of_path ~opts (File_key.to_string filename)
       |> Base.Option.bind
            ~f:(Flow_projects.individual_projects_bitsets_from_common_project_bitset ~opts)
     with
    | None -> ()
    | Some projects ->
      let pos = Loc.{ line = 1; column = 0 } in
      let loc_of_file f = Loc.{ source = Some f; start = pos; _end = pos } |> ALoc.of_loc in
      let add_duplicate_provider_error platform_specific_provider_file =
        Flow_js_utils.add_output
          cx
          (Error_message.EDuplicateModuleProvider
             {
               module_name = haste_name;
               conflict = loc_of_file filename;
               provider = loc_of_file platform_specific_provider_file;
             }
          )
      in
      if
        (* Suppose we have the setup of web project, native project, and web+native common code project.
         * We want to emit the same kinds of Haste module provider conflict error as if the same set of
         * code is covered by two flowconfigs.
         * (one include web only + common, one include native only + common) *)
        Files.has_flow_ext filename
      then
        (* We have Foo.js.flow in common code.

           1. If we have also have Foo.js in common code:
             a. We have Foo.js.flow or Foo.js in web code.
                This is not good, but we will complain anyways from Foo.js in common code.
             b. Nothing in web code. We are good
           2. If we don't have Foo.js in common code:
             a. We have Foo.js.flow or Foo.js in web code.
                This can be tolerated, because the Foo.js.flow file can act as common interface file.
             b. Nothing in web code. We are good.

           Therefore, in all possible cases, we don't have to emit an error.
        *)
        Base.List.iter projects ~f:(fun project ->
            match
              Context.find_require
                cx
                (Flow_import_specifier.HasteImportWithSpecifiedNamespace
                   {
                     namespace = Flow_projects.to_bitset project;
                     name = haste_name;
                     allow_implicit_platform_specific_import = false;
                   }
                )
            with
            | Context.MissingModule ->
              (* There is no corresponding implementation file. This is allowed. *)
              ()
            | Context.UncheckedModule platform_specific_provider_module_loc ->
              (* If the corresponding platform specific file is untyped, then we assume it satisfies
               * the common interface. However, we do need to make sure that it's not another
               * .js.flow file *)
              let platform_specific_provider_file =
                Base.Option.value_exn (ALoc.source platform_specific_provider_module_loc)
              in
              if Files.has_flow_ext platform_specific_provider_file then
                add_duplicate_provider_error platform_specific_provider_file
            | Context.TypedModule f ->
              (match f () with
              | Error t ->
                (* Similar to the case above,
                 * but in this case the module is any typed instead of untyped. *)
                let platform_specific_provider_file =
                  Base.Option.value_exn (ALoc.source (TypeUtil.loc_of_t t))
                in
                if Files.has_flow_ext platform_specific_provider_file then
                  add_duplicate_provider_error platform_specific_provider_file
              | Ok platform_specific_module_type ->
                let open Type in
                let get_exports_t ~is_common_interface_module reason m =
                  (* For conformance test, we only care about the value part *)
                  let (values_t, _) =
                    Flow_js_utils.ImportModuleNsTKit.on_ModuleT
                      cx
                      ~is_common_interface_module
                      (reason, false)
                      m
                  in
                  values_t
                in
                let (self_sig_loc, self_module_type) =
                  Module_info_analyzer.analyze_program cx tast
                in
                let prog_aloc = fst tast in
                let interface_t =
                  let reason = Reason.(mk_reason (RCustom "common interface") (fst tast)) in
                  get_exports_t ~is_common_interface_module:true reason self_module_type
                in
                let platform_specific_t =
                  get_exports_t
                    ~is_common_interface_module:false
                    platform_specific_module_type.Type.module_reason
                    platform_specific_module_type
                in
                (* We need to fully resolve the type to prevent tvar widening. *)
                Tvar_resolver.resolve cx interface_t;
                Tvar_resolver.resolve cx platform_specific_t;
                let use_op =
                  Op
                    (ConformToCommonInterface
                       { self_sig_loc; self_module_loc = prog_aloc; originate_from_import = false }
                    )
                in
                Flow_js.flow cx (platform_specific_t, UseT (use_op, interface_t)))
        )
      else
        (* We have Foo.js in common code.
         * We should error if we want Foo.js or Foo.js.flow in web only code. *)
        Base.List.iter projects ~f:(fun project ->
            let platform_specific_provider_module_loc =
              match
                Context.find_require
                  cx
                  (Flow_import_specifier.HasteImportWithSpecifiedNamespace
                     {
                       namespace = Flow_projects.to_bitset project;
                       name = haste_name;
                       allow_implicit_platform_specific_import = false;
                     }
                  )
              with
              | Context.MissingModule -> None
              | Context.UncheckedModule loc -> Some loc
              | Context.TypedModule f ->
                (match f () with
                | Ok { Type.module_reason; _ } -> Some (Reason.loc_of_reason module_reason)
                | Error t -> Some (TypeUtil.loc_of_t t))
            in
            Base.Option.iter
              platform_specific_provider_module_loc
              ~f:(fun platform_specific_provider_module_loc ->
                let platform_specific_provider_file =
                  Base.Option.value_exn (ALoc.source platform_specific_provider_module_loc)
                in
                add_duplicate_provider_error platform_specific_provider_file
            )
        ))

let validate_strict_boundary_import_pattern_opt_outs cx =
  let validate (error_loc, import_specifier, projects) =
    let get_exports_t specifier =
      match Context.find_require cx specifier with
      | Context.MissingModule -> None
      | Context.UncheckedModule _ -> None
      | Context.TypedModule f ->
        (match f () with
        | Ok ({ Type.module_reason; module_export_types; _ } as m) ->
          if module_export_types.Type.has_every_named_export then
            Some (Type.AnyT.at Type.Untyped error_loc)
          else
            (* For conformance test, we only care about the value part *)
            let (values_t, _) =
              Flow_js_utils.ImportModuleNsTKit.on_ModuleT
                cx
                ~is_common_interface_module:false
                (module_reason, false)
                m
            in
            Some values_t
        | Error _ -> None)
    in
    match get_exports_t (Flow_import_specifier.userland_specifier import_specifier) with
    | None ->
      (* If we cannot resolve the actual import, we will already error.
       * Therefore, we don't have to error again. *)
      ()
    | Some acting_common_interface_module_t ->
      let file_options = Context.file_options cx in
      let projects_options = Context.projects_options cx in
      let missing_platforms =
        Base.List.filter_map projects ~f:(fun project ->
            match
              get_exports_t
                (Flow_import_specifier.HasteImportWithSpecifiedNamespace
                   {
                     namespace = Flow_projects.to_bitset project;
                     name = import_specifier;
                     allow_implicit_platform_specific_import = true;
                   }
                )
            with
            | None ->
              (* If we cannot find the counterparts in other Haste namespaces, we need to error.
               * Consider the following example:
               *
               * ```
               * // file:common.js
               * import 'foo';
               *
               * // file: web/foo.js
               * // missing file: native/foo.js
               * ```
               *
               * The above example will cause us unable to find `foo` from `native` namespace.
               * We should error, because it will cause missing-module error in a flowconfig that
               * includes all of common code + `native/`.
               *)
              Platform_set.available_platforms
                ~file_options
                ~projects_options
                ~filename:(File_key.to_string (Context.file cx))
                ~explicit_available_platforms:
                  (Flow_projects.multi_platform_ambient_supports_platform_for_project
                     ~opts:projects_options
                     project
                  )
              |> Option.map (Platform_set.to_platform_string_set ~file_options)
            | Some alternative_module_t ->
              Tvar_resolver.resolve cx acting_common_interface_module_t;
              Tvar_resolver.resolve cx alternative_module_t;
              let open Type in
              let use_op =
                Op
                  (ConformToCommonInterface
                     {
                       self_sig_loc = error_loc;
                       self_module_loc = error_loc;
                       originate_from_import = true;
                     }
                  )
              in
              Flow_js.flow cx (alternative_module_t, UseT (use_op, acting_common_interface_module_t));
              None
        )
      in
      let missing_platforms = Base.List.fold missing_platforms ~init:SSet.empty ~f:SSet.union in
      if SSet.cardinal missing_platforms > 0 then
        let message =
          Error_message.EMissingPlatformSupport { loc = error_loc; missing_platforms }
        in
        Flow_js_utils.add_output cx message
  in
  Base.List.iter
    ~f:validate
    (Context.post_inference_projects_strict_boundary_import_pattern_opt_outs_validations cx)

let check_multiplatform_conformance cx ast tast =
  let (prog_aloc, _) = ast in
  let filename = Context.file cx in
  let file_options = Context.file_options cx in
  let file_loc = Loc.{ none with source = Some filename } |> ALoc.of_loc in
  match
    Files.relative_interface_mref_of_possibly_platform_specific_file ~options:file_options filename
  with
  | Some imported_interface_module_name ->
    let open Type in
    (match
       Context.find_require
         cx
         (Flow_import_specifier.userland_specifier imported_interface_module_name)
     with
    | Context.MissingModule
    | Context.UncheckedModule _ ->
      (* It's ok if a platform specific implementation file doesn't have an interface.
       * It just makes the module non-importable without platform extension. *)
      ()
    | Context.TypedModule interface_module_f ->
      let get_exports_t ~is_common_interface_module reason = function
        | Some m ->
          if
            is_common_interface_module
            && Platform_set.no_overlap
                 (Base.Option.value_exn (Context.available_platforms cx))
                 (Base.Option.value_exn m.module_available_platforms)
          then
            (* If the current module's platform has no overlap with the common interface
             * file's platforms, then the common interface file is irrelevant. We give it an
             * any type to make conformance check always passing. *)
            AnyT.make Untyped reason
          else
            (* For conformance test, we only care about the value part *)
            let (values_t, _) =
              Flow_js_utils.ImportModuleNsTKit.on_ModuleT
                cx
                ~is_common_interface_module
                (reason, false)
                m
            in
            values_t
        | None -> AnyT.make Untyped reason
      in
      let interface_t =
        let reason = Reason.(mk_reason (RCustom "common interface") prog_aloc) in
        get_exports_t
          ~is_common_interface_module:true
          reason
          (interface_module_f () |> Base.Result.ok)
      in
      let (self_sig_loc, self_module_type) = Module_info_analyzer.analyze_program cx tast in
      let self_t =
        get_exports_t
          ~is_common_interface_module:false
          self_module_type.Type.module_reason
          (Some self_module_type)
      in
      (* We need to fully resolve the type to prevent tvar widening. *)
      Tvar_resolver.resolve cx interface_t;
      Tvar_resolver.resolve cx self_t;
      let use_op =
        Op
          (ConformToCommonInterface
             { self_sig_loc; self_module_loc = prog_aloc; originate_from_import = false }
          )
      in
      Flow_js.flow cx (self_t, UseT (use_op, interface_t)))
  | None ->
    (match
       Platform_set.platform_specific_implementation_mrefs_of_possibly_interface_file
         ~file_options
         ~platform_set:(Context.available_platforms cx)
         ~file:filename
     with
    | None -> ()
    | Some (unconditional_extensions, grouped_extensions_with_conditional_extensions) ->
      let module_exists mref =
        match Context.find_require cx (Flow_import_specifier.userland_specifier mref) with
        | Context.TypedModule _
        | Context.UncheckedModule _ ->
          true
        | Context.MissingModule -> false
      in
      if
        (not (Context.has_explicit_supports_platform cx))
        && Base.List.for_all unconditional_extensions ~f:(fun m -> not (module_exists m))
        && Base.List.for_all
             grouped_extensions_with_conditional_extensions
             ~f:(fun (grouped, conditional) ->
               (not (module_exists grouped))
               && Base.List.for_all conditional ~f:(fun m -> not (module_exists m))
           )
      then
        (* We are fine if no implementation file exist.
         * The .js.flow file might be declaring a builtin module. *)
        ()
      else (
        Base.List.iter unconditional_extensions ~f:(fun name ->
            if not (module_exists name) then
              Flow_js_utils.add_output
                cx
                (Error_message.EPlatformSpecificImplementationModuleLookupFailed
                   { loc = file_loc; name }
                )
        );
        Base.List.iter
          grouped_extensions_with_conditional_extensions
          ~f:(fun (grouped, conditional) ->
            if not (module_exists grouped) then
              Base.List.iter conditional ~f:(fun name ->
                  if not (module_exists name) then
                    Flow_js_utils.add_output
                      cx
                      (Error_message.EPlatformSpecificImplementationModuleLookupFailed
                         { loc = file_loc; name }
                      )
              )
        )
      ))

let check_spread_prop_keys cx tast =
  let checker =
    object
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_type_annot x = x

      method on_loc_annot x = x

      method! jsx_spread_attribute attr =
        let { Ast.JSX.SpreadAttribute.argument = ((spread, ty), _); _ } = attr in
        let rec find seen ty =
          let open Type in
          match ty with
          | OpenT (_, id) when ISet.mem id seen -> ()
          | OpenT (_, id) ->
            Flow_js_utils.possible_types cx id |> Base.List.iter ~f:(find (ISet.add id seen))
          | AnnotT (_, t, _) -> find seen t
          | UnionT (_, rep) -> UnionRep.members rep |> Base.List.iter ~f:(find seen)
          | IntersectionT (_, rep) -> InterRep.members rep |> Base.List.iter ~f:(find seen)
          | OptionalT { type_; _ } -> find seen type_
          | MaybeT (_, ty) -> find seen ty
          | DefT (r, ObjT { props_tmap; _ }) ->
            let prop = Context.get_prop cx props_tmap (Reason.OrdinaryName "key") in
            begin
              match prop with
              | Some prop ->
                let loc =
                  Base.Option.value ~default:(Reason.loc_of_reason r) (Property.first_loc prop)
                in
                Flow_js.add_output cx Error_message.(EKeySpreadProp { spread; loc })
              | _ -> ()
            end
          | _ -> ()
        in
        find ISet.empty ty;
        super#jsx_spread_attribute attr
    end
  in
  if Context.ban_spread_key_props cx then
    let (_ : _ Ast.Program.t) = checker#program tast in
    ()

let check_match_exhaustiveness cx tast =
  let checker =
    object
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_type_annot x = x

      method on_loc_annot x = x

      method! match_ ~on_case_body x =
        let { Ast.Match.match_keyword_loc = match_loc; arg = ((_, arg_t), _); cases; _ } = x in
        let patterns =
          Base.List.map cases ~f:(function (_, { Ast.Match.Case.pattern; guard; _ }) ->
              (pattern, Base.Option.is_some guard)
              )
        in
        Exhaustive.analyze cx ~match_loc patterns arg_t;
        super#match_ ~on_case_body x
    end
  in
  if Context.enable_pattern_matching cx then
    let (_ : _ Ast.Program.t) = checker#program tast in
    ()

let emit_refinement_information_as_errors =
  let open Loc_collections in
  let emit_refined_locations_info cx =
    ALocMap.iter
      (fun refined_loc refining_locs ->
        Flow_js_utils.add_output
          cx
          Error_message.(
            EDevOnlyRefinedLocInfo { refined_loc; refining_locs = ALocSet.elements refining_locs }
          ))
      (Context.refined_locations cx)
  in
  let emit_invalidated_locations_info cx =
    ALocMap.iter
      (fun read_loc invalidation_info ->
        Flow_js_utils.add_output
          cx
          Error_message.(
            EDevOnlyInvalidatedRefinementInfo
              { read_loc; invalidation_info = ALocMap.elements invalidation_info }
          ))
      (Context.aggressively_invalidated_locations cx)
  in
  fun cx ->
    if Context.dev_only_refinement_info_as_errors cx then (
      emit_refined_locations_info cx;
      emit_invalidated_locations_info cx
    )

let check_assert_operator cx tast =
  let open Type_operation_utils in
  let check_specialized_assert_operator ~op_reason expr =
    let open Ast.Expression in
    let obj_reason = Reason.mk_typed_expression_reason expr in
    match expr with
    | ( _,
        ( Member
            {
              Member._object = ((_, obj_t), _);
              property = Member.PropertyIdentifier (_, { Ast.Identifier.name; _ });
              _;
            }
        | OptionalMember
            {
              OptionalMember.member =
                {
                  Member._object = ((_, obj_t), _);
                  property = Member.PropertyIdentifier (_, { Ast.Identifier.name; _ });
                  _;
                };
              _;
            } )
      ) ->
      TypeAssertions.check_specialized_assert_operator_property cx ~op_reason ~obj_reason obj_t name
    | ( _,
        ( Member
            {
              Member._object = ((_, obj_t), _);
              property = Member.PropertyExpression ((_, prop_t), _);
              _;
            }
        | OptionalMember
            {
              OptionalMember.member =
                {
                  Member._object = ((_, obj_t), _);
                  property = Member.PropertyExpression ((_, prop_t), _);
                  _;
                };
              _;
            } )
      ) ->
      TypeAssertions.check_specialized_assert_operator_lookup cx ~op_reason ~obj_reason obj_t prop_t
    | _ ->
      Flow_js_utils.add_output
        cx
        Error_message.(
          EIllegalAssertOperator { op = op_reason; obj = obj_reason; specialized = true }
        )
  in
  let check_assert_operator_useful ~op_reason expr =
    let obj_reason = Reason.mk_typed_expression_reason expr in
    let ((_, t), expr_node) = expr in
    let legal =
      TypeAssertions.check_assert_operator_nullable cx t
      ||
      match expr_node with
      | Ast.Expression.Member { Ast.Expression.Member._object; _ }
      | Ast.Expression.OptionalMember
          { Ast.Expression.OptionalMember.member = { Ast.Expression.Member._object; _ }; _ } ->
        let ((_, obj_t), _) = _object in
        TypeAssertions.check_assert_operator_implicitly_nullable cx obj_t
      | _ -> false
    in
    if not legal then
      Flow_js_utils.add_output
        cx
        Error_message.(
          EIllegalAssertOperator { op = op_reason; obj = obj_reason; specialized = false }
        )
  in
  let check ~op_reason expr =
    check_assert_operator_useful ~op_reason expr;
    if Context.assert_operator_specialized cx then check_specialized_assert_operator ~op_reason expr
  in
  let checker =
    object
      inherit
        [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

      method on_type_annot x = x

      method on_loc_annot x = x

      method! expression expr =
        begin
          let open Ast.Expression in
          match expr with
          | (_, Unary { Unary.operator = Unary.Nonnull; argument; _ }) ->
            let op_reason = Reason.mk_typed_expression_reason expr in
            check ~op_reason argument
          | ( _,
              OptionalCall
                {
                  OptionalCall.optional = OptionalCall.AssertNonnull;
                  call = { Call.callee = target; _ };
                  _;
                }
            )
          | ( _,
              OptionalMember
                {
                  OptionalMember.optional = OptionalMember.AssertNonnull;
                  member = { Member._object = target; _ };
                  _;
                }
            ) ->
            let op_reason = Reason.mk_typed_expression_reason target in
            check ~op_reason target
          | _ -> ()
        end;
        super#expression expr
    end
  in
  if Context.assert_operator_enabled cx then ignore (checker#program tast : (_, _) Ast.Program.t)

let get_lint_severities metadata strict_mode lint_severities =
  if metadata.Context.strict || metadata.Context.strict_local then
    StrictModeSettings.fold
      (fun lint_kind lint_severities ->
        LintSettings.set_value lint_kind (Severity.Err, None) lint_severities)
      strict_mode
      lint_severities
  else
    lint_severities

(* Post-merge errors.
 *
 * At this point, all dependencies have been merged and the component has been
 * linked together. Any constraints should have already been evaluated, which
 * means we can complain about things that either haven't happened yet, or
 * which require complete knowledge of tvar bounds.
 *)
let post_merge_checks cx ast tast metadata =
  force_lazy_tvars cx;
  check_react_rules cx tast;
  if not (Context.is_lib_file cx) then (
    check_haste_provider_conflict cx tast;
    check_multiplatform_conformance cx ast tast;
    validate_strict_boundary_import_pattern_opt_outs cx
  );
  check_polarity cx;
  check_general_post_inference_validations cx;
  detect_sketchy_null_checks cx tast;
  detect_non_voidable_properties cx;
  detect_test_prop_misses cx;
  detect_unnecessary_optional_chains cx;
  detect_constant_conditions cx;
  detect_import_export_errors cx ast metadata;
  if Context.enable_invalid_comparison_general cx || Context.enable_invalid_comparison_null_check cx
  then begin
    detect_invalid_strict_comparison cx
  end;
  if
    not
      (Context.enable_invalid_comparison_general cx
      && Context.enable_invalid_comparison_null_check cx
      )
  then begin
    detect_matching_props_violations cx;
    detect_literal_subtypes cx
  end;
  detect_unused_promises cx;
  check_union_opt cx;
  check_spread_prop_keys cx tast;
  check_match_exhaustiveness cx tast;
  check_assert_operator cx tast;
  emit_refinement_information_as_errors cx

(* Check will lazily create types for the checked file's dependencies. These
 * types are created in the dependency's context and need to be copied into the
 * checked file's context.
 *
 * This visitor walks a type in the dependency's context (src_cx) and copies
 * any tvars, property maps, evaluated types, etc. into the check file's context
 * (dst_cx).
 *
 * When calculating a direct dependency's types, we might also need to construct
 * types for a transitive dependency. These types are similarly created in the
 * transitive dependency's context, then copied into the dependency's context,
 * and so on.
 *
 * Finally, due to cycles, it's possible that src_cx and dst_cx share the same
 * component cx, and thus have the same tvar graph, property maps, etc. Happily,
 * this does not complicate the implementation, as the mem checks and early
 * returns on each method override are sufficient.
 *
 * Crucially, this copying process is shallow. We only copy what is necessary to
 * interpret a given type. *)
let copier =
  let open Type in
  let open Constraint in
  object (self)
    inherit [Context.t] Type_visitor.t as super

    (* Copying a tvar produces a FullyResolved tvar in the dst cx, which
     * contains an unevaluated thunk. The laziness here makes the copying
     * shallow. Note that the visitor stops at root tvars here and only resumes
     * if the thunk is forced. *)
    method! tvar src_cx pole dst_cx r id =
      let dst_graph = Context.graph dst_cx in
      if IMap.mem id dst_graph then
        dst_cx
      else
        let (root_id, constraints) = Context.find_constraints src_cx id in
        if id == root_id then (
          let state =
            match constraints with
            | Unresolved _
            | Resolved _ ->
              failwith "unexpected unresolved constraint"
            | FullyResolved s ->
              ForcingState.copy
                ~on_error:(Context.on_cyclic_tvar_error src_cx)
                ~visit_for_copier:(fun t ->
                  let (_ : Context.t) = self#type_ src_cx pole dst_cx t in
                  ())
                s
          in
          let node = create_root (FullyResolved state) in
          Context.set_graph dst_cx (IMap.add id node dst_graph);
          dst_cx
        ) else
          let node = create_goto root_id in
          Context.set_graph dst_cx (IMap.add id node dst_graph);
          self#tvar src_cx pole dst_cx r root_id

    method! props src_cx pole dst_cx id =
      let dst_property_maps = Context.property_maps dst_cx in
      if Properties.Map.mem id dst_property_maps then
        dst_cx
      else
        let props = Context.find_props src_cx id in
        Context.set_property_maps dst_cx (Properties.Map.add id props dst_property_maps);
        super#props src_cx pole dst_cx id

    method! call_prop src_cx pole dst_cx id =
      let dst_call_props = Context.call_props dst_cx in
      if IMap.mem id dst_call_props then
        dst_cx
      else
        let t = Context.find_call src_cx id in
        Context.set_call_props dst_cx (IMap.add id t dst_call_props);
        super#call_prop src_cx pole dst_cx id

    method! exports src_cx pole dst_cx id =
      let dst_export_maps = Context.export_maps dst_cx in
      if Exports.Map.mem id dst_export_maps then
        dst_cx
      else
        let map = Context.find_exports src_cx id in
        Context.set_export_maps dst_cx (Exports.Map.add id map dst_export_maps);
        super#exports src_cx pole dst_cx id

    method! eval_id src_cx pole dst_cx id =
      match Eval.Map.find_opt id (Context.evaluated src_cx) with
      | None -> dst_cx
      | Some t ->
        let dst_evaluated = Context.evaluated dst_cx in
        if Eval.Map.mem id dst_evaluated then
          dst_cx
        else (
          Context.set_evaluated dst_cx (Eval.Map.add id t dst_evaluated);
          super#eval_id src_cx pole dst_cx id
        )
  end

let copy_into dst_cx src_cx f () =
  let m = f () in
  match m with
  | Ok ({ Type.module_export_types; _ } as m) ->
    let (_ : Context.t) = copier#export_types src_cx Polarity.Positive dst_cx module_export_types in
    Ok m
  | Error _ -> m

let copied dst_cx src_cx t =
  let (_ : Context.t) = copier#type_ src_cx Polarity.Positive dst_cx t in
  t

let module_type_copied dst_cx src_cx ({ Type.module_export_types; _ } as m) =
  let (_ : Context.t) = copier#export_types src_cx Polarity.Positive dst_cx module_export_types in
  m

let merge_libs_from_ordered_asts ~sig_opts ordered_asts =
  let (builtin_errors, builtin_locs, builtins) =
    Type_sig_utils.parse_and_pack_builtins sig_opts ordered_asts
  in
  let builtin_errors =
    builtin_errors
    |> Base.List.filter_map ~f:(function
           | Type_sig.SigError e ->
             let e =
               Signature_error.map
                 (fun l -> l |> Type_sig_collections.Locs.get builtin_locs |> ALoc.of_loc)
                 e
             in
             let msg = Error_message.ESignatureVerification e in
             let source_file =
               Base.Option.value_exn
                 (msg |> Error_message.loc_of_msg |> Base.Option.bind ~f:ALoc.source)
             in
             Some (Flow_error.error_of_msg ~source_file msg)
           | Type_sig.BindingValidationError e ->
             let e =
               Signature_error.map_binding_validation_t
                 (fun l -> l |> Type_sig_collections.Locs.get builtin_locs |> ALoc.of_loc)
                 e
             in
             let msg = Error_message.ESignatureBindingValidation e in
             let source_file =
               Base.Option.value_exn
                 (msg |> Error_message.loc_of_msg |> Base.Option.bind ~f:ALoc.source)
             in
             Some (Flow_error.error_of_msg ~source_file msg)
           | Type_sig.CheckError -> None
           )
    |> Flow_error.ErrorSet.of_list
  in
  (builtin_errors, builtin_locs, builtins)

let merge_lib_files ~project_opts ~sig_opts ordered_asts_with_scoped_projects =
  let builtin_leader_file_key =
    match ordered_asts_with_scoped_projects with
    | [] -> None
    | (_, fst_ast) :: _ -> fst_ast |> fst |> Loc.source
  in
  let scoped_project_key_equal = Base.Option.equal Flow_projects.equal in
  let scoped_project_of_ordered_ast =
    let scoped_project_of_ordered_ast =
      Base.List.fold_right
        ordered_asts_with_scoped_projects
        ~init:[]
        ~f:(fun (scoped_project_key, ast) acc ->
          let scoped_project_key =
            Base.Option.map
              scoped_project_key
              ~f:(Flow_projects.bitset_of_project_string ~opts:project_opts)
          in
          match Base.List.Assoc.find acc ~equal:scoped_project_key_equal scoped_project_key with
          | None -> Base.List.Assoc.add ~equal:scoped_project_key_equal acc scoped_project_key [ast]
          | Some existing_list ->
            Base.List.Assoc.add
              ~equal:scoped_project_key_equal
              acc
              scoped_project_key
              (ast :: existing_list)
      )
    in
    let non_scoped_asts =
      Base.List.Assoc.find scoped_project_of_ordered_ast ~equal:scoped_project_key_equal None
    in
    (* Make every scoped asts include all of non-scoped asts at the end *)
    Base.List.map scoped_project_of_ordered_ast ~f:(function
        | (Some scoped_dir, ast_list) ->
          (match non_scoped_asts with
          | None -> (Some scoped_dir, ast_list)
          | Some non_scoped_asts -> (Some scoped_dir, Base.List.append ast_list non_scoped_asts))
        | entry -> entry
        )
  in
  let (scoped_builtins, builtin_errors) =
    let all_errors_ref = ref Flow_error.ErrorSet.empty in
    let scoped_builtins =
      Base.List.Assoc.map scoped_project_of_ordered_ast ~f:(fun ordered_asts ->
          let (builtin_errors, builtin_locs, builtins) =
            merge_libs_from_ordered_asts ~sig_opts ordered_asts
          in
          all_errors_ref := Flow_error.ErrorSet.union !all_errors_ref builtin_errors;
          Context.BuiltinGroup { builtin_locs; builtins }
      )
    in
    (scoped_builtins, !all_errors_ref)
  in
  let (unscoped_builtins, scoped_builtins) =
    Base.List.partition_map scoped_builtins ~f:(fun (scoped_dir_opt, builtins) ->
        match scoped_dir_opt with
        | None -> Base.Either.First builtins
        | Some scoped_dir -> Base.Either.Second (scoped_dir, builtins)
    )
  in
  let unscoped_builtins =
    match unscoped_builtins with
    | [unscoped_builtins] -> unscoped_builtins
    | _ -> failwith "There can be only one group of unscoped builtins"
  in
  match builtin_leader_file_key with
  | None -> (builtin_errors, Context.EmptyMasterContext)
  | Some builtin_leader_file_key ->
    ( builtin_errors,
      Context.NonEmptyMasterContext { builtin_leader_file_key; unscoped_builtins; scoped_builtins }
    )

let mk_builtins metadata master_cx =
  match master_cx with
  | Context.EmptyMasterContext -> (fun _ -> Builtins.empty ())
  | Context.NonEmptyMasterContext { builtin_leader_file_key; unscoped_builtins; scoped_builtins } ->
    let create_mapped_builtins (Context.BuiltinGroup { builtin_locs; builtins }) =
      let builtins_ref = ref (Builtins.empty ()) in
      let cx =
        Context.make
          (Context.make_ccx ())
          { metadata with Context.checked = false }
          builtin_leader_file_key
          (lazy (ALoc.empty_table builtin_leader_file_key))
          (fun _ -> Context.MissingModule)
          (fun _ -> !builtins_ref)
      in
      let (values, types, modules) =
        Type_sig_merge.merge_builtins cx builtin_leader_file_key builtin_locs builtins
      in
      let original_builtins =
        Builtins.of_name_map
          ~type_mapper:Base.Fn.id
          ~module_type_mapper:Base.Fn.id
          ~values
          ~types
          ~modules
      in
      builtins_ref := original_builtins;
      let mapped_builtins dst_cx =
        Builtins.of_name_map
          ~type_mapper:(copied dst_cx cx)
          ~module_type_mapper:(module_type_copied dst_cx cx)
          ~values
          ~types
          ~modules
      in
      mapped_builtins
    in
    let mapped_unscoped_builtins = create_mapped_builtins unscoped_builtins in
    let mapped_scoped_builtins = Base.List.Assoc.map scoped_builtins ~f:create_mapped_builtins in
    fun dst_cx ->
      let project =
        Flow_projects.projects_bitset_of_path
          ~opts:metadata.Context.projects_options
          (File_key.to_string (Context.file dst_cx))
      in
      (match
         (* With the scoped libdef feature,
          * the set of libdefs active for a given file might be different.
          * The correct set of builtins is chosen here. *)
         Base.List.find_map mapped_scoped_builtins ~f:(fun (scoped_project, mapped_builtins) ->
             if project = Some scoped_project then
               Some (mapped_builtins dst_cx)
             else
               None
         )
       with
      | None -> mapped_unscoped_builtins dst_cx
      | Some builtins -> builtins)
