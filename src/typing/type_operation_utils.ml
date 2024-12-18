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
open Utils_js

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

module Import_export = struct
  let concretize_module_type cx get_reason module_t =
    match Flow.possible_concrete_types_for_inspection cx get_reason module_t with
    | [ModuleT m] -> Ok m
    | [AnyT (lreason, any_source)] -> Error (lreason, any_source)
    | _ ->
      Flow_js_utils.add_output
        cx
        Error_message.(
          EInternal
            (loc_of_reason get_reason, UnexpectedModuleT (Debug_js.dump_t cx ~depth:3 module_t))
        );
      Error (get_reason, AnyError None)

  let check_platform_availability cx reason imported_module_available_platforms =
    let current_module_available_platforms = Context.available_platforms cx in
    match (current_module_available_platforms, imported_module_available_platforms) with
    | (None, None)
    | (None, Some _)
    | (Some _, None) ->
      ()
    | (Some required_platforms, Some available_platforms) ->
      let file_options = Context.((metadata cx).file_options) in
      let required_platforms =
        Platform_set.to_platform_string_set ~file_options required_platforms
      in
      let available_platforms =
        Platform_set.to_platform_string_set ~file_options available_platforms
      in
      let missing_platforms = SSet.diff required_platforms available_platforms in
      if SSet.cardinal missing_platforms > 0 then
        let loc = Reason.loc_of_reason reason in
        let message =
          Error_message.EMissingPlatformSupport { loc; available_platforms; required_platforms }
        in
        Flow_js_utils.add_output cx message

  let get_module_t
      cx
      ?(perform_platform_validation = false)
      ~import_kind_for_untyped_import_validation
      (loc, mref) =
    if Context.in_declare_module cx then
      Flow_js_utils.get_builtin_module cx mref loc
    else
      let module_t =
        match Context.find_require cx mref with
        | Context.TypedModule t -> t
        | Context.UncheckedModule (module_def_loc, mref) ->
          Base.Option.iter import_kind_for_untyped_import_validation ~f:(fun import_kind ->
              match import_kind with
              | ImportType
              | ImportTypeof ->
                let message = Error_message.EUntypedTypeImport (loc, mref) in
                Flow_js_utils.add_output cx message
              | ImportValue ->
                let message = Error_message.EUntypedImport (loc, mref) in
                Flow_js_utils.add_output cx message
          );
          AnyT.why Untyped (mk_reason (RModule mref) module_def_loc)
        | Context.MissingModule m_name -> Flow_js_utils.lookup_builtin_module_error cx m_name loc
      in
      let reason = Reason.(mk_reason (RCustom mref) loc) in
      let need_platform_validation =
        perform_platform_validation && Files.multi_platform Context.((metadata cx).file_options)
      in
      ( if need_platform_validation then
        match concretize_module_type cx reason module_t with
        | Ok m ->
          if need_platform_validation then
            check_platform_availability cx reason m.module_available_platforms
        | Error _ -> ()
      );
      module_t

  let singleton_concretize_type_for_imports_exports cx r t =
    match Flow.possible_concrete_types_for_imports_exports cx r t with
    | [] -> EmptyT.why r
    | [t] -> t
    | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)

  let assert_export_is_type cx name t =
    let reason = TypeUtil.reason_of_t t in
    Tvar_resolver.mk_tvar_and_fully_resolve_where cx reason (fun tout ->
        let t =
          t
          |> singleton_concretize_type_for_imports_exports cx reason
          |> Flow_js_utils.AssertExportIsTypeTKit.on_concrete_type cx name
        in
        Flow.flow_t cx (t, tout)
    )

  let get_imported_t
      cx ~import_reason ~module_name ~source_module_t ~import_kind ~remote_name ~local_name =
    let is_strict = Context.is_strict cx in
    let name_def_loc_ref = ref None in
    let t =
      let with_concretized_type cx r f t =
        f (singleton_concretize_type_for_imports_exports cx r t)
      in
      match concretize_module_type cx import_reason source_module_t with
      | Ok m ->
        let (name_loc_opt, t) =
          if remote_name = "default" then
            Flow_js_utils.ImportDefaultTKit.on_ModuleT
              cx
              ~with_concretized_type
              (import_reason, import_kind, (local_name, module_name), is_strict)
              m
          else
            Flow_js_utils.ImportNamedTKit.on_ModuleT
              cx
              ~with_concretized_type
              (import_reason, import_kind, remote_name, module_name, is_strict)
              m
        in
        name_def_loc_ref := name_loc_opt;
        t
      | Error (lreason, any_source) -> AnyT (lreason, any_source)
    in
    let name_def_loc = !name_def_loc_ref in
    (name_def_loc, t)

  let type_kind_of_kind = function
    | Ast.Statement.ImportDeclaration.ImportType -> Type.ImportType
    | Ast.Statement.ImportDeclaration.ImportTypeof -> Type.ImportTypeof
    | Ast.Statement.ImportDeclaration.ImportValue -> Type.ImportValue

  let import_named_specifier_type
      cx import_reason import_kind ~module_name ~source_module_t ~remote_name ~local_name =
    let import_kind = type_kind_of_kind import_kind in
    get_imported_t
      cx
      ~import_reason
      ~module_name
      ~source_module_t
      ~import_kind
      ~remote_name
      ~local_name

  let get_module_namespace_type cx reason ~namespace_symbol source_module_t =
    let is_strict = Context.is_strict cx in
    match concretize_module_type cx reason source_module_t with
    | Ok m ->
      let (values_type, types_tmap) =
        Flow_js_utils.ImportModuleNsTKit.on_ModuleT cx (reason, is_strict) m
      in
      NamespaceT { namespace_symbol; values_type; types_tmap }
    | Error (lreason, any_source) -> AnyT (lreason, any_source)

  let import_namespace_specifier_type
      cx import_reason import_kind ~module_name ~namespace_symbol ~source_module_t ~local_loc =
    let open Ast.Statement in
    match import_kind with
    | ImportDeclaration.ImportType -> assert_false "import type * is a parse error"
    | ImportDeclaration.ImportTypeof ->
      let module_ns_t =
        get_module_namespace_type cx import_reason ~namespace_symbol source_module_t
      in
      let bind_reason = repos_reason local_loc import_reason in
      Flow_js_utils.ImportTypeofTKit.on_concrete_type cx bind_reason "*" module_ns_t
    | ImportDeclaration.ImportValue ->
      let reason = mk_reason (RModule module_name) local_loc in
      let namespace_symbol = FlowSymbol.mk_module_symbol ~name:module_name ~def_loc:local_loc in
      get_module_namespace_type cx reason ~namespace_symbol source_module_t

  let import_default_specifier_type
      cx import_reason import_kind ~module_name ~source_module_t ~local_name =
    let import_kind = type_kind_of_kind import_kind in
    get_imported_t
      cx
      ~import_reason
      ~module_name
      ~source_module_t
      ~import_kind
      ~remote_name:"default"
      ~local_name

  let cjs_require_type
      cx reason ~namespace_symbol ~standard_cjs_esm_interop ~legacy_interop source_module_t =
    let is_strict = Context.is_strict cx in
    match concretize_module_type cx reason source_module_t with
    | Ok m ->
      Flow_js_utils.CJSRequireTKit.on_ModuleT
        cx
        ~reposition:Flow.reposition
        ~reason
        ~module_symbol:namespace_symbol
        ~is_strict
        ~standard_cjs_esm_interop
        ~legacy_interop
        m
    | Error (lreason, any_source) -> AnyT (lreason, any_source)
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
      | (DefT (_, (StrGeneralT _ | StrT_UNSOUND _)), DefT (_, (StrGeneralT _ | StrT_UNSOUND _)))
      | (DefT (_, (NumGeneralT _ | NumT_UNSOUND _)), DefT (_, (NumGeneralT _ | NumT_UNSOUND _)))
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
    let strict_equatable_error cond_context (l, r) =
      let comparison_error =
        let open TypeUtil in
        lazy
          (match cond_context with
          | Some (SwitchTest { case_test_loc; switch_discriminant_loc }) ->
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
          | _ ->
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
        match cond_context with
        | Some (SwitchTest _) -> Some (Lazy.force comparison_error)
        | None
        | Some _ ->
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
    let rec distribute cx ~cond_context (t1, t2) =
      match (t1, t2) with
      | (DefT (_, EmptyT), _)
      | (_, DefT (_, EmptyT))
      | (AnyT _, _)
      | (_, AnyT _) ->
        ()
      | (IntersectionT (r1, rep1), t2) ->
        let cases =
          Base.List.map (InterRep.members rep1) ~f:(fun t1 () ->
              distribute cx ~cond_context (t1, t2)
          )
        in
        Speculation_flow.try_custom
          cx
          ~no_match_error_loc:(get_no_match_error_loc (r1, TypeUtil.reason_of_t t2))
          cases
      | (t1, IntersectionT (r2, rep2)) ->
        let cases =
          Base.List.map (InterRep.members rep2) ~f:(fun t2 () ->
              distribute cx ~cond_context (t1, t2)
          )
        in
        Speculation_flow.try_custom
          cx
          ~no_match_error_loc:(get_no_match_error_loc (TypeUtil.reason_of_t t1, r2))
          cases
      | (t1, t2) ->
        let t1_needs_concretization = eq_needs_concretization t1 in
        let t2_needs_concretization = eq_needs_concretization t2 in
        if (not t1_needs_concretization) && not t2_needs_concretization then
          match strict_equatable_error cond_context (t1, t2) with
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
                Base.List.iter t2s ~f:(fun t2 -> distribute cx ~cond_context (t1, t2))
            )
    in

    (fun ~cond_context -> distribute ~cond_context)

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
      | DefT (_, SingletonBoolT false)
      | DefT (_, StrT_UNSOUND (_, OrdinaryName ""))
      | DefT (_, SingletonStrT (OrdinaryName ""))
      | DefT (_, NumT_UNSOUND (_, (0., _)))
      | DefT (_, SingletonNumT (0., _))
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
      | DefT (_, StrT_UNSOUND _) ->
        ()
      | DefT (_, NumGeneralT _)
      | DefT (_, NumT_UNSOUND _) ->
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
