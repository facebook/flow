(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
        | OpaqueT (_, { underlying_t = None; super_t = Some t; _ }) ->
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
            | DefT (_, BoolT _) -> { exists_check with bool_loc = t_loc }
            | DefT (_, StrT _) -> { exists_check with string_loc = t_loc }
            | DefT (_, NumT _) -> { exists_check with number_loc = t_loc }
            | DefT (_, BigIntT _) -> { exists_check with bigint_loc = t_loc }
            | DefT (_, MixedT _) -> { exists_check with mixed_loc = t_loc }
            | DefT (_, EnumValueT (ConcreteEnum { representation_t = DefT (_, BoolT _); _ }))
            | DefT (_, EnumValueT (AbstractEnum { representation_t = DefT (_, BoolT _); _ })) ->
              { exists_check with enum_bool_loc = t_loc }
            | DefT (_, EnumValueT (ConcreteEnum { representation_t = DefT (_, StrT _); _ }))
            | DefT (_, EnumValueT (AbstractEnum { representation_t = DefT (_, StrT _); _ })) ->
              { exists_check with enum_string_loc = t_loc }
            | DefT (_, EnumValueT (ConcreteEnum { representation_t = DefT (_, NumT _); _ }))
            | DefT (_, EnumValueT (AbstractEnum { representation_t = DefT (_, NumT _); _ })) ->
              { exists_check with enum_number_loc = t_loc }
            | DefT (_, EnumValueT (ConcreteEnum { representation_t = DefT (_, BigIntT _); _ }))
            | DefT (_, EnumValueT (AbstractEnum { representation_t = DefT (_, BigIntT _); _ })) ->
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
        (Error_message.EPropNotFound { prop_name; reason_prop; reason_obj; use_op; suggestion }))
    misses

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
          super_t = None;
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
    | DefT (_, (BoolT (Some _) | StrT (Literal _) | NumT (Literal _))) -> true
    | _ -> false
  in
  let matching_props_checks =
    Base.List.filter_map (Context.matching_props cx) ~f:(fun (prop_name, sentinel, obj_t) ->
        match peek cx sentinel with
        (* Limit the check to promitive literal sentinels *)
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
    match
      Flow_js.possible_concrete_types_for_inspection cx obj_reason obj
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
    | [obj] -> Flow_js.flow cx (obj, u)
    | t0 :: t1 :: ts ->
      (* When the object type is a union, as long as one of them has a matching prop, it should be
       * good. (e.g. obj.type === 'a' when `typeof obj = {type: 'a'} | {type: 'b'}`). Therefore,
       * we turn unions into intersections below.
       *
       * We have to invoke the speculation kit directly, so that we won't hit the logic of
       * non-speculating IntersectionT ~> LookupT handler. *)
      SpeculationKit.try_intersection cx DepthTrace.dummy_trace u obj_reason (InterRep.make t0 t1 ts)
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
          | Env_api.SingletonNum (lit_loc, sense, num, raw) ->
            let reason = lit_loc |> Reason.(mk_reason (RNumberLit raw)) in
            DefT (reason, NumT (Literal (Some sense, (num, raw))))
          | Env_api.SingletonBool (lit_loc, b) ->
            let reason = lit_loc |> Reason.(mk_reason (RBooleanLit b)) in
            DefT (reason, BoolT (Some b))
          | Env_api.SingletonStr (lit_loc, sense, str) ->
            let reason = lit_loc |> Reason.(mk_reason (RStringLit (OrdinaryName str))) in
            DefT (reason, StrT (Literal (Some sense, Reason.OrdinaryName str)))
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

let check_multiplatform_conformance cx ast tast =
  let (prog_aloc, _) = ast in
  let filename = Context.file cx in
  let file_options = (Context.metadata cx).Context.file_options in
  let file_loc = Loc.{ none with source = Some filename } |> ALoc.of_loc in
  match
    Files.relative_interface_mref_of_possibly_platform_specific_file ~options:file_options filename
  with
  | Some imported_interface_module_name ->
    let open Type in
    (match Context.find_require cx imported_interface_module_name with
    | Context.MissingModule _
    | Context.UncheckedModule _ ->
      (* It's ok if a platform speicific implementation file doesn't have an interface.
       * It just makes the module non-importable without platform extension. *)
      ()
    | Context.TypedModule interface_module_t ->
      let get_exports_t ~is_common_interface_module reason module_t =
        match Flow_js.possible_concrete_types_for_inspection cx reason module_t with
        | [ModuleT m] ->
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
        | _ -> AnyT.make Untyped reason
      in
      let interface_t =
        let reason = Reason.(mk_reason (RCustom "common interface") prog_aloc) in
        get_exports_t ~is_common_interface_module:true reason interface_module_t
      in
      let (self_sig_loc, self_module_t) = Module_info_analyzer.analyze_program cx tast in
      let self_t =
        get_exports_t
          ~is_common_interface_module:false
          (TypeUtil.reason_of_t self_module_t)
          self_module_t
      in
      (* We need to fully resolve the type to prevent tvar widening. *)
      Tvar_resolver.resolve cx interface_t;
      Tvar_resolver.resolve cx self_t;
      let use_op = Op (ConformToCommonInterface { self_sig_loc; self_module_loc = prog_aloc }) in
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
        match Context.find_require cx mref with
        | Context.TypedModule _
        | Context.UncheckedModule _ ->
          true
        | Context.MissingModule _ -> false
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
  check_multiplatform_conformance cx ast tast;
  check_polarity cx;
  check_general_post_inference_validations cx;
  detect_sketchy_null_checks cx tast;
  detect_non_voidable_properties cx;
  detect_test_prop_misses cx;
  detect_unnecessary_optional_chains cx;
  detect_import_export_errors cx ast metadata;
  detect_matching_props_violations cx;
  detect_literal_subtypes cx;
  detect_unused_promises cx;
  check_union_opt cx;
  check_spread_prop_keys cx tast;
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
 * Finally, due to cycles, it's possile that src_cx and dst_cx share the same
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

let copy_into dst_cx src_cx t =
  let (_ : Context.t) = copier#type_ src_cx Polarity.Positive dst_cx t in
  ()

let copied dst_cx src_cx t =
  let (_ : Context.t) = copier#type_ src_cx Polarity.Positive dst_cx t in
  t

let merge_lib_files ~sig_opts ordered_asts =
  let (_builtin_errors, builtin_locs, builtins) =
    Type_sig_utils.parse_and_pack_builtins sig_opts ordered_asts
  in
  match ordered_asts with
  | [] -> (builtins, Context.EmptyMasterContext)
  | fst_ast :: _ ->
    let builtin_leader_file_key = Base.Option.value_exn (fst_ast |> fst |> Loc.source) in
    (builtins, Context.NonEmptyMasterContext { builtin_leader_file_key; builtin_locs; builtins })

let mk_builtins metadata master_cx =
  match master_cx with
  | Context.EmptyMasterContext -> (fun _ -> Builtins.empty ())
  | Context.NonEmptyMasterContext { builtin_leader_file_key; builtin_locs; builtins } ->
    let builtins_ref = ref (Builtins.empty ()) in
    let cx =
      Context.make
        (Context.make_ccx ())
        { metadata with Context.checked = false }
        builtin_leader_file_key
        (lazy (ALoc.empty_table builtin_leader_file_key))
        (fun mref -> Context.MissingModule mref)
        (fun _ -> !builtins_ref)
    in
    let (values, types, modules) =
      Type_sig_merge.merge_builtins cx builtin_leader_file_key builtin_locs builtins
    in
    builtins_ref := Builtins.of_name_map ~mapper:Base.Fn.id ~values ~types ~modules;
    (fun dst_cx -> Builtins.of_name_map ~mapper:(copied dst_cx cx) ~values ~types ~modules)
