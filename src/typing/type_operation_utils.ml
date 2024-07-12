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

  let cjs_require_type cx reason ~namespace_symbol ~legacy_interop source_module_t =
    let is_strict = Context.is_strict cx in
    match concretize_module_type cx reason source_module_t with
    | Ok m ->
      Flow_js_utils.CJSRequireTKit.on_ModuleT
        cx
        ~reposition:Flow.reposition
        (reason, namespace_symbol, is_strict, legacy_interop)
        m
    | Error (lreason, any_source) -> AnyT (lreason, any_source)
end

module ImplicitInstantiation =
  Implicit_instantiation.Kit
    (Flow.FlowJs)
    (struct
      let mk_targ = Instantiation_utils.ImplicitTypeArgument.mk_targ

      let is_subtype = Flow.FlowJs.rec_flow_t

      let unify cx trace ~use_op (t1, t2) =
        Flow.FlowJs.rec_unify cx trace ~use_op ~unify_any:true t1 t2

      let reposition = Flow.FlowJs.reposition ?desc:None ?annot_loc:None
    end)

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
      | (DefT (_, StrT _), DefT (_, StrT _))
      | (DefT (_, NumT _), DefT (_, NumT _))
      | (DefT (_, BigIntT _), DefT (_, BigIntT _))
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
end

module Promise = struct
  let await cx reason t =
    (* await distributes over union: await (Promise<T> | void) = T | void *)
    match
      Flow.possible_concrete_types_for_inspection cx reason t
      |> List.map (ImplicitInstantiation.run_await cx ~use_op:unknown_use ~reason)
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
      | DefT (_, StrT _) -> ()
      | DefT (_, NumT _) -> ()
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
end
