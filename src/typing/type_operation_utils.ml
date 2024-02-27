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

  let get_module_t cx ?(perform_platform_validation = false) (loc, mref) =
    let reason = Reason.(mk_reason (RCustom mref) loc) in
    if Context.in_declare_module cx then
      Flow_js_utils.get_builtin_module cx mref reason
    else
      let module_t =
        match Context.find_require cx mref with
        | Ok t -> t
        | Error m_name ->
          Flow_js_utils.lookup_builtin_error cx m_name reason
          |> Flow_js_utils.apply_env_errors cx loc
      in
      ( if perform_platform_validation && Context.((metadata cx).file_options.Files.multi_platform)
      then
        match Flow_js.possible_concrete_types_for_inspection cx reason module_t with
        | [ModuleT m] -> check_platform_availability cx reason m.module_available_platforms
        | _ -> ()
      );
      module_t

  let singleton_concretize_type_for_imports_exports cx r t =
    match Flow.possible_concrete_types_for_imports_exports cx r t with
    | [] -> EmptyT.why r
    | [t] -> t
    | t0 :: t1 :: ts -> UnionT (r, UnionRep.make t0 t1 ts)

  let get_imported_t
      cx ~import_reason ~module_name ~source_module_t ~import_kind ~remote_name ~local_name =
    let is_strict = Context.is_strict cx in
    let name_def_loc_ref = ref None in
    let t =
      let assert_import_is_value cx reason name export_t =
        Flow.FlowJs.flow_opt
          cx
          ~trace:Trace.dummy_trace
          (export_t, AssertImportIsValueT (reason, name))
      in
      let with_concretized_type cx r f t =
        f (singleton_concretize_type_for_imports_exports cx r t)
      in
      match concretize_module_type cx import_reason source_module_t with
      | Ok m ->
        let (name_loc_opt, t) =
          if remote_name = "default" then
            Flow_js_utils.ImportDefaultTKit.on_ModuleT
              cx
              ~assert_import_is_value
              ~with_concretized_type
              (import_reason, import_kind, (local_name, module_name), is_strict)
              m
          else
            Flow_js_utils.ImportNamedTKit.on_ModuleT
              cx
              ~assert_import_is_value
              ~with_concretized_type
              (import_reason, import_kind, remote_name, module_name, is_strict)
              m
        in
        name_def_loc_ref := name_loc_opt;
        t
      | Error (lreason, any_source) ->
        Flow_js_utils.check_untyped_import cx import_kind lreason import_reason;
        AnyT (lreason, any_source)
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

  let get_module_namespace_type cx reason source_module_t =
    let is_strict = Context.is_strict cx in
    match concretize_module_type cx reason source_module_t with
    | Ok m -> Flow_js_utils.ImportModuleNsTKit.on_ModuleT cx (reason, is_strict) m
    | Error (lreason, any_source) ->
      Flow_js_utils.check_untyped_import cx ImportValue lreason reason;
      AnyT (lreason, any_source)

  let import_namespace_specifier_type
      cx import_reason import_kind ~module_name ~source_module_t ~local_loc =
    let open Ast.Statement in
    match import_kind with
    | ImportDeclaration.ImportType -> assert_false "import type * is a parse error"
    | ImportDeclaration.ImportTypeof ->
      let module_ns_t = get_module_namespace_type cx import_reason source_module_t in
      let bind_reason = repos_reason local_loc import_reason in
      Flow_js_utils.ImportTypeofTKit.on_concrete_type cx bind_reason "*" module_ns_t
    | ImportDeclaration.ImportValue ->
      let reason = mk_reason (RModule (OrdinaryName module_name)) local_loc in
      get_module_namespace_type cx reason source_module_t

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

  let cjs_require_type cx reason ~legacy_interop source_module_t =
    let is_strict = Context.is_strict cx in
    match concretize_module_type cx reason source_module_t with
    | Ok m ->
      Flow_js_utils.CJSRequireTKit.on_ModuleT
        cx
        ~reposition:Flow.reposition
        (reason, is_strict, legacy_interop)
        m
    | Error (lreason, any_source) ->
      Flow_js_utils.check_untyped_import cx ImportValue lreason reason;
      AnyT (lreason, any_source)
end
