(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AST handling and type setup for import/export *)

module Flow = Flow_js
open Reason
open Type

let mk_module_t cx reason =
  ModuleT
    ( reason,
      {
        exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
        cjs_export = None;
        has_every_named_export = false;
      },
      Context.is_strict cx
    )

(**
 * When CommonJS modules set their export type, we do two things:
 *
 * (1) Set the type in the cjs_export slot of the ModuleT container
 *
 * (2) If the type is an object, mark it's properties as named exports, via
 *     CJSExtractNamedExportsT. (this is for convenience as part of our
 *     ES <-> CJS module interop semantics)
 *)
let mk_commonjs_module_t cx reason_exports_module reason export_t =
  let exporttypes =
    {
      exports_tmap = Context.make_export_map cx NameUtils.Map.empty;
      cjs_export = Some export_t;
      has_every_named_export = false;
    }
  in
  Tvar.mk_where cx reason (fun t ->
      Flow.flow
        cx
        ( export_t,
          CJSExtractNamedExportsT
            (reason, (reason_exports_module, exporttypes, Context.is_strict cx), t)
        )
  )

let get_module_t cx ?(declare_module = false) (loc, mref) =
  if declare_module || Context.in_declare_module cx then
    OpenT (Flow.get_builtin_module cx loc mref)
  else
    match Context.find_require cx mref with
    | Ok t -> t
    | Error m_name ->
      let reason = Reason.(mk_reason (RCustom mref) loc) in
      Flow_js_utils.lookup_builtin_strict_error cx m_name reason
      |> Flow_js_utils.apply_env_errors cx loc

let require cx ~legacy_interop require_loc module_ref module_t =
  let reason = mk_reason (RCommonJSExports module_ref) require_loc in
  let is_strict = Context.is_strict cx in
  Tvar.mk_where cx reason (fun t_out ->
      Flow.flow cx (module_t, CJSRequireT { reason; t_out; is_strict; legacy_interop })
  )

let import_ns cx reason module_t =
  let is_strict = Context.is_strict cx in
  Tvar.mk_where cx reason (fun t ->
      Flow.flow cx (module_t, ImportModuleNsT { reason; t; is_strict })
  )

(* Module exports are treated differently than `exports`. The latter is a
   variable that is implicitly set to the empty object at the top of a
   module. As such, properties can be added to it throughout the module,
   corresponding to the pattern exports.foo = ... for exporting foo. As it turns
   out, module.exports is the same object. A different pattern is to reset
   module.exports at the end of the module, like module.exports = { foo: ... }
   to export foo. This makes any properties added to the exports object
   redundant. Both these patterns are modeled by storing module.exports as an
   internal variable, initially set to the empty object, doing inference on a
   module, and then flowing module.exports to exports, so that whatever its
   final value is (initial object or otherwise) is checked against the type
   declared for exports or any other use of exports. *)

let cjs_clobber cx loc t =
  if Module_info.cjs_clobber (Context.module_info cx) loc then Type_env.set_module_exports cx t

let export cx = Module_info.export (Context.module_info cx)

let export_star cx = Module_info.export_star (Context.module_info cx)

let export_type cx = Module_info.export_type (Context.module_info cx)

let export_type_star cx = Module_info.export_type_star (Context.module_info cx)

let export_binding cx ?is_function name ?preferred_def_locs ~name_loc = function
  | Flow_ast.Statement.ExportValue ->
    let t =
      Type_env.get_var_declared_type
        ~lookup_mode:Type_env.LookupMode.ForValue
        ?is_declared_function:is_function
        cx
        name
        name_loc
    in
    export cx name ?preferred_def_locs ~name_loc t
  | Flow_ast.Statement.ExportType ->
    let t =
      Type_env.get_var_declared_type
        ~lookup_mode:Type_env.LookupMode.ForType
        ?is_declared_function:is_function
        cx
        name
        name_loc
    in
    export_type cx name ?preferred_def_locs ~name_loc:(Some name_loc) t

(* After we have seen all the export statements in a module, this function will
 * calculate a ModuleT type (or a tvar that resolves to one) describing the
 * exports of a file.
 *
 * For CommonJS modules, this is fairly simple. We have the exported value
 * itself, plus any type exports. If the exported value is an object, we treat
 * the fields as named exports for ES module dependents.
 *
 * For ES modules, we have both named exports and "star" exports, which copy the
 * exports of one file into another. This can lead to conflits, which are
 * resolved carefully. Note that locally named exports always win, even if they
 * are followed by a star export that includes a conflicting name.
 *
 * Finally, both CJS and ES modules can export types, which also has a star
 * export variant. Conflicts are handled in the same way.
 *)
let mk_module_t =
  Module_info.(
    let copy_named_exports cx reason module_t (loc, from_ns) =
      let reason = repos_reason loc reason in
      Tvar.mk_where cx reason (fun tout ->
          Flow.flow cx (from_ns, CopyNamedExportsT (reason, module_t, tout))
      )
    in
    let copy_type_exports cx reason module_t (loc, from_ns) =
      let reason = repos_reason loc reason in
      Tvar.mk_where cx reason (fun tout ->
          Flow.flow cx (from_ns, CopyTypeExportsT (reason, module_t, tout))
      )
    in
    let copy_star_exports cx reason exports module_t =
      Module_info.fold_star2
        (copy_named_exports cx reason)
        (copy_type_exports cx reason)
        module_t
        exports
    in
    let export_named cx reason kind named module_t =
      Tvar.mk_where cx reason (fun tout ->
          Flow.flow cx (module_t, ExportNamedT (reason, named, kind, tout))
      )
    in
    fun cx reason loc ->
      let info = Context.module_info cx in
      match info.kind with
      | CJS _ ->
        Type_env.get_module_exports cx loc
        |> mk_commonjs_module_t cx reason reason
        |> export_named cx reason ExportType info.type_named
        |> copy_star_exports cx reason ([], info.type_star)
      | ES { named; star } ->
        mk_module_t cx reason
        |> export_named cx reason ExportValue named
        |> export_named cx reason ExportType info.type_named
        |> copy_star_exports cx reason (star, info.type_star)
  )

(* A best effort way to pick a location as the signature location of the module.
 * - For cjs, we will first try to pick the location of module.exports, then
 *   fallback to the first module.exports prop assignment
 * - For esm, we will first try to pick the location of default exports, then
 *   fallback to the first export. *)
let module_exports_sig_loc cx =
  let { Module_info.kind; _ } = Context.module_info cx in
  match kind with
  | Module_info.CJS _ ->
    let { Loc_env.var_info = { Env_api.cjs_exports_state; _ }; _ } = Context.environment cx in
    (match cjs_exports_state with
    | Env_api.CJSModuleExports l -> Some l
    | Env_api.CJSExportNames names ->
      names
      |> SMap.values
      |> Base.List.map ~f:fst
      |> Base.List.sort ~compare:ALoc.compare
      |> Base.List.hd)
  | Module_info.ES { named; _ } ->
    (match NameUtils.Map.find_opt (Reason.OrdinaryName "default") named with
    | Some { Type.name_loc; _ } -> name_loc
    | None ->
      named
      |> NameUtils.Map.values
      |> Base.List.filter_map ~f:(fun { Type.name_loc; _ } -> name_loc)
      |> Base.List.sort ~compare:ALoc.compare
      |> Base.List.hd)
