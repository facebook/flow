(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
        exports_tmap = Context.make_export_map cx SMap.empty;
        cjs_export = None;
        has_every_named_export = false;
      },
      Context.is_strict cx )

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
      exports_tmap = Context.make_export_map cx SMap.empty;
      cjs_export = Some export_t;
      has_every_named_export = false;
    }
  in
  Tvar.mk_where cx reason (fun t ->
      Flow.flow
        cx
        ( export_t,
          CJSExtractNamedExportsT
            (reason, (reason_exports_module, exporttypes, Context.is_strict cx), t) ))

let mk_resource_module_t cx loc f =
  let (reason, exports_t) =
    match Utils_js.extension_of_filename f with
    | Some ".css" ->
      let reason = Reason.mk_reason RObjectType loc in
      (reason, Type.AnyT.make Type.Untyped reason)
    | Some _ ->
      let reason = Reason.mk_reason RString loc in
      (reason, Type.StrT.why reason |> with_trust bogus_trust)
    | _ -> failwith "How did we find a resource file without an extension?!"
  in
  mk_commonjs_module_t cx reason reason exports_t

(* given a module name, return associated tvar in module map (failing if not
   found); e.g., used to find tvars associated with requires *after* all
   requires already have entries in the module map *)
let require_t_of_ref_unsafe cx (loc, _) = Context.find_require cx loc

let require cx ((_, module_ref) as source) require_loc =
  let module_t = require_t_of_ref_unsafe cx source in
  let reason = mk_reason (RCommonJSExports module_ref) require_loc in
  Tvar.mk_where cx reason (fun t ->
      Flow.flow cx (module_t, CJSRequireT (reason, t, Context.is_strict cx)))

let import cx source = require_t_of_ref_unsafe cx source

let import_ns cx reason source =
  let module_t = require_t_of_ref_unsafe cx source in
  Tvar.mk_where cx reason (fun t ->
      Flow.flow cx (module_t, ImportModuleNsT (reason, t, Context.is_strict cx)))

(**
 * Given an exported default declaration, identify nameless declarations and
 * name them with a special internal name that can be used to reference them
 * when assigning the export value.
 *
 * Paired with function which undoes this, for typed AST construction
 *)
let nameify_default_export_decl decl =
  let open Flow_ast.Statement in
  let identity x = x in
  match decl with
  | (loc, FunctionDeclaration func_decl) ->
    let open Flow_ast.Function in
    if func_decl.id <> None then
      (decl, identity)
    else
      ( ( loc,
          FunctionDeclaration
            {
              func_decl with
              id = Some (Flow_ast_utils.ident_of_source (loc, internal_name "*default*"));
            } ),
        (function
        | (x, FunctionDeclaration func_decl) -> (x, FunctionDeclaration { func_decl with id = None })
        | _ -> failwith "expected FunctionDeclaration") )
  | (loc, ClassDeclaration class_decl) ->
    let open Flow_ast.Class in
    if class_decl.id <> None then
      (decl, identity)
    else
      ( ( loc,
          ClassDeclaration
            {
              class_decl with
              id = Some (Flow_ast_utils.ident_of_source (loc, internal_name "*default*"));
            } ),
        (function
        | (x, ClassDeclaration class_decl) -> (x, ClassDeclaration { class_decl with id = None })
        | _ -> failwith "expected ClassDeclaration") )
  | _ -> (decl, identity)

let warn_or_ignore_export_star_as cx name =
  if name = None then
    ()
  else
    match (Context.esproposal_export_star_as cx, name) with
    | (Options.ESPROPOSAL_WARN, Some (loc, _)) ->
      Flow.add_output cx (Error_message.EExperimentalExportStarAs loc)
    | _ -> ()

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
let get_module_exports cx loc = Env.get_internal_var cx "exports" loc

let set_module_exports cx loc t =
  let change : Changeset.EntryRef.t option = Env.set_internal_var cx "exports" t loc in
  ignore change

let cjs_clobber cx loc t =
  match Module_info.cjs_clobber (Context.module_info cx) loc with
  | Ok () -> set_module_exports cx loc t
  | Error msg -> Flow.add_output cx msg

let export cx name loc t =
  match Module_info.export (Context.module_info cx) name loc t with
  | Ok () -> ()
  | Error msg -> Flow.add_output cx msg

let export_star cx loc ns =
  match Module_info.export_star (Context.module_info cx) loc ns with
  | Ok () -> ()
  | Error msg -> Flow.add_output cx msg

let export_type cx = Module_info.export_type (Context.module_info cx)

let export_type_star cx = Module_info.export_type_star (Context.module_info cx)

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
          Flow.flow cx (from_ns, CopyNamedExportsT (reason, module_t, tout)))
    in
    let copy_type_exports cx reason module_t (loc, from_ns) =
      let reason = repos_reason loc reason in
      Tvar.mk_where cx reason (fun tout ->
          Flow.flow cx (from_ns, CopyTypeExportsT (reason, module_t, tout)))
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
          Flow.flow cx (module_t, ExportNamedT (reason, named, kind, tout)))
    in
    fun cx reason ->
      let info = Context.module_info cx in
      match info.kind with
      | CJS _ ->
        Loc.{ none with source = Some (Context.file cx) }
        |> ALoc.of_loc
        |> get_module_exports cx
        |> mk_commonjs_module_t cx reason reason
        |> export_named cx reason ExportType info.type_named
        |> copy_star_exports cx reason ([], info.type_star)
      | ES { named; star } ->
        mk_module_t cx reason
        |> export_named cx reason ExportValue named
        |> export_named cx reason ExportType info.type_named
        |> copy_star_exports cx reason (star, info.type_star))
