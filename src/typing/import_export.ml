(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AST handling and type setup for import/export *)

module Flow = Flow_js

open Reason
open Type

let mk_module_t cx reason = ModuleT(
  reason,
  {
    exports_tmap = Context.make_export_map cx SMap.empty;
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
  let exporttypes = {
    exports_tmap = Context.make_export_map cx SMap.empty;
    cjs_export = Some export_t;
    has_every_named_export = false;
  } in
  Tvar.mk_where cx reason (fun t ->
    Flow.flow cx (
      export_t,
      CJSExtractNamedExportsT(
        reason,
        (reason_exports_module, exporttypes, (Context.is_strict cx)),
        t
      )
    )
  )

let mk_resource_module_t cx loc f =
  let reason, exports_t = match Utils_js.extension_of_filename f with
  | Some ".css" ->
    let reason = Reason.mk_reason RObjectType loc in
    reason, Type.AnyT.make Type.Untyped reason
  | Some _ ->
    let reason = Reason.mk_reason RString loc in
    reason, Type.StrT.why reason |> with_trust bogus_trust
  | _ -> failwith "How did we find a resource file without an extension?!"
  in

  mk_commonjs_module_t cx reason reason exports_t


(* given a module name, return associated tvar in module map (failing if not
   found); e.g., used to find tvars associated with requires *after* all
   requires already have entries in the module map *)
let require_t_of_ref_unsafe cx (loc, _) =
  Context.find_require cx loc

let require cx ((_, module_ref) as source) require_loc =
  Type_inference_hooks_js.dispatch_import_hook cx source require_loc;
  let module_t = require_t_of_ref_unsafe cx source in
  let reason = mk_reason (RCommonJSExports module_ref) require_loc in
  Tvar.mk_where cx reason (fun t ->
    Flow.flow cx (module_t, CJSRequireT(reason, t, Context.is_strict cx))
  )

let import cx source import_loc =
  Type_inference_hooks_js.dispatch_import_hook cx source import_loc;
  require_t_of_ref_unsafe cx source

let import_ns cx reason source import_loc =
  Type_inference_hooks_js.dispatch_import_hook cx source import_loc;
  let module_t = require_t_of_ref_unsafe cx source in
  Tvar.mk_where cx reason (fun t ->
    Flow.flow cx (module_t, ImportModuleNsT(reason, t, Context.is_strict cx))
  )

let module_t_of_cx cx =
  let m = Context.module_ref cx in
  match SMap.get m (Context.module_map cx) with
  | Some t -> t
  | None ->
    let loc = Loc.({ none with source = Some (Context.file cx) }) in
    let reason = (Reason.mk_reason (RCustom "exports") (loc |> ALoc.of_loc)) in
    Tvar.mk_where cx reason (fun t -> Context.add_module cx m t)

let set_module_t cx reason f =
  let module_ref = Context.module_ref cx in
  Context.add_module cx module_ref (Tvar.mk_where cx reason f)

(**
 * Before running inference, we assume that we're dealing with a CommonJS
 * module that has a built-in, initialized `exports` object (i.e. it is not an
 * ES module).
 *
 * During inference, if we encounter an assignment to module.exports then we
 * use this as an indicator that the module is definitely a CommonJS module --
 * but that the bult-in `exports` value is no longer the exported variable.
 * Instead, whatever was assigned to `module.exports` is now that CJS exported
 * value.
 *
 * On the other hand, if we encounter an ES `export` statement during inference,
 * we use this as an indicator that the module is an ES module. The one
 * exception to this rule is that we do not use `export type` as an indicator of
 * an ES module (since we want CommonJS modules to be able to use `export type`
 * as well).
 *
 * At the end of inference, we make use of this information to decide which
 * types to store as the expors of the module (i.e. Do we use the built-in
 * `exports` value? Do we use the type that clobbered `module.exports`? Or do we
 * use neither because the module only has direct ES exports?).
 *)
let set_module_kind cx loc new_exports_kind = Context.(
  (match (Context.module_kind cx, new_exports_kind) with
  | (ESModule, CommonJSModule(Some _))
  | (CommonJSModule(Some _), ESModule)
    ->
      Flow.add_output cx (Error_message.EIndeterminateModuleType loc)
  | _ -> ()
  );
  Context.set_module_kind cx new_exports_kind
)

(**
 * Given an exported default declaration, identify nameless declarations and
 * name them with a special internal name that can be used to reference them
 * when assigning the export value.
 *
 * Paired with function which undoes this, for typed AST construction
 *)
let nameify_default_export_decl decl = Flow_ast.Statement.(
  let identity x = x in
  match decl with
  | loc, FunctionDeclaration func_decl -> Flow_ast.Function.(
    if func_decl.id <> None then decl, identity else
      (loc, FunctionDeclaration {
        func_decl with
          id = Some (Flow_ast_utils.ident_of_source (loc, internal_name "*default*"));
      }), (function
        | x, FunctionDeclaration func_decl ->
          x, FunctionDeclaration { func_decl with id = None }
        | _ -> failwith "expected FunctionDeclaration"
      )
    )

  | loc, ClassDeclaration class_decl -> Flow_ast.Class.(
    if class_decl.id <> None then decl, identity else
      (loc, ClassDeclaration {
        class_decl with
          id = Some (Flow_ast_utils.ident_of_source (loc, internal_name "*default*"));
      }), (function
        | x, ClassDeclaration class_decl ->
          x, ClassDeclaration { class_decl with id = None }
        | _ -> failwith "expected ClassDeclaration"
      )
    )

  | _ -> decl, identity
)

let warn_or_ignore_export_star_as cx name =
  if name = None then () else
  match Context.esproposal_export_star_as cx, name with
  | Options.ESPROPOSAL_WARN, Some(loc, _) ->
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
let get_module_exports cx loc =
  Env.get_internal_var cx "exports" loc

let set_module_exports cx loc t =
  let change: Changeset.EntryRef.t option =
    Env.set_internal_var cx "exports" t loc in
  ignore change
