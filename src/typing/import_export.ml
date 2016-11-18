(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* AST handling and type setup for import/export *)

module Ast = Spider_monkey_ast
module Flow = Flow_js

open Utils_js
open Reason
open Type

let mk_module_t cx reason = ModuleT(
  reason,
  {
    exports_tmap = Context.make_export_map cx SMap.empty;
    cjs_export = None;
    has_every_named_export = false;
  }
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
  Flow.mk_tvar_where cx reason (fun t ->
    Flow.flow cx (
      export_t,
      CJSExtractNamedExportsT(reason, (reason_exports_module, exporttypes), t)
    )
  )

let mk_resource_module_t cx loc f =
  let reason, exports_t = match Utils_js.extension_of_filename f with
  | Some ".css" ->
    let desc_str = "Flow assumes requiring a .css file returns an Object" in
    let reason = Reason.mk_reason (RCustom desc_str) loc in
    reason, Type.AnyObjT reason
  | Some ext ->
    let desc_str =
      Utils_js.spf "Flow assumes that requiring a %s file returns a string" ext
    in
    let reason = Reason.mk_reason (RCustom desc_str) loc in
    reason, Type.StrT.why reason
  | _ -> failwith "How did we find a resource file without an extension?!"
  in

  mk_commonjs_module_t cx reason reason exports_t


(* given a module name, return associated tvar if already
 * present in module map, or create and add *)
let module_t_of_name cx m reason =
  match SMap.get m (Context.module_map cx) with
  | Some t -> t
  | None ->
    Flow.mk_tvar_where cx reason (fun t -> Context.add_module cx m t)

let require cx ?(internal=false) m_name loc =
  Context.add_require cx m_name loc;
  if not internal
  then Type_inference_hooks_js.dispatch_require_hook cx m_name loc;
  let desc = RCustom (spf "CommonJS exports of \"%s\"" m_name) in
  let reason = mk_reason desc loc in
  Flow.mk_tvar_where cx reason (fun t ->
    Flow.flow cx (
      module_t_of_name cx m_name (mk_reason (RCustom m_name) loc),
      CJSRequireT(reason, t)
    )
  )

let import ?reason cx m_name loc =
  Context.add_require cx m_name loc;
  Type_inference_hooks_js.dispatch_import_hook cx m_name loc;
  let reason =
    match reason with
    | Some r -> r
    | None -> mk_reason (RCustom m_name) loc
  in
  module_t_of_name cx m_name reason

let import_ns cx reason module_name loc =
  Context.add_require cx module_name loc;
  Type_inference_hooks_js.dispatch_import_hook cx module_name loc;
  Flow.mk_tvar_where cx reason (fun t ->
    Flow.flow cx (
      module_t_of_name cx module_name (mk_reason (RCustom module_name) loc),
      ImportModuleNsT(reason, t)
    )
  )

let module_t_of_cx cx =
  match Context.declare_module_t cx with
  | None ->
    let m = Modulename.to_string (Context.module_name cx) in
    let loc = Loc.({ none with source = Some (Context.file cx) }) in
    module_t_of_name cx m (Reason.mk_reason (RCustom "exports") loc)
  | Some t -> t

let set_module_t cx reason f =
  match Context.declare_module_t cx with
  | None -> (
    let module_name = Modulename.to_string (Context.module_name cx) in
    Context.add_module cx module_name (Flow.mk_tvar_where cx reason f)
  )
  | Some _ ->
    Context.set_declare_module_t cx (Some (Flow.mk_tvar_where cx reason f))

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
let set_module_kind cx reason new_exports_kind = Context.(
  (match (Context.module_kind cx, new_exports_kind) with
  | (ESModule, CommonJSModule(Some _))
  | (CommonJSModule(Some _), ESModule)
    ->
      Flow_error.(add_output cx (EIndeterminateModuleType reason))
  | _ -> ()
  );
  Context.set_module_kind cx new_exports_kind
)

(**
 * Given an exported default declaration, identify nameless declarations and
 * name them with a special internal name that can be used to reference them
 * when assigning the export value.
 *)
let nameify_default_export_decl decl = Ast.Statement.(
  match decl with
  | loc, FunctionDeclaration func_decl -> Ast.Function.(
    if func_decl.id <> None then decl else
      loc, FunctionDeclaration {
        func_decl with
          id = Some (loc, internal_name "*default*");
      }
    )

  | loc, ClassDeclaration class_decl -> Ast.Class.(
    if class_decl.id <> None then decl else
      loc, ClassDeclaration {
        class_decl with
          id = Some (loc, internal_name "*default*");
      }
    )

  | _ -> decl
)

let warn_or_ignore_export_star_as cx name =
  if name = None then () else
  match Context.esproposal_export_star_as cx, name with
  | Options.ESPROPOSAL_WARN, Some(loc, _) ->
    Flow_error.(add_output cx (EExperimentalExportStarAs loc))
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
let get_module_exports cx reason =
  Env.get_var cx (internal_name "exports") reason

let set_module_exports cx reason t =
  ignore Env.(set_var cx (internal_name "exports") t reason)
