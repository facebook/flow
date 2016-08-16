(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* infer phase services *)

module Ast = Spider_monkey_ast
module Flow = Flow_js
module FlowError = Flow_error
module ImpExp = Import_export
module Utils = Utils_js

(**********)
(* Driver *)
(**********)

let force_annotations cx =
  let m = Modulename.to_string (Context.module_name cx) in
  let tvar = Flow_js.lookup_module cx m in
  let _, id = Type.open_tvar tvar in
  let before = Errors.ErrorSet.cardinal (Context.errors cx) in
  Flow_js.enforce_strict cx id;
  let after = Errors.ErrorSet.cardinal (Context.errors cx) in
  if (after > before) then
    Context.add_tvar cx id Constraint.(Root {
      rank = 0; constraints = Resolved Type.AnyT.t
    })

(* core inference, assuming setup and teardown happens elsewhere *)
let infer_core cx statements =
  try
    statements |> Statement.toplevel_decls cx;
    statements |> Statement.toplevels cx;
  with
  | Abnormal.Exn _ ->
    let msg = "abnormal control flow" in
    FlowError.add_warning cx
      (Loc.({ none with source = Some (Context.file cx) }), [msg])
  | exc ->
    let msg = Utils.fmt_exc exc in
    FlowError.add_warning cx
      (Loc.({ none with source = Some (Context.file cx) }), [msg])

(* There's a .flowconfig option to specify suppress_comments regexes. Any
 * comments that match those regexes will suppress any errors on the next line
 *)
let scan_for_suppressions =
  let should_suppress suppress_comments comment =
    List.exists (fun r -> Str.string_match r comment 0) suppress_comments

  in fun cx comments ->
    let suppress_comments = Context.suppress_comments cx in
    let should_suppress = should_suppress suppress_comments in

    (* Bail immediately if we're not using error suppressing comments *)
    if suppress_comments <> []
    then List.iter (function
      | loc, Ast.Comment.Block comment
      | loc, Ast.Comment.Line comment when should_suppress comment ->
          Context.add_error_suppression cx loc
      | _ -> ()) comments

(* build module graph *)
let infer_ast ~metadata ~filename ~module_name ast =
  Flow_js.Cache.clear();

  let _, statements, comments = ast in

  let cx = Flow_js.fresh_context metadata filename module_name in
  let checked = Context.is_checked cx in

  let exported_module_name = Modulename.to_string module_name in
  let reason_exports_module =
    Reason.reason_of_string (
      Utils.spf "exports of module `%s`" exported_module_name) in

  let local_exports_var = Flow_js.mk_tvar cx reason_exports_module in

  let module_scope = Scope.(
    let scope = fresh ~var_scope_kind:Module () in

    add_entry "exports"
      (Entry.new_var ~loc:(Type.loc_of_t local_exports_var) local_exports_var)
      scope;

    add_entry (Reason.internal_name "exports")
      (Entry.new_var
        ~loc:(Reason.loc_of_reason reason_exports_module)
        ~specific:(Type.EmptyT (
          Reason.replace_reason "undefined exports" reason_exports_module))
        (Type.AnyT reason_exports_module))
      scope;

    scope
  ) in

  Env.init_env cx module_scope;

  let reason = Reason.mk_reason "exports" Loc.({
    none with source = Some filename
  }) in

  if checked then (
    let init_exports = Flow.mk_object cx reason in
    ImpExp.set_module_exports cx reason init_exports;

    let initial_module_t = ImpExp.exports cx in

    (* infer *)
    Flow_js.flow_t cx (init_exports, local_exports_var);
    infer_core cx statements;

    scan_for_suppressions cx comments;

    let module_t = Context.(
      match Context.module_exports_type cx with
      (* CommonJS with a clobbered module.exports *)
      | CommonJSModule(Some(loc)) ->
        let module_exports_t = ImpExp.get_module_exports cx reason in
        let reason = Reason.mk_reason "exports" loc in
        ImpExp.mk_commonjs_module_t cx reason_exports_module
          reason module_exports_t

      (* CommonJS with a mutated 'exports' object *)
      | CommonJSModule(None) ->
        ImpExp.mk_commonjs_module_t cx reason_exports_module
          reason local_exports_var

      (* Uses standard ES module exports *)
      | ESModule -> ImpExp.mk_module_t cx reason_exports_module
    ) in
    Flow_js.flow_t cx (module_t, initial_module_t)
  ) else (
    Flow_js.unify cx (ImpExp.exports cx) Type.AnyT.t
  );

  (* insist that whatever type flows into exports is fully annotated *)
  force_annotations cx;

  cx


(* infer a parsed library file.
   processing is similar to an ordinary module, except that
   a) symbols from prior library loads are suppressed if found,
   b) bindings are added as properties to the builtin object
 *)
let infer_lib_file ~metadata ~exclude_syms file statements comments =
  Flow_js.Cache.clear();

  let cx = Flow_js.fresh_context
    metadata file (Modulename.String Files.lib_module) in

  let module_scope = Scope.fresh () in
  Env.init_env ~exclude_syms cx module_scope;

  infer_core cx statements;
  scan_for_suppressions cx comments;

  module_scope |> Scope.(iter_entries Entry.(fun name entry ->
    Flow_js.set_builtin cx name (actual_type entry)
  ));

  cx, SMap.keys Scope.(module_scope.entries)

let infer_resource_file ~metadata ~filename ~module_name =
  Flow_js.Cache.clear();

  let cx = Flow_js.fresh_context metadata filename module_name in
  let exported_module_name = Modulename.to_string module_name in

  let reason_exports_module =
    Reason.reason_of_string (
      Utils.spf "exports of module `%s`" exported_module_name) in
  let reason_loc = Loc.({ none with source = Some filename }) in

  let filename = Loc.(match filename with
  | ResourceFile filename -> filename
  | SourceFile _
  | LibFile _
  | Builtins
  | JsonFile _ -> failwith "Resource file is not a resource file?!") in
  let reason, exports_t = match Utils.extension_of_filename filename with
  | Some ".css" ->
      let reason = Reason.mk_reason
        "Flow assumes requiring a .css file returns undefined"
        reason_loc in
      reason, Type.VoidT.why reason
  | Some ext ->
      let reason = Reason.mk_reason
        (Utils.spf "Flow assumes that requiring a %s file returns a string" ext)
        reason_loc in
      reason, Type.StrT.why reason
  | None -> failwith "How did we find a resource file without an extension?!"
  in

  let module_t = ImpExp.mk_commonjs_module_t cx reason_exports_module
    reason exports_t in
  Flow_js.flow_t cx (module_t, ImpExp.exports cx);
  cx
