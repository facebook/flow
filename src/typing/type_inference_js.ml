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

module Flow = Flow_js
module FlowError = Flow_error
module ImpExp = Import_export
module Utils = Utils_js

(**********)
(* Driver *)
(**********)

let force_annotations cx =
  let m = Context.module_ref cx in
  let tvar = Flow_js.lookup_module cx m in
  let _, id = Type.open_tvar tvar in
  let before = Errors.ErrorSet.cardinal (Context.errors cx) in
  Flow_js.enforce_strict cx id;
  let after = Errors.ErrorSet.cardinal (Context.errors cx) in
  if (after > before) then
    Context.add_tvar cx id Constraint.(Root {
      rank = 0; constraints = Resolved Type.Locationless.AnyT.t
    })

(* core inference, assuming setup and teardown happens elsewhere *)
let infer_core cx statements =
  try
    statements |> Statement.toplevel_decls cx;
    statements |> Statement.toplevels cx;
  with
  | Abnormal.Exn Abnormal.Throw ->
    (* throw is allowed as a top-level statement *)
    ()
  | Abnormal.Exn _ ->
    (* should never happen *)
    let loc = Loc.({ none with source = Some (Context.file cx) }) in
    Flow_js.add_output cx FlowError.(EInternal (loc, AbnormalControlFlow))
  | exc ->
    let loc = Loc.({ none with source = Some (Context.file cx) }) in
    Flow_js.add_output cx FlowError.(EInternal (loc, UncaughtException exc))

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
let infer_ast ~metadata ~filename ast ~require_loc_map =
  Flow_js.Cache.clear();

  let _, statements, comments = ast in

  let module_ref = Files.module_ref filename in
  let cx = Flow_js.fresh_context metadata filename module_ref in
  let checked = Context.is_checked cx in

  let reason_exports_module =
    let desc = Reason.RCustom (
      Utils.spf "exports of file `%s`" module_ref
    ) in
    Reason.locationless_reason desc
  in

  let local_exports_var = Flow_js.mk_tvar cx reason_exports_module in

  let module_scope = Scope.(
    let scope = fresh ~var_scope_kind:Module () in

    add_entry "exports"
      (Entry.new_var ~loc:(Type.loc_of_t local_exports_var) local_exports_var)
      scope;

    add_entry (Reason.internal_name "exports")
      (Entry.new_var
        ~loc:(Reason.loc_of_reason reason_exports_module)
        ~specific:(Type.DefT (
          Reason.replace_reason_const
            (Reason.RCustom "undefined exports")
            reason_exports_module,
          Type.EmptyT))
        (Type.DefT (reason_exports_module, Type.AnyT)))
      scope;

    scope
  ) in

  Env.init_env cx module_scope;

  let file_loc = Loc.({ none with source = Some filename }) in
  let reason = Reason.mk_reason (Reason.RCustom "exports") file_loc in

  let initial_module_t = ImpExp.module_t_of_cx cx in
  if checked then (
    SMap.iter (fun r loc ->
      Context.add_require cx r loc;
      Import_export.add_module_tvar cx r loc;
    ) require_loc_map;

    let init_exports = Flow.mk_object cx reason in
    ImpExp.set_module_exports cx file_loc init_exports;

    (* infer *)
    Flow_js.flow_t cx (init_exports, local_exports_var);
    infer_core cx statements;

    scan_for_suppressions cx comments;

    let module_t = Context.(
      match Context.module_kind cx with
      (* CommonJS with a clobbered module.exports *)
      | CommonJSModule(Some(loc)) ->
        let module_exports_t = ImpExp.get_module_exports cx file_loc in
        let reason = Reason.mk_reason (Reason.RCustom "exports") loc in
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
    Flow_js.unify cx initial_module_t Type.Locationless.AnyT.t
  );

  (* insist that whatever type flows into exports is fully annotated *)
  force_annotations cx;

  cx


(* infer a parsed library file.
   processing is similar to an ordinary module, except that
   a) symbols from prior library loads are suppressed if found,
   b) bindings are added as properties to the builtin object
 *)
let infer_lib_file ~metadata ~exclude_syms file ast =
  let _, statements, comments = ast in
  Flow_js.Cache.clear();

  let cx = Flow_js.fresh_context metadata file Files.lib_module_ref in
  let mapper = new Require.mapper false in
  let _ = mapper#program ast in
  let require_loc = mapper#requires in
  SMap.iter (fun r loc ->
    Context.add_require cx r loc;
    Import_export.add_module_tvar cx r loc;
  ) require_loc;

  let module_scope = Scope.fresh () in
  Env.init_env ~exclude_syms cx module_scope;

  infer_core cx statements;
  scan_for_suppressions cx comments;

  module_scope |> Scope.(iter_entries Entry.(fun name entry ->
    Flow_js.set_builtin cx name (actual_type entry)
  ));

  cx, SMap.keys Scope.(module_scope.entries)
