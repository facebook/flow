(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The mark phase visits a signature starting from exports, such that the only
 * visited data are also reachable from exports.
 *
 * During this walk, it resolves references from the parsing phase, which are
 * left unresolved as (name, scope) pairs until this point.
 *
 * The "mark" name is borred from garbage collection vocabulary. Like a tracing
 * collector, the marking process is used to detect what values are reachable so
 * the remainder can be discarded. *)

open Type_sig
open Type_sig_collections
module P = Type_sig_parse

let return_false _ = false

let mark_and_report_should_be_dirtified ~locs_to_dirtify f node =
  let should_be_dirty_ref = ref false in
  let visit_loc l =
    should_be_dirty_ref :=
      !should_be_dirty_ref || Base.List.exists locs_to_dirtify ~f:(Loc.contains l)
  in
  f ~locs_to_dirtify ~visit_loc node;
  !should_be_dirty_ref

let mark_and_report_not_dirty f node =
  f node;
  false

let mark_loc ~visit_loc loc =
  visit_loc (Locs.value loc);
  Locs.mark loc return_false

let mark_mref mref = Module_refs.mark mref return_false

let rec mark_parsed ~locs_to_dirtify ~visit_loc = function
  | P.Annot t -> mark_annot ~locs_to_dirtify ~visit_loc t
  | P.Value def -> mark_value ~locs_to_dirtify ~visit_loc def
  | P.TyRef name -> mark_tyname ~locs_to_dirtify ~visit_loc name
  | P.TyRefApp { loc; name; targs } ->
    mark_loc ~visit_loc loc;
    mark_tyname ~locs_to_dirtify ~visit_loc name;
    List.iter (mark_parsed ~locs_to_dirtify ~visit_loc) targs
  | P.AsyncVoidReturn loc -> mark_loc ~visit_loc loc
  | P.BuiltinTyRef { ref_loc; name = _ } -> mark_loc ~visit_loc ref_loc
  | P.Err (loc, SigError err) ->
    Signature_error.iter (mark_loc ~visit_loc) err;
    mark_loc ~visit_loc loc
  | P.Err (loc, _) -> mark_loc ~visit_loc loc
  | P.ValRef { type_only = false; ref } -> resolve_value_ref ~locs_to_dirtify ~visit_loc ref
  | P.ValRef { type_only = true; ref } -> resolve_type_ref ~locs_to_dirtify ~visit_loc ref
  | P.Pattern p ->
    Patterns.mark p (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_pattern)
  | P.Eval (loc, t, op) ->
    mark_loc ~visit_loc loc;
    mark_parsed ~locs_to_dirtify ~visit_loc t;
    mark_op ~locs_to_dirtify ~visit_loc op
  | P.Require { loc; mref }
  | P.ImportDynamic { loc; mref }
  | P.ModuleRef { loc; mref; legacy_interop = _ } ->
    mark_loc ~visit_loc loc;
    mark_mref mref

and mark_tyname ~locs_to_dirtify ~visit_loc = function
  | P.Unqualified ref -> resolve_type_ref ~locs_to_dirtify ~visit_loc ref
  | P.Qualified { loc; id_loc; name = _; qualification } ->
    mark_loc ~visit_loc loc;
    mark_loc ~visit_loc id_loc;
    mark_tyname ~locs_to_dirtify ~visit_loc qualification

and mark_binding ~locs_to_dirtify = function
  | P.LocalBinding node ->
    Local_defs.mark node (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_local_binding)
  | P.RemoteBinding node -> Remote_refs.mark node (mark_and_report_not_dirty mark_remote_binding)

and mark_local_binding ~locs_to_dirtify ~visit_loc = function
  | P.TypeBinding { id_loc = _; def } -> mark_def ~locs_to_dirtify ~visit_loc (Lazy.force def)
  | P.VarBinding { id_loc; name = _; def }
  | P.LetConstBinding { id_loc; name = _; def } ->
    mark_loc ~visit_loc id_loc;
    mark_parsed ~locs_to_dirtify ~visit_loc (Lazy.force def)
  | P.ConstRefBinding { id_loc; name = _; ref } ->
    mark_loc ~visit_loc id_loc;
    resolve_value_ref ~locs_to_dirtify ~visit_loc ref
  | P.ConstFunBinding { id_loc; name = _; loc; async = _; generator = _; def; statics } ->
    mark_loc ~visit_loc id_loc;
    mark_loc ~visit_loc loc;
    mark_fun ~locs_to_dirtify ~visit_loc (Lazy.force def);
    SMap.iter
      (fun _ (id_loc, def) ->
        mark_loc ~visit_loc id_loc;
        mark_parsed ~locs_to_dirtify ~visit_loc def)
      statics
  | P.FunBinding { id_loc; name = _; async = _; generator = _; fn_loc; def; statics } ->
    mark_loc ~visit_loc id_loc;
    mark_loc ~visit_loc fn_loc;
    mark_fun ~locs_to_dirtify ~visit_loc (Lazy.force def);
    SMap.iter
      (fun _ (id_loc, def) ->
        mark_loc ~visit_loc id_loc;
        mark_parsed ~locs_to_dirtify ~visit_loc def)
      statics
  | P.ComponentBinding { id_loc; name = _; fn_loc; def } ->
    mark_loc ~visit_loc id_loc;
    begin
      match def with
      | Some (lazy def) ->
        mark_loc ~visit_loc fn_loc;
        mark_component ~locs_to_dirtify ~visit_loc def
      | None -> ()
    end
  | P.ClassBinding { id_loc; name = _; def } ->
    mark_loc ~visit_loc id_loc;
    mark_class ~locs_to_dirtify ~visit_loc (Lazy.force def)
  | P.DeclareClassBinding { id_loc; name = _; def } ->
    mark_loc ~visit_loc id_loc;
    mark_declare_class ~locs_to_dirtify ~visit_loc (Lazy.force def)
  | P.DeclareFunBinding { name = _; defs_rev } ->
    Nel.iter
      (fun (id_loc, fn_loc, def) ->
        mark_loc ~visit_loc id_loc;
        mark_loc ~visit_loc fn_loc;
        mark_fun ~locs_to_dirtify ~visit_loc (Lazy.force def))
      defs_rev
  | P.EnumBinding { id_loc; name = _; def } ->
    mark_loc ~visit_loc id_loc;
    begin
      match def with
      | None -> ()
      | Some (lazy (rep, members, has_unknown_members)) ->
        ignore rep;
        ignore has_unknown_members;
        SMap.iter (fun _ -> mark_loc ~visit_loc) members
    end
  | P.NamespaceBinding { id_loc; name = _; values; types } ->
    mark_loc ~visit_loc id_loc;
    let f _ (loc, parsed) =
      mark_loc ~visit_loc loc;
      mark_parsed ~locs_to_dirtify ~visit_loc parsed
    in
    SMap.iter f values;
    SMap.iter f types

and mark_remote_binding = function
  | P.ImportBinding { id_loc; name = _; mref; remote = _ }
  | P.ImportTypeBinding { id_loc; name = _; mref; remote = _ }
  | P.ImportTypeofBinding { id_loc; name = _; mref; remote = _ }
  | P.ImportNsBinding { id_loc; name = _; mref }
  | P.ImportTypeofNsBinding { id_loc; name = _; mref } ->
    mark_loc ~visit_loc:ignore id_loc;
    mark_mref mref

and mark_pattern ~locs_to_dirtify ~visit_loc = function
  | P.PDef def ->
    Pattern_defs.mark
      (Lazy.force def)
      (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_parsed)
  | P.PropP { def; id_loc = loc; name = _ }
  | P.ObjRestP { def; loc; xs = _ }
  | P.IndexP { def; loc; i = _ }
  | P.ArrRestP { def; loc; i = _ } ->
    mark_loc ~visit_loc loc;
    Patterns.mark def (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_pattern)
  | P.ComputedP { def; elem } ->
    Patterns.mark def (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_pattern);
    Pattern_defs.mark elem (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_parsed)
  | P.UnsupportedLiteralP loc -> mark_loc ~visit_loc loc

and mark_value ~locs_to_dirtify ~visit_loc def =
  iter_value (mark_loc ~visit_loc) (mark_parsed ~locs_to_dirtify ~visit_loc) def

and mark_def ~locs_to_dirtify ~visit_loc def =
  iter_def (mark_loc ~visit_loc) (mark_parsed ~locs_to_dirtify ~visit_loc) def

and mark_annot ~locs_to_dirtify ~visit_loc t =
  iter_annot (mark_loc ~visit_loc) (mark_parsed ~locs_to_dirtify ~visit_loc) t

and mark_fun ~locs_to_dirtify ~visit_loc def =
  iter_fun_sig (mark_loc ~visit_loc) (mark_parsed ~locs_to_dirtify ~visit_loc) def

and mark_component ~locs_to_dirtify ~visit_loc def =
  iter_component_sig (mark_loc ~visit_loc) (mark_parsed ~locs_to_dirtify ~visit_loc) def

and mark_class ~locs_to_dirtify ~visit_loc def =
  iter_class_sig (mark_loc ~visit_loc) (mark_parsed ~locs_to_dirtify ~visit_loc) def

and mark_declare_class ~locs_to_dirtify ~visit_loc def =
  iter_declare_class_sig (mark_loc ~visit_loc) (mark_parsed ~locs_to_dirtify ~visit_loc) def

and mark_op ~locs_to_dirtify ~visit_loc op = iter_op (mark_parsed ~locs_to_dirtify ~visit_loc) op

and resolve_value_ref
    ~locs_to_dirtify ~visit_loc (P.Ref ({ ref_loc; name; scope; resolved = _ } as ref)) =
  mark_loc ~visit_loc ref_loc;
  match P.Scope.lookup_value scope name with
  | Some (binding, _) ->
    mark_binding ~locs_to_dirtify binding;
    ref.resolved <- Some binding
  | None -> ref.resolved <- None

and resolve_type_ref
    ~locs_to_dirtify ~visit_loc (P.Ref ({ ref_loc; name; scope; resolved = _ } as ref)) =
  mark_loc ~visit_loc ref_loc;
  match P.Scope.lookup_type scope name with
  | Some (binding, _) ->
    mark_binding ~locs_to_dirtify binding;
    ref.resolved <- Some binding
  | None -> ref.resolved <- None

let mark_export ~locs_to_dirtify = function
  | P.ExportRef ref -> resolve_value_ref ~locs_to_dirtify ~visit_loc:ignore ref
  | P.ExportBinding binding ->
    Local_defs.mark binding (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_local_binding)
  | P.ExportDefaultBinding { default_loc; name = _; binding } ->
    mark_loc ~visit_loc:ignore default_loc;
    Local_defs.mark binding (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_local_binding)
  | P.ExportDefault { default_loc; def } ->
    mark_loc ~visit_loc:ignore default_loc;
    mark_parsed ~locs_to_dirtify ~visit_loc:ignore def
  | P.ExportFrom ref -> Remote_refs.mark ref (mark_and_report_not_dirty mark_remote_binding)

let mark_export_type ~locs_to_dirtify = function
  | P.ExportTypeRef ref -> resolve_type_ref ~locs_to_dirtify ~visit_loc:ignore ref
  | P.ExportTypeBinding binding ->
    Local_defs.mark binding (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_local_binding)
  | P.ExportTypeFrom ref -> Remote_refs.mark ref (mark_and_report_not_dirty mark_remote_binding)

let mark_star (loc, mref) =
  mark_loc ~visit_loc:ignore loc;
  mark_mref mref

let mark_exports
    ~locs_to_dirtify
    file_loc
    (P.Exports { kind; types; type_stars; strict = _; platform_availability_set = _ }) =
  SMap.iter (fun _ t -> mark_export_type ~locs_to_dirtify t) types;
  List.iter mark_star type_stars;
  match kind with
  | P.UnknownModule -> ()
  | P.CJSModule t -> mark_parsed ~locs_to_dirtify ~visit_loc:ignore t
  | P.CJSModuleProps props ->
    mark_loc ~visit_loc:ignore file_loc;
    SMap.iter
      (fun _ (loc, t) ->
        mark_loc ~visit_loc:ignore loc;
        mark_parsed ~locs_to_dirtify ~visit_loc:ignore t)
      props
  | P.CJSDeclareModule props ->
    mark_loc ~visit_loc:ignore file_loc;
    SMap.iter
      (fun _ binding ->
        Local_defs.mark
          binding
          (mark_and_report_should_be_dirtified ~locs_to_dirtify mark_local_binding))
      props
  | P.ESModule { names; stars } ->
    SMap.iter (fun _ t -> mark_export ~locs_to_dirtify t) names;
    List.iter mark_star stars

let mark_builtin_module (loc, exports) =
  mark_loc ~visit_loc:ignore loc;
  mark_exports ~locs_to_dirtify:[] loc exports
