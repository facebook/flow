(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
module Option = Base.Option

let mark_loc loc = Locs.mark loc ignore

let mark_mref mref = Module_refs.mark mref ignore

let rec mark_parsed = function
  | P.Annot t -> mark_annot t
  | P.Value def -> mark_value def
  | P.TyRef name -> mark_tyname name
  | P.TyRefApp { loc; name; targs } ->
    mark_loc loc;
    mark_tyname name;
    List.iter mark_parsed targs
  | P.AsyncVoidReturn loc -> mark_loc loc
  | P.BuiltinTyRef { ref_loc; name = _ } -> mark_loc ref_loc
  | P.Err (loc, SigError err) ->
    Signature_error.iter mark_loc err;
    mark_loc loc
  | P.Err (loc, _) -> mark_loc loc
  | P.ValRef ref -> resolve_ref ref
  | P.Pattern p -> Patterns.mark p mark_pattern
  | P.Eval (loc, t, op) ->
    mark_loc loc;
    mark_parsed t;
    mark_op op
  | P.Require { loc; mref } ->
    mark_loc loc;
    mark_mref mref
  | P.ModuleRef { loc; mref } ->
    mark_loc loc;
    mark_mref mref

and mark_tyname = function
  | P.Unqualified ref -> resolve_ref ref
  | P.Qualified { loc; id_loc; name = _; qualification } ->
    mark_loc loc;
    mark_loc id_loc;
    mark_tyname qualification

and mark_binding = function
  | P.LocalBinding node -> Local_defs.mark node mark_local_binding
  | P.RemoteBinding node -> Remote_refs.mark node mark_remote_binding

and mark_local_binding = function
  | P.TypeBinding { id_loc = _; def } -> mark_def (Lazy.force def)
  | P.VarBinding { id_loc; name = _; def }
  | P.LetConstBinding { id_loc; name = _; def } ->
    mark_loc id_loc;
    mark_parsed (Lazy.force def)
  | P.ConstRefBinding { id_loc; name = _; ref } ->
    mark_loc id_loc;
    resolve_ref ref
  | P.ConstFunBinding { id_loc; name = _; loc; async = _; generator = _; def; statics } ->
    mark_loc id_loc;
    mark_loc loc;
    mark_fun (Lazy.force def);
    SMap.iter
      (fun _ (id_loc, def) ->
        mark_loc id_loc;
        mark_parsed def)
      statics
  | P.FunBinding { id_loc; name = _; async = _; generator = _; fn_loc; def; statics } ->
    mark_loc id_loc;
    mark_loc fn_loc;
    mark_fun (Lazy.force def);
    SMap.iter
      (fun _ (id_loc, def) ->
        mark_loc id_loc;
        mark_parsed def)
      statics
  | P.ClassBinding { id_loc; name = _; def } ->
    mark_loc id_loc;
    mark_class (Lazy.force def)
  | P.DeclareClassBinding { id_loc; name = _; def } ->
    mark_loc id_loc;
    mark_declare_class (Lazy.force def)
  | P.DeclareFunBinding { name = _; defs_rev } ->
    Nel.iter
      (fun (id_loc, fn_loc, def) ->
        mark_loc id_loc;
        mark_loc fn_loc;
        mark_fun (Lazy.force def))
      defs_rev
  | P.EnumBinding { id_loc; name = _; def } ->
    mark_loc id_loc;
    begin
      match def with
      | None -> ()
      | Some (lazy (rep, members, has_unknown_members)) ->
        ignore rep;
        ignore has_unknown_members;
        SMap.iter (fun _ -> mark_loc) members
    end

and mark_remote_binding = function
  | P.ImportBinding { id_loc; name = _; mref; remote = _ }
  | P.ImportTypeBinding { id_loc; name = _; mref; remote = _ }
  | P.ImportTypeofBinding { id_loc; name = _; mref; remote = _ }
  | P.ImportNsBinding { id_loc; name = _; mref }
  | P.ImportTypeofNsBinding { id_loc; name = _; mref } ->
    mark_loc id_loc;
    mark_mref mref

and mark_pattern = function
  | P.PDef def -> Pattern_defs.mark (Lazy.force def) mark_parsed
  | P.PropP { def; id_loc = loc; name = _ }
  | P.ObjRestP { def; loc; xs = _ }
  | P.IndexP { def; loc; i = _ }
  | P.ArrRestP { def; loc; i = _ } ->
    mark_loc loc;
    Patterns.mark def mark_pattern
  | P.ComputedP { def; elem } ->
    Patterns.mark def mark_pattern;
    Pattern_defs.mark elem mark_parsed
  | P.UnsupportedLiteralP loc -> mark_loc loc

and mark_value def = iter_value mark_loc mark_parsed def

and mark_def def = iter_def mark_loc mark_parsed def

and mark_tparams tparams = iter_tparams mark_loc mark_parsed tparams

and mark_annot t = iter_annot mark_loc mark_parsed t

and mark_fun def = iter_fun_sig mark_loc mark_parsed def

and mark_class def = iter_class_sig mark_loc mark_parsed def

and mark_declare_class def = iter_declare_class_sig mark_loc mark_parsed def

and mark_interface def = iter_interface_sig mark_loc mark_parsed def

and mark_op op = iter_op mark_parsed op

and resolve_ref (P.Ref ({ ref_loc; name; scope; resolved = _ } as ref)) =
  mark_loc ref_loc;
  let b = P.Scope.lookup scope name in
  Option.iter ~f:mark_binding b;
  ref.resolved <- b

let mark_export = function
  | P.ExportRef ref -> resolve_ref ref
  | P.ExportBinding binding -> Local_defs.mark binding mark_local_binding
  | P.ExportDefaultBinding { default_loc; name = _; binding } ->
    mark_loc default_loc;
    Local_defs.mark binding mark_local_binding
  | P.ExportDefault { default_loc; def } ->
    mark_loc default_loc;
    mark_parsed def
  | P.ExportFrom ref -> Remote_refs.mark ref mark_remote_binding

let mark_export_type = function
  | P.ExportTypeRef ref -> resolve_ref ref
  | P.ExportTypeBinding binding -> Local_defs.mark binding mark_local_binding
  | P.ExportTypeFrom ref -> Remote_refs.mark ref mark_remote_binding

let mark_star (loc, mref) =
  mark_loc loc;
  mark_mref mref

let mark_exports file_loc (P.Exports { kind; types; type_stars; strict = _ }) =
  SMap.iter (fun _ t -> mark_export_type t) types;
  List.iter mark_star type_stars;
  match kind with
  | P.UnknownModule -> ()
  | P.CJSModule t -> mark_parsed t
  | P.CJSModuleProps props ->
    mark_loc file_loc;
    SMap.iter
      (fun _ (loc, t) ->
        mark_loc loc;
        mark_parsed t)
      props
  | P.ESModule { names; stars } ->
    SMap.iter (fun _ t -> mark_export t) names;
    List.iter mark_star stars

let mark_builtin_module (loc, exports) =
  mark_loc loc;
  mark_exports loc exports
