(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The pack phase performs a walk over the graph of names which are reachable
 * from the exports of a parsed signature. As it walks, it also converts the
 * parsed signature to a packed signature.
 *
 * Before packing a signature, it is necessary to first complete a marking pass
 * over the signature and compact any tables, to ensure that the `index_exn`
 * function calls in this module will succeed.
 *
 * The packed signature replaces parsed references, which are (string, scope)
 * pairs, with offsets to a dense array of packed, outlined definitions. Note
 * that references to other modules and builtins remain unresolved at this
 * point. These references will be resolved later.
 *
 * Signature verification errors, which are stored as part of the parsed
 * signature, are hoisted out in this phase and replaced with a packed `Err`
 * type, which can be efficiently stored and converted to AnyT when used for
 * checking.
 *
 * The packed representation is designed to be compact and suitable for storage
 * in shared memory. Packed signatures can be constructed and stored during
 * Flow's parsing phase.
 *)

open Type_sig
open Type_sig_collections
module P = Type_sig_parse
module Fn = Base.Fn

type 'loc remote_ref =
  | Import of {
      id_loc: 'loc;
      name: string;
      index: Module_refs.index;
      remote: string;
    }
  | ImportType of {
      id_loc: 'loc;
      name: string;
      index: Module_refs.index;
      remote: string;
    }
  | ImportTypeof of {
      id_loc: 'loc;
      name: string;
      index: Module_refs.index;
      remote: string;
    }
  | ImportTypeofNs of {
      id_loc: 'loc;
      name: string;
      index: Module_refs.index;
    }
  | ImportNs of {
      id_loc: 'loc;
      name: string;
      index: Module_refs.index;
    }
[@@deriving map, show { with_path = false }]

type 'loc packed_ref =
  | LocalRef of {
      ref_loc: 'loc;
      index: Local_defs.index;
    }
  | RemoteRef of {
      ref_loc: 'loc;
      index: Remote_refs.index;
    }
  | BuiltinRef of {
      ref_loc: 'loc;
      name: string;
    }
[@@deriving map, show { with_path = false }]

type 'loc tyref =
  | Unqualified of 'loc packed_ref
  | Qualified of {
      loc: 'loc;
      id_loc: 'loc;
      name: string;
      qualification: 'loc tyref;
    }
[@@deriving map, show { with_path = false }]

type 'loc packed =
  | Annot of 'loc packed_annot
  | Value of 'loc packed_value
  | Ref of 'loc packed_ref
  | TyRef of 'loc tyref
  | TyRefApp of {
      loc: 'loc;
      name: 'loc tyref;
      targs: 'loc packed list;
    }
  | AsyncVoidReturn of 'loc
  | Pattern of Patterns.index
  | Err of 'loc
  | Eval of 'loc * 'loc packed * 'loc packed op
  | Require of {
      loc: 'loc;
      index: Module_refs.index;
    }
  | ModuleRef of {
      loc: 'loc;
      index: Module_refs.index;
    }

and 'loc packed_value = ('loc, 'loc packed) value

and 'loc packed_annot = ('loc, 'loc packed) annot [@@deriving map, show { with_path = false }]

type 'loc packed_def = ('loc, 'loc packed) def [@@deriving map, show { with_path = false }]

type 'loc export =
  | ExportRef of 'loc packed_ref
  | ExportBinding of Local_defs.index
  | ExportDefault of { default_loc: 'loc }
  | ExportDefaultBinding of {
      default_loc: 'loc;
      index: Local_defs.index;
    }
  | ExportFrom of Remote_refs.index
[@@deriving map, show { with_path = false }]

type 'loc export_type =
  | ExportTypeRef of 'loc packed_ref
  | ExportTypeBinding of Local_defs.index
  | ExportTypeFrom of Remote_refs.index
[@@deriving map, show { with_path = false }]

type 'loc cjs_exports = {
  types: 'loc export_type smap;
  type_stars: ('loc * Module_refs.index) list;
  strict: bool;
}
[@@deriving map, show { with_path = false }]

type 'loc es_exports = {
  names: 'loc export smap;
  types: 'loc export_type smap;
  stars: ('loc * Module_refs.index) list;
  type_stars: ('loc * Module_refs.index) list;
  strict: bool;
}
[@@deriving map, show { with_path = false }]

type 'loc exports =
  | CJSExports of 'loc cjs_exports
  | ESExports of 'loc es_exports
[@@deriving map, show { with_path = false }]

type 'loc pattern =
  | PDef of Pattern_defs.index
  | PropP of {
      id_loc: 'loc;
      name: string;
      def: Patterns.index;
    }
  | ComputedP of {
      elem: Pattern_defs.index;
      def: Patterns.index;
    }
  | UnsupportedLiteralP of 'loc
  | ObjRestP of {
      loc: 'loc;
      xs: string list;
      def: Patterns.index;
    }
  | IndexP of {
      loc: 'loc;
      i: int;
      def: Patterns.index;
    }
  | ArrRestP of {
      loc: 'loc;
      i: int;
      def: Patterns.index;
    }
[@@deriving map, show { with_path = false }]

type 'loc cx = { mutable errs: (Locs.index * 'loc errno) list }

let create_cx () = { errs = [] }

let pack_loc loc = Locs.index_exn loc

let pack_star (loc, mref) = (pack_loc loc, Module_refs.index_exn mref)

let rec pack_parsed cx = function
  | P.Annot t -> Annot (pack_annot cx t)
  | P.Value def -> Value (pack_value cx def)
  | P.TyRef name -> TyRef (pack_tyname cx name)
  | P.TyRefApp { loc; name; targs } -> pack_tyapp cx loc name targs
  | P.AsyncVoidReturn loc -> AsyncVoidReturn (pack_loc loc)
  | P.BuiltinTyRef { ref_loc; name } ->
    TyRef (Unqualified (BuiltinRef { ref_loc = pack_loc ref_loc; name }))
  | P.Err (loc, err) ->
    let loc = pack_loc loc in
    let err = map_errno pack_loc err in
    cx.errs <- (loc, err) :: cx.errs;
    Err loc
  | P.ValRef ref -> Ref (pack_ref ref)
  | P.Pattern p -> Pattern (Patterns.index_exn p)
  | P.Eval (loc, t, op) -> Eval (pack_loc loc, pack_parsed cx t, pack_op cx op)
  | P.Require { loc; mref } ->
    let loc = pack_loc loc in
    let index = Module_refs.index_exn mref in
    Require { loc; index }
  | P.ModuleRef { loc; mref } ->
    let loc = pack_loc loc in
    let index = Module_refs.index_exn mref in
    ModuleRef { loc; index }

and pack_tyapp cx loc name targs =
  let loc = pack_loc loc in
  let name = pack_tyname cx name in
  let targs = List.map (pack_parsed cx) targs in
  TyRefApp { loc; name; targs }

and pack_tyname cx = function
  | P.Unqualified ref -> Unqualified (pack_ref ref)
  | P.Qualified { loc; id_loc; name; qualification } ->
    let loc = pack_loc loc in
    let id_loc = pack_loc id_loc in
    let qualification = pack_tyname cx qualification in
    Qualified { loc; id_loc; name; qualification }

and pack_ref (P.Ref { ref_loc; name; scope = _; resolved }) =
  let ref_loc = pack_loc ref_loc in
  match resolved with
  | None -> BuiltinRef { ref_loc; name }
  | Some (P.LocalBinding b) ->
    let index = Local_defs.index_exn b in
    LocalRef { ref_loc; index }
  | Some (P.RemoteBinding b) ->
    let index = Remote_refs.index_exn b in
    RemoteRef { ref_loc; index }

and pack_local_binding cx = function
  | P.TypeBinding { id_loc = _; def } -> pack_def cx (Lazy.force def)
  | P.VarBinding { id_loc; name; def }
  | P.LetConstBinding { id_loc; name; def } ->
    let id_loc = pack_loc id_loc in
    let def = pack_parsed cx (Lazy.force def) in
    Variable { id_loc; name; def }
  | P.ConstRefBinding { id_loc; name; ref } ->
    let id_loc = pack_loc id_loc in
    let def = Ref (pack_ref ref) in
    Variable { id_loc; name; def }
  | P.ConstFunBinding { id_loc; name; loc; async; generator; def; statics } ->
    let id_loc = pack_loc id_loc in
    let loc = pack_loc loc in
    let def = pack_fun cx (Lazy.force def) in
    let statics =
      SMap.map
        (fun (id_loc, t) ->
          let id_loc = pack_loc id_loc in
          let t = pack_parsed cx t in
          (id_loc, t))
        statics
    in
    let def = Value (FunExpr { loc; async; generator; def; statics }) in
    Variable { id_loc; name; def }
  | P.ClassBinding { id_loc; name; def } ->
    let id_loc = pack_loc id_loc in
    let def = pack_class cx (Lazy.force def) in
    ClassBinding { id_loc; name; def }
  | P.DeclareClassBinding { id_loc; name; def } ->
    let id_loc = pack_loc id_loc in
    let def = pack_declare_class cx (Lazy.force def) in
    DeclareClassBinding { id_loc; name; def }
  | P.FunBinding { id_loc; name; async; generator; fn_loc; def; statics } ->
    let id_loc = pack_loc id_loc in
    let fn_loc = pack_loc fn_loc in
    let def = pack_fun cx (Lazy.force def) in
    let statics =
      SMap.map
        (fun (id_loc, t) ->
          let id_loc = pack_loc id_loc in
          let t = pack_parsed cx t in
          (id_loc, t))
        statics
    in
    FunBinding { id_loc; name; async; generator; fn_loc; def; statics }
  | P.DeclareFunBinding { name; defs_rev } ->
    let ((id_loc, fn_loc, def), tail) =
      Nel.rev_map
        (fun (id_loc, fn_loc, def) ->
          let id_loc = pack_loc id_loc in
          let fn_loc = pack_loc fn_loc in
          let def = pack_fun cx (Lazy.force def) in
          (id_loc, fn_loc, def))
        defs_rev
    in
    DeclareFun { id_loc; name; fn_loc; def; tail }
  | P.EnumBinding { id_loc; name; def } ->
    let id_loc = pack_loc id_loc in
    begin
      match def with
      | None -> DisabledEnumBinding { id_loc; name }
      | Some (lazy (rep, members, has_unknown_members)) ->
        let members = SMap.map pack_loc members in
        EnumBinding { id_loc; name; rep; members; has_unknown_members }
    end

and pack_remote_binding = function
  | P.ImportBinding { id_loc; name; mref; remote } ->
    let id_loc = pack_loc id_loc in
    let index = Module_refs.index_exn mref in
    Import { id_loc; name; index; remote }
  | P.ImportTypeBinding { id_loc; name; mref; remote } ->
    let id_loc = pack_loc id_loc in
    let index = Module_refs.index_exn mref in
    ImportType { id_loc; name; index; remote }
  | P.ImportTypeofBinding { id_loc; name; mref; remote } ->
    let id_loc = pack_loc id_loc in
    let index = Module_refs.index_exn mref in
    ImportTypeof { id_loc; name; index; remote }
  | P.ImportNsBinding { id_loc; name; mref } ->
    let id_loc = pack_loc id_loc in
    let index = Module_refs.index_exn mref in
    ImportNs { id_loc; name; index }
  | P.ImportTypeofNsBinding { id_loc; name; mref } ->
    let id_loc = pack_loc id_loc in
    let index = Module_refs.index_exn mref in
    ImportTypeofNs { id_loc; name; index }

and pack_pattern = function
  | P.PDef def ->
    let def = Pattern_defs.index_exn (Lazy.force def) in
    PDef def
  | P.PropP { id_loc; name; def } ->
    let id_loc = pack_loc id_loc in
    let def = Patterns.index_exn def in
    PropP { id_loc; name; def }
  | P.ComputedP { elem; def } ->
    let elem = Pattern_defs.index_exn elem in
    let def = Patterns.index_exn def in
    ComputedP { elem; def }
  | P.UnsupportedLiteralP loc -> UnsupportedLiteralP (pack_loc loc)
  | P.ObjRestP { loc; xs; def } ->
    let loc = pack_loc loc in
    let def = Patterns.index_exn def in
    ObjRestP { loc; xs; def }
  | P.IndexP { loc; i; def } ->
    let loc = pack_loc loc in
    let def = Patterns.index_exn def in
    IndexP { loc; i; def }
  | P.ArrRestP { loc; i; def } ->
    let loc = pack_loc loc in
    let def = Patterns.index_exn def in
    ArrRestP { loc; i; def }

and pack_exports cx file_loc (P.Exports { kind; types; type_stars; strict }) =
  let types = SMap.map pack_export_type types in
  let type_stars = List.map pack_star type_stars in
  match kind with
  | P.UnknownModule -> (CJSExports { types; type_stars; strict }, None)
  | P.CJSModule t ->
    let export_def = Some (pack_parsed cx t) in
    (CJSExports { types; type_stars; strict }, export_def)
  | P.CJSModuleProps props ->
    let file_loc = pack_loc file_loc in
    let props =
      SMap.map
        (fun (id_loc, t) ->
          let id_loc = pack_loc id_loc in
          let t = pack_parsed cx t in
          ObjValueField (id_loc, t, Polarity.Neutral))
        props
    in
    let export_def = Some (Value (ObjLit { loc = file_loc; frozen = true; proto = None; props })) in
    (CJSExports { types; type_stars; strict }, export_def)
  | P.ESModule { names; stars } ->
    let export_def = ref None in
    let names = SMap.map (pack_export cx export_def) names in
    let stars = List.map pack_star stars in
    (ESExports { names; types; stars; type_stars; strict }, !export_def)

and pack_export cx export_def = function
  | P.ExportRef ref ->
    let ref = pack_ref ref in
    ExportRef ref
  | P.ExportBinding binding ->
    let index = Local_defs.index_exn binding in
    ExportBinding index
  | P.ExportDefault { default_loc; def } ->
    let default_loc = pack_loc default_loc in
    let def = pack_parsed cx def in
    export_def := Some def;
    ExportDefault { default_loc }
  | P.ExportDefaultBinding { default_loc; name = _; binding } ->
    let default_loc = pack_loc default_loc in
    let index = Local_defs.index_exn binding in
    ExportDefaultBinding { default_loc; index }
  | P.ExportFrom ref ->
    let index = Remote_refs.index_exn ref in
    ExportFrom index

and pack_export_type = function
  | P.ExportTypeRef ref ->
    let ref = pack_ref ref in
    ExportTypeRef ref
  | P.ExportTypeBinding binding ->
    let index = Local_defs.index_exn binding in
    ExportTypeBinding index
  | P.ExportTypeFrom ref ->
    let index = Remote_refs.index_exn ref in
    ExportTypeFrom index

and pack_value cx def = map_value pack_loc (pack_parsed cx) def

and pack_def cx def = map_def pack_loc (pack_parsed cx) def

and pack_tparams cx tparams = map_tparams pack_loc (pack_parsed cx) tparams

and pack_annot cx t = map_annot pack_loc (pack_parsed cx) t

and pack_fun cx def = map_fun_sig pack_loc (pack_parsed cx) def

and pack_class cx def = map_class_sig pack_loc (pack_parsed cx) def

and pack_declare_class cx def = map_declare_class_sig pack_loc (pack_parsed cx) def

and pack_interface cx def = map_interface_sig pack_loc (pack_parsed cx) def

and pack_op cx op = map_op (pack_parsed cx) op

and pack_builtin = function
  | P.LocalBinding b -> Local_defs.index_exn b
  | P.RemoteBinding _ -> failwith "unexpected remote builtin"
