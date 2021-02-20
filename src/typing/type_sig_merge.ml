(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type_sig
open Type_sig_collections
module Pack = Type_sig_pack
module Option = Base.Option
module Fn = Base.Fn

module Component : sig
  type 'a t

  type index = private int

  val make : 'a Nel.t -> (index -> 'a -> 'b) -> 'b t

  val leader : 'a t -> 'a

  val get : 'a t -> index -> 'a

  val map : ('a -> 'b) -> 'a t -> 'b t

  val iter : ('a -> unit) -> 'a t -> unit

  val iteri : (index -> 'a -> unit) -> 'a t -> unit
end = struct
  type 'a t = 'a array

  type index = int

  let make (x, xs) f =
    let len = 1 + List.length xs in
    let component = Array.make len (f 0 x) in
    List.iteri (fun i x -> component.(i + 1) <- f (i + 1) x) xs;
    component

  external get : 'a t -> index -> 'a = "%array_unsafe_get"

  let leader xs = get xs 0

  let map = Array.map

  let iter = Array.iter

  let iteri = Array.iteri
end

type file = {
  key: File_key.t;
  cx: Context.t;
  dependencies: (string * (ALoc.t -> Type.t)) Module_refs.t;
  exports: unit -> Type.t;
  export_def: Type.t Lazy.t option;
  local_defs: (unit -> ALoc.t * string * Type.t) Local_defs.t;
  remote_refs: (unit -> ALoc.t * string * Type.t) Remote_refs.t;
  patterns: Type.t Lazy.t Patterns.t;
  pattern_defs: Type.t Lazy.t Pattern_defs.t;
}

let visit f = f ()

let def_reason = function
  | TypeAlias { id_loc; name; _ }
  | OpaqueType { id_loc; name; _ } ->
    Type.DescFormat.type_reason (Reason.OrdinaryName name) id_loc
  | Interface { id_loc; name; _ }
  | ClassBinding { id_loc; name; _ }
  | DeclareClassBinding { id_loc; name; _ } ->
    Type.DescFormat.instance_reason (Reason.OrdinaryName name) id_loc
  | FunBinding { fn_loc; _ } ->
    (* RFunctionType should be Reason.func_reason instead, but this matches the
     * behavior of types-first where function bindings are converted to declared
     * functions, which are given a RFunctionType reason.
     *
     * TODO Fix once T71257430 is closed. *)
    Reason.(mk_reason RFunctionType fn_loc)
  | DeclareFun { id_loc; _ } -> Reason.(mk_reason RFunctionType id_loc)
  | Variable { id_loc; name; _ } -> Reason.(mk_reason (RIdentifier (OrdinaryName name)) id_loc)
  | DisabledEnumBinding { id_loc; name; _ }
  | EnumBinding { id_loc; name; _ } ->
    Reason.(mk_reason (REnum name) id_loc)

let remote_ref_reason = function
  | Pack.Import { id_loc; name; _ }
  | Pack.ImportNs { id_loc; name; _ } ->
    Reason.(mk_reason (RIdentifier (OrdinaryName name)) id_loc)
  | Pack.ImportType { id_loc; name; _ }
  | Pack.ImportTypeof { id_loc; name; _ }
  | Pack.ImportTypeofNs { id_loc; name; _ } ->
    Type.DescFormat.type_reason (Reason.OrdinaryName name) id_loc

let obj_lit_reason ~frozen loc =
  let open Reason in
  let desc =
    if frozen then
      RFrozen RObjectLit
    else
      RObjectLit
  in
  mk_reason desc loc

let trust = Trust.bogus_trust ()

let specialize file t =
  let open Type in
  let open TypeUtil in
  let reason = reason_of_t t in
  Tvar.mk_derivable_where file.cx reason (fun tout ->
      Flow_js.flow file.cx (t, SpecializeT (unknown_use, reason, reason, None, None, tout)))

let eval_unary file loc t =
  let module U = Flow_ast.Expression.Unary in
  function
  | U.Minus ->
    let reason = Reason.mk_reason (TypeUtil.desc_of_t t) loc in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.UnaryMinusT (reason, tout)))
  | U.Plus -> Type.NumT.at loc trust
  | U.Not ->
    let reason = Reason.(mk_reason (RUnaryOperator ("not", TypeUtil.desc_of_t t)) loc) in
    Tvar.mk_no_wrap_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.NotT (reason, tout)))
  | U.BitNot -> Type.NumT.at loc trust
  | U.Typeof -> Type.StrT.at loc trust
  | U.Void -> Type.VoidT.at loc trust
  | U.Delete -> Type.BoolT.at loc trust
  | U.Await ->
    let reason = Reason.(mk_reason (RCustom "await") loc) in
    let await = Flow_js.get_builtin file.cx (Reason.OrdinaryName "$await") reason in
    (* TODO: use_op *)
    let use_op = Type.unknown_use in
    Tvar.mk_no_wrap_where file.cx reason (fun tout ->
        Flow_js.flow
          file.cx
          ( await,
            Type.CallT (use_op, reason, Type.mk_functioncalltype reason None [Type.Arg t] tout) ))

let eval file loc t = function
  | Unary op -> eval_unary file loc t op
  | GetProp name ->
    let reason = Reason.(mk_reason (RProperty (Some (OrdinaryName name))) loc) in
    (* TODO: use_op *)
    let use_op = Type.unknown_use in
    Tvar.mk_no_wrap_where file.cx reason (fun tout ->
        Flow_js.flow
          file.cx
          (t, Type.GetPropT (use_op, reason, Type.Named (reason, Reason.OrdinaryName name), tout)))
  | GetElem index ->
    let reason = Reason.(mk_reason (RProperty None) loc) in
    (* TODO: use_op *)
    let use_op = Type.unknown_use in
    Tvar.mk_no_wrap_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.GetElemT (use_op, reason, index, tout)))

let async_void_return file loc =
  let t = Type.VoidT.at loc trust in
  let reason = Reason.(mk_reason (RCustom "async return") loc) in
  Flow_js.get_builtin_typeapp
    file.cx
    reason
    (Reason.OrdinaryName "Promise")
    [
      Tvar.mk_derivable_where file.cx reason (fun tvar ->
          let funt = Flow_js.get_builtin file.cx (Reason.OrdinaryName "$await") reason in
          let callt = Type.mk_functioncalltype reason None [Type.Arg t] (Type.open_tvar tvar) in
          let reason =
            Reason.repos_reason (Reason.aloc_of_reason (TypeUtil.reason_of_t t)) reason
          in
          Flow_js.flow file.cx (funt, Type.CallT (Type.unknown_use, reason, callt)));
    ]

let add_default_constructor reason extends props =
  match extends with
  | ClassExplicitExtends _
  | ClassExplicitExtendsApp _ ->
    props
  | ClassImplicitExtends
  | ObjectPrototypeExtendsNull ->
    SMap.update
      "constructor"
      (function
        | None ->
          let reason = Reason.(replace_desc_reason RDefaultConstructor reason) in
          let return = Type.VoidT.why reason trust in
          let statics = Type.dummy_static reason in
          let proto = Type.dummy_prototype in
          let funtype = Type.mk_boundfunctiontype [] return ~rest_param:None ~def_reason:reason in
          Some Type.(Method (None, DefT (reason, trust, FunT (statics, proto, funtype))))
        | prop -> prop)
      props

let add_name_field reason =
  let f = function
    | Some _ as p -> p
    | None ->
      let open Type in
      Some (Field (None, StrT.why reason trust, Polarity.Neutral))
  in
  SMap.update "name" f

let require file loc index =
  let (mref, mk_module_t) = Module_refs.get file.dependencies index in
  let reason = Reason.(mk_reason (RCommonJSExports mref) loc) in
  Tvar.mk_where file.cx reason (fun tout ->
      Flow_js.flow file.cx (mk_module_t loc, Type.CJSRequireT (reason, tout, false)))

let import file reason id_loc index kind ~remote ~local =
  let (mref, mk_module_t) = Module_refs.get file.dependencies index in
  Tvar.mk_where file.cx reason (fun tout ->
      Flow_js.flow
        file.cx
        ( mk_module_t id_loc,
          if remote = "default" then
            Type.ImportDefaultT (reason, kind, (local, mref), tout, false)
          else
            Type.ImportNamedT (reason, kind, remote, mref, tout, false) ))

let import_ns file reason id_loc index =
  let (_, mk_module_t) = Module_refs.get file.dependencies index in
  Tvar.mk_where file.cx reason (fun tout ->
      Flow_js.flow file.cx (mk_module_t id_loc, Type.ImportModuleNsT (reason, tout, false)))

let import_typeof_ns file reason id_loc index =
  let (_, mk_module_t) = Module_refs.get file.dependencies index in
  let ns_t =
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (mk_module_t id_loc, Type.ImportModuleNsT (reason, tout, false)))
  in
  Tvar.mk_where file.cx reason (fun tout ->
      Flow_js.flow file.cx (ns_t, Type.ImportTypeofT (reason, "*", tout)))

let merge_enum file reason id_loc enum_name rep members has_unknown_members =
  let rep_reason desc = Reason.(mk_reason (REnumRepresentation desc) id_loc) in
  let rep_t desc def_t = Type.DefT (rep_reason desc, trust, def_t) in
  let representation_t =
    let open Type in
    match rep with
    | BoolRep lit -> rep_t Reason.RBoolean (BoolT lit)
    | NumberRep { truthy } ->
      let lit =
        if truthy then
          Truthy
        else
          AnyLiteral
      in
      rep_t Reason.RNumber (NumT lit)
    | StringRep { truthy } ->
      let lit =
        if truthy then
          Truthy
        else
          AnyLiteral
      in
      rep_t Reason.RString (StrT lit)
    | SymbolRep -> rep_t Reason.RSymbol SymbolT
  in
  let enum_id = Context.make_aloc_id file.cx id_loc in
  Type.(
    DefT
      ( reason,
        trust,
        EnumObjectT { enum_id; enum_name; members; representation_t; has_unknown_members } ))

let merge_pattern file = function
  | Pack.PDef i -> Lazy.force (Pattern_defs.get file.pattern_defs i)
  | Pack.PropP { id_loc; name; def } ->
    let t = Lazy.force (Patterns.get file.patterns def) in
    let reason = Reason.(mk_reason (RProperty (Some (Reason.OrdinaryName name))) id_loc) in
    (* TODO: use_op *)
    let use_op = Type.unknown_use in
    Tvar.mk_no_wrap_where file.cx reason (fun tout ->
        Flow_js.flow
          file.cx
          (t, Type.GetPropT (use_op, reason, Type.Named (reason, Reason.OrdinaryName name), tout)))
  | Pack.ComputedP { elem; def } ->
    let elem = Lazy.force (Pattern_defs.get file.pattern_defs elem) in
    let t = Lazy.force (Patterns.get file.patterns def) in
    let loc = TypeUtil.loc_of_t elem in
    let reason = Reason.(mk_reason (RProperty None) loc) in
    (* TODO: use_op *)
    let use_op = Type.unknown_use in
    Tvar.mk_no_wrap_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.GetElemT (use_op, reason, elem, tout)))
  | Pack.UnsupportedLiteralP loc -> Type.(AnyT.at (AnyError None) loc)
  | Pack.ObjRestP { loc; xs; def } ->
    let t = Lazy.force (Patterns.get file.patterns def) in
    let reason = Reason.(mk_reason RObjectPatternRestProp loc) in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.ObjRestT (reason, xs, tout, Reason.mk_id ())))
  | Pack.IndexP { loc; i; def } ->
    let t = Lazy.force (Patterns.get file.patterns def) in
    let reason = Reason.(mk_reason (RCustom (Utils_js.spf "element %d" i)) loc) in
    let i =
      let reason = Reason.(mk_reason RNumber loc) in
      Type.(DefT (reason, trust, NumT (Literal (None, (float i, string_of_int i)))))
    in
    (* TODO: use_op *)
    let use_op = Type.unknown_use in
    Tvar.mk_no_wrap_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.GetElemT (use_op, reason, i, tout)))
  | Pack.ArrRestP { loc; i; def } ->
    let t = Lazy.force (Patterns.get file.patterns def) in
    let reason = Reason.(mk_reason RArrayPatternRestProp loc) in
    (* TODO: use_op *)
    let use_op = Type.unknown_use in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.ArrRestT (use_op, reason, i, tout)))

let merge_remote_ref file reason = function
  | Pack.Import { id_loc; name; index; remote } ->
    import file reason id_loc index Type.ImportValue ~remote ~local:name
  | Pack.ImportType { id_loc; name; index; remote } ->
    import file reason id_loc index Type.ImportType ~remote ~local:name
  | Pack.ImportTypeof { id_loc; name; index; remote } ->
    import file reason id_loc index Type.ImportTypeof ~remote ~local:name
  | Pack.ImportNs { id_loc; name = _; index } -> import_ns file reason id_loc index
  | Pack.ImportTypeofNs { id_loc; name = _; index } -> import_typeof_ns file reason id_loc index

let merge_ref : 'a. _ -> (_ -> _ -> _ -> 'a) -> _ -> 'a =
 fun file f ref ->
  match ref with
  | Pack.LocalRef { ref_loc; index } ->
    let (_loc, name, t) = visit (Local_defs.get file.local_defs index) in
    let t = Flow_js.reposition file.cx ref_loc t in
    f t ref_loc name
  | Pack.RemoteRef { ref_loc; index } ->
    let (_loc, name, t) = visit (Remote_refs.get file.remote_refs index) in
    let t = Flow_js.reposition file.cx ref_loc t in
    f t ref_loc name
  | Pack.BuiltinRef { ref_loc; name } ->
    let reason = Reason.(mk_reason (RIdentifier (Reason.OrdinaryName name)) ref_loc) in
    let t = Flow_js.get_builtin file.cx (Reason.OrdinaryName name) reason in
    f t ref_loc name

let rec merge_tyref file f = function
  | Pack.Unqualified ref ->
    let f t loc name = f t loc (Nel.one name) in
    merge_ref file f ref
  | Pack.Qualified { loc; id_loc; name; qualification } ->
    let f t _ names =
      let names = Nel.cons name names in
      let qname = String.concat "." (List.rev (Nel.to_list names)) in
      let id_reason = Reason.(mk_reason (RType (OrdinaryName name)) id_loc) in
      let reason_op = Reason.(mk_reason (RType (OrdinaryName qname)) loc) in
      let use_op = Type.(Op (GetProperty reason_op)) in
      let t =
        Tvar.mk_no_wrap_where file.cx reason_op (fun tout ->
            Flow_js.flow
              file.cx
              ( t,
                Type.(
                  GetPropT (use_op, reason_op, Named (id_reason, Reason.OrdinaryName name), tout))
              ))
      in
      f t loc names
    in
    merge_tyref file f qualification

let merge_export file = function
  | Pack.ExportRef ref -> merge_ref file (fun t ref_loc _ -> (Some ref_loc, t)) ref
  | Pack.ExportBinding index ->
    let (loc, _name, t) = visit (Local_defs.get file.local_defs index) in
    (Some loc, t)
  | Pack.ExportDefault { default_loc } ->
    let (lazy t) = Option.value_exn file.export_def in
    (Some default_loc, t)
  | Pack.ExportDefaultBinding { default_loc; index } ->
    let (_loc, _name, t) = visit (Local_defs.get file.local_defs index) in
    (Some default_loc, t)
  | Pack.ExportFrom index ->
    let (loc, _name, t) = visit (Remote_refs.get file.remote_refs index) in
    (Some loc, t)

let merge_export_type file = function
  | Pack.ExportTypeRef ref -> merge_ref file (fun t ref_loc _ -> (Some ref_loc, t)) ref
  | Pack.ExportTypeBinding index ->
    let (loc, _name, t) = visit (Local_defs.get file.local_defs index) in
    (Some loc, t)
  | Pack.ExportTypeFrom index ->
    let (loc, _name, t) = visit (Remote_refs.get file.remote_refs index) in
    (Some loc, t)

let merge_exports =
  let merge_star file (loc, index) =
    let (_, mk_module_t) = Module_refs.get file.dependencies index in
    (loc, mk_module_t loc)
  in
  let mk_es_module_t file reason is_strict =
    let open Type in
    let exportstype =
      {
        exports_tmap = Context.make_export_map file.cx NameUtils.Map.empty;
        cjs_export = None;
        has_every_named_export = false;
      }
    in
    ModuleT (reason, exportstype, is_strict)
  in
  let mk_commonjs_module_t file reason is_strict t =
    let open Type in
    let exporttypes =
      {
        exports_tmap = Context.make_export_map file.cx NameUtils.Map.empty;
        cjs_export = Some t;
        has_every_named_export = false;
      }
    in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow
          file.cx
          (t, CJSExtractNamedExportsT (reason, (reason, exporttypes, is_strict), tout)))
  in
  let export_named file reason kind named module_t =
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (module_t, Type.ExportNamedT (reason, named, kind, tout)))
  in
  let copy_named_exports file reason module_t (loc, from_ns) =
    let reason = Reason.repos_reason loc reason in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (from_ns, Type.CopyNamedExportsT (reason, module_t, tout)))
  in
  let copy_type_exports file reason module_t (loc, from_ns) =
    let reason = Reason.repos_reason loc reason in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (from_ns, Type.CopyTypeExportsT (reason, module_t, tout)))
  in
  let copy_star_exports =
    let rec loop file reason acc = function
      | ([], []) -> acc
      | (xs, []) -> List.fold_left (copy_named_exports file reason) acc xs
      | ([], ys) -> List.fold_left (copy_type_exports file reason) acc ys
      | ((x :: xs' as xs), (y :: ys' as ys)) ->
        if ALoc.compare (fst x) (fst y) > 0 then
          loop file reason (copy_named_exports file reason acc x) (xs', ys)
        else
          loop file reason (copy_type_exports file reason acc y) (xs, ys')
    in
    (fun file reason stars acc -> loop file reason acc stars)
  in
  fun file reason -> function
    | Pack.CJSExports { types; type_stars; strict } ->
      let value =
        match file.export_def with
        | Some (lazy t) -> t
        | None -> Obj_type.mk_unsealed file.cx reason
      in
      let types = SMap.map (merge_export_type file) types |> NameUtils.namemap_of_smap in
      let type_stars = List.map (merge_star file) type_stars in
      mk_commonjs_module_t file reason strict value
      |> export_named file reason Type.ExportType types
      |> copy_star_exports file reason ([], type_stars)
    | Pack.ESExports { names; types; stars; type_stars; strict } ->
      let names = SMap.map (merge_export file) names |> NameUtils.namemap_of_smap in
      let types = SMap.map (merge_export_type file) types |> NameUtils.namemap_of_smap in
      let stars = List.map (merge_star file) stars in
      let type_stars = List.map (merge_star file) type_stars in
      mk_es_module_t file reason strict
      |> export_named file reason Type.ExportValue names
      |> export_named file reason Type.ExportType types
      |> copy_star_exports file reason (stars, type_stars)

let rec merge file = function
  | Pack.Annot t -> merge_annot file t
  | Pack.Value t -> merge_value file t
  | Pack.Ref ref -> merge_ref file (fun t _ _ -> t) ref
  | Pack.TyRef name ->
    let f t ref_loc (name, _) =
      let reason = Reason.(mk_annot_reason (RType (Reason.OrdinaryName name)) ref_loc) in
      Flow_js.mk_instance file.cx reason t
    in
    merge_tyref file f name
  | Pack.TyRefApp { loc; name; targs } ->
    let targs = List.map (merge file) targs in
    let f t _ _ = TypeUtil.typeapp_annot loc t targs in
    merge_tyref file f name
  | Pack.AsyncVoidReturn loc -> async_void_return file loc
  | Pack.Pattern i -> Lazy.force (Patterns.get file.patterns i)
  | Pack.Err loc -> Type.(AnyT.at (AnyError None) loc)
  | Pack.Eval (loc, t, op) ->
    let t = merge file t in
    let op = merge_op file op in
    eval file loc t op
  | Pack.Require { loc; index } -> require file loc index
  | Pack.ModuleRef { loc; index } ->
    let t = require file loc index in
    let reason = Reason.(mk_reason (RCustom "module reference") loc) in
    Flow_js.get_builtin_typeapp file.cx reason (Reason.OrdinaryName "$Flow$ModuleRef") [t]

and merge_annot file = function
  | Any loc -> Type.AnyT.at Type.AnnotatedAny loc
  | Mixed loc -> Type.MixedT.at loc trust
  | Empty loc -> Type.EmptyT.at loc trust
  | Void loc -> Type.VoidT.at loc trust
  | Null loc -> Type.NullT.at loc trust
  | Symbol loc -> Type.SymbolT.at loc trust
  | Number loc -> Type.NumT.at loc trust
  | BigInt loc ->
    let reason = Reason.(mk_annot_reason RBigInt loc) in
    Type.(AnyT.error reason)
  | String loc -> Type.StrT.at loc trust
  | Boolean loc -> Type.BoolT.at loc trust
  | Exists { loc; force } ->
    let reason = Reason.(mk_annot_reason RExistential loc) in
    if force then
      Tvar.mk file.cx reason
    else
      Type.ExistsT reason
  | Optional t -> TypeUtil.optional (merge file t)
  | Maybe (loc, t) ->
    let t = merge file t in
    let desc = TypeUtil.desc_of_t t in
    let reason = Reason.(mk_annot_reason (RMaybe desc) loc) in
    Type.MaybeT (reason, t)
  | Union { loc; t0; t1; ts } ->
    let reason = Reason.(mk_annot_reason RUnionType loc) in
    let t0 = merge file t0 in
    let t1 = merge file t1 in
    (* NB: tail-recursive map in case of very large types *)
    let ts = Base.List.map ~f:(merge file) ts in
    let open Type in
    let rep = UnionRep.make t0 t1 ts in
    UnionRep.optimize
      rep
      ~reasonless_eq:TypeUtil.reasonless_eq
      ~flatten:(Type_mapper.union_flatten file.cx)
      ~find_resolved:(Context.find_resolved file.cx)
      ~find_props:(Context.find_props file.cx);
    UnionT (reason, rep)
  | Intersection { loc; t0; t1; ts } ->
    let reason = Reason.(mk_annot_reason RIntersectionType loc) in
    let t0 = merge file t0 in
    let t1 = merge file t1 in
    (* NB: tail-recursive map in case of very large types *)
    let ts = Base.List.map ~f:(merge file) ts in
    Type.(IntersectionT (reason, InterRep.make t0 t1 ts))
  | Tuple { loc; ts } ->
    let reason = Reason.(mk_annot_reason RTupleType loc) in
    let elem_reason = Reason.(mk_annot_reason RTupleElement loc) in
    (* NB: tail-recursive map in case of very large types *)
    let ts = Base.List.map ~f:(merge file) ts in
    let elem_t =
      match ts with
      | [] -> Type.EmptyT.why elem_reason trust
      | [t] -> t
      | t0 :: t1 :: ts ->
        let rep = Type.UnionRep.make t0 t1 ts in
        Type.UnionT (elem_reason, rep)
    in
    Type.(DefT (reason, trust, ArrT (TupleAT (elem_t, ts))))
  | Array (loc, t) ->
    let reason = Reason.(mk_annot_reason RArrayType loc) in
    let t = merge file t in
    Type.(DefT (reason, trust, ArrT (ArrayAT (t, None))))
  | ReadOnlyArray (loc, t) ->
    let reason = Reason.(mk_annot_reason RROArrayType loc) in
    let t = merge file t in
    Type.(DefT (reason, trust, ArrT (ROArrayAT t)))
  | SingletonString (loc, str) ->
    let reason = Reason.(mk_annot_reason (RStringLit (OrdinaryName str)) loc) in
    Type.(DefT (reason, trust, SingletonStrT (Reason.OrdinaryName str)))
  | SingletonNumber (loc, num, raw) ->
    let reason = Reason.(mk_annot_reason (RNumberLit raw) loc) in
    Type.(DefT (reason, trust, SingletonNumT (num, raw)))
  | SingletonBigInt (loc, raw) ->
    let reason = Reason.(mk_annot_reason (RBigIntLit raw) loc) in
    Type.(AnyT.error reason)
  | SingletonBoolean (loc, b) ->
    let reason = Reason.(mk_annot_reason (RBooleanLit b) loc) in
    Type.(DefT (reason, trust, SingletonBoolT b))
  | Typeof { loc; qname; t } ->
    let qname = String.concat "." qname in
    let reason = Reason.(mk_reason (RTypeof qname) loc) in
    let t = merge file t in
    Flow_js.mk_typeof_annotation file.cx reason t
  | Bound { ref_loc; name } ->
    let reason = Reason.(mk_annot_reason (RType (OrdinaryName name)) ref_loc) in
    Type.BoundT (reason, name)
  | TEMPORARY_Number (loc, num, raw) ->
    let reason = Reason.(mk_annot_reason RNumber loc) in
    Type.(DefT (reason, trust, NumT (Literal (None, (num, raw)))))
  | TEMPORARY_String (loc, str) ->
    let reason = Reason.(mk_annot_reason RString loc) in
    Type.(DefT (reason, trust, StrT (Literal (None, Reason.OrdinaryName str))))
  | TEMPORARY_LongString loc ->
    let len = Context.max_literal_length file.cx in
    let reason = Reason.(mk_annot_reason (RLongStringLit len) loc) in
    Type.(DefT (reason, trust, StrT AnyLiteral))
  | TEMPORARY_Boolean (loc, b) ->
    let reason = Reason.(mk_annot_reason RBoolean loc) in
    Type.(DefT (reason, trust, BoolT (Some b)))
  | TEMPORARY_Object t ->
    let t = merge file t in
    let open Type in
    (match t with
    | ExactT (_, DefT (r, trust, ObjT o))
    | DefT (r, trust, ObjT o) ->
      let r = Reason.(replace_desc_reason RObjectLit r) in
      let obj_kind =
        match o.flags.obj_kind with
        | Indexed _ -> o.flags.obj_kind
        | _ -> Type.Exact
      in
      DefT (r, trust, ObjT { o with flags = { o.flags with obj_kind } })
    | EvalT (l, TypeDestructorT (use_op, r, SpreadType (target, ts, head_slice)), id) ->
      let r = Reason.(replace_desc_reason RObjectLit r) in
      EvalT (l, TypeDestructorT (use_op, r, SpreadType (target, ts, head_slice)), id)
    | _ -> t)
  | TEMPORARY_Array (loc, t) ->
    let reason = Reason.(mk_annot_reason RArrayLit loc) in
    let t = merge file t in
    Type.(DefT (reason, trust, ArrT (ArrayAT (t, None))))
  | AnyWithLowerBound (_loc, t) ->
    let t = merge file t in
    Type.AnyT.annot (TypeUtil.reason_of_t t)
  | AnyWithUpperBound (_loc, t) ->
    let t = merge file t in
    Type.AnyT.annot (TypeUtil.reason_of_t t)
  | PropertyType { loc; obj; prop } ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "$PropertyType")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let obj = merge file obj in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(
      EvalT (obj, TypeDestructorT (use_op, reason, Type.PropertyType (Reason.OrdinaryName prop)), id))
  | ElementType { loc; obj; elem } ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "$ElementType")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let obj = merge file obj in
    let elem = merge file elem in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (obj, TypeDestructorT (use_op, reason, Type.ElementType elem), id))
  | NonMaybeType (loc, t) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "$NonMaybeType")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t = merge file t in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (t, TypeDestructorT (use_op, reason, Type.NonMaybeType), id))
  | Shape (_loc, t) ->
    let t = merge file t in
    let desc = TypeUtil.desc_of_t t in
    let loc = TypeUtil.loc_of_t t in
    let reason = Reason.(mk_reason (RShapeOf desc) loc) in
    Type.ShapeT (reason, t)
  | Diff (loc, t1, t2) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "$Diff")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t1 = merge file t1 in
    let t2 = merge file t2 in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(
      EvalT (t1, TypeDestructorT (use_op, reason, RestType (Object.Rest.IgnoreExactAndOwn, t2)), id))
  | ReadOnly (loc, t) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "$ReadOnly")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t = merge file t in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (t, TypeDestructorT (use_op, reason, ReadOnlyType), id))
  | Keys (loc, t) ->
    let reason = Reason.(mk_reason RKeySet loc) in
    let t = merge file t in
    Type.KeysT (reason, t)
  | Values (loc, t) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "$Values")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t = merge file t in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (t, TypeDestructorT (use_op, reason, ValuesType), id))
  | Exact (loc, t) ->
    let t = merge file t in
    let desc = TypeUtil.desc_of_t t in
    let reason = Reason.(mk_annot_reason (RExactType desc) loc) in
    Type.ExactT (reason, t)
  | Rest (loc, t1, t2) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "$Rest")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t1 = merge file t1 in
    let t2 = merge file t2 in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (t1, TypeDestructorT (use_op, reason, RestType (Object.Rest.Sound, t2)), id))
  | ExportsT (loc, ref) ->
    let reason = Reason.(mk_annot_reason (RModule (OrdinaryName ref)) loc) in
    let m_name = Reason.internal_module_name ref in
    let module_t =
      Tvar.mk_no_wrap_where file.cx reason (fun tout ->
          Flow_js.lookup_builtin file.cx m_name reason (Type.Strict reason) tout)
    in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (module_t, Type.CJSRequireT (reason, tout, false)))
  | Call { loc; fn; args } ->
    let reason = Reason.(mk_reason RFunctionCallType loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let fn = merge file fn in
    let args = List.map (merge file) args in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (fn, TypeDestructorT (use_op, reason, CallType args), id))
  | TupleMap { loc; tup; fn } ->
    let reason = Reason.(mk_reason RTupleMap loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let tup = merge file tup in
    let fn = merge file fn in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (tup, TypeDestructorT (use_op, reason, TypeMap (Type.TupleMap fn)), id))
  | ObjMap { loc; obj; fn } ->
    let reason = Reason.(mk_reason RObjectMap loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let obj = merge file obj in
    let fn = merge file fn in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (obj, TypeDestructorT (use_op, reason, TypeMap (ObjectMap fn)), id))
  | ObjMapi { loc; obj; fn } ->
    let reason = Reason.(mk_reason RObjectMapi loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let obj = merge file obj in
    let fn = merge file fn in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (obj, TypeDestructorT (use_op, reason, TypeMap (ObjectMapi fn)), id))
  | CharSet (loc, str) ->
    let chars = String_utils.CharSet.of_string str in
    let char_str = String_utils.CharSet.to_string chars in
    let reason_str = Utils_js.spf "character set `%s`" char_str in
    let reason = Reason.(mk_annot_reason (RCustom reason_str) loc) in
    Type.(DefT (reason, trust, CharSetT chars))
  | ClassT (loc, t) ->
    let t = merge file t in
    let desc = TypeUtil.desc_of_t t in
    let reason = Reason.(mk_reason (RStatics desc) loc) in
    Type.DefT (reason, trust, Type.ClassT t)
  | Function_apply loc ->
    let reason = Reason.(mk_annot_reason RFunctionType loc) in
    Type.FunProtoApplyT reason
  | Function_bind loc ->
    let reason = Reason.(mk_annot_reason RFunctionType loc) in
    Type.FunProtoBindT reason
  | Function_call loc ->
    let reason = Reason.(mk_annot_reason RFunctionType loc) in
    Type.FunProtoCallT reason
  | Object_assign loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.ObjectAssign)
  | Object_getPrototypeOf loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.ObjectGetPrototypeOf)
  | Object_setPrototypeOf loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.ObjectSetPrototypeOf)
  | Compose loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.Compose false)
  | ComposeReverse loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.Compose true)
  | ReactAbstractComponent { loc; config; instance } ->
    let reason = Reason.(mk_reason (RCustom "AbstractComponent") loc) in
    let config = merge file config in
    let instance = merge file instance in
    Type.(DefT (reason, trust, ReactAbstractComponentT { config; instance }))
  | ReactConfig { loc; props; default } ->
    let reason = Reason.(mk_reason RReactConfig loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let props = merge file props in
    let default = merge file default in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (props, TypeDestructorT (use_op, reason, ReactConfigType default), id))
  | ReactPropTypePrimitive (loc, t) ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    let t = merge file t in
    Type.(CustomFunT (reason, ReactPropType (React.PropType.Primitive (false, t))))
  | ReactPropTypePrimitiveRequired (loc, t) ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    let t = merge file t in
    Type.(CustomFunT (reason, ReactPropType (React.PropType.Primitive (true, t))))
  | ReactPropTypeArrayOf loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.(CustomFunT (reason, ReactPropType React.PropType.(Complex ArrayOf)))
  | ReactPropTypeInstanceOf loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.(CustomFunT (reason, ReactPropType React.PropType.(Complex InstanceOf)))
  | ReactPropTypeObjectOf loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.(CustomFunT (reason, ReactPropType React.PropType.(Complex ObjectOf)))
  | ReactPropTypeOneOf loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.(CustomFunT (reason, ReactPropType React.PropType.(Complex OneOf)))
  | ReactPropTypeOneOfType loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.(CustomFunT (reason, ReactPropType React.PropType.(Complex OneOfType)))
  | ReactPropTypeShape loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.(CustomFunT (reason, ReactPropType React.(PropType.Complex PropType.Shape)))
  | ReactCreateClass loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.ReactCreateClass)
  | ReactCreateElement loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.ReactCreateElement)
  | ReactCloneElement loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.ReactCloneElement)
  | ReactElementFactory (loc, t) ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    let t = merge file t in
    Type.CustomFunT (reason, Type.ReactElementFactory t)
  | ReactElementProps (loc, t) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "React$ElementProps")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t = merge file t in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (t, TypeDestructorT (use_op, reason, ReactElementPropsType), id))
  | ReactElementConfig (loc, t) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "React$ElementConfig")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t = merge file t in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (t, TypeDestructorT (use_op, reason, ReactElementConfigType), id))
  | ReactElementRef (loc, t) ->
    let reason = Reason.(mk_reason (RType (OrdinaryName "React$ElementRef")) loc) in
    let use_op = Type.Op (Type.TypeApplication { type' = reason }) in
    let t = merge file t in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (t, TypeDestructorT (use_op, reason, ReactElementRefType), id))
  | FacebookismIdx loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.Idx)
  | FacebookismTypeAssertIs loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.TypeAssertIs)
  | FacebookismTypeAssertThrows loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.TypeAssertThrows)
  | FacebookismTypeAssertWraps loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.TypeAssertWraps)
  | FlowDebugPrint loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.DebugPrint)
  | FlowDebugThrow loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.DebugThrow)
  | FlowDebugSleep loc ->
    let reason = Reason.(mk_reason RFunctionType loc) in
    Type.CustomFunT (reason, Type.DebugSleep)
  | Pred (loc, n) ->
    let open Type in
    let fun_reason = Reason.(mk_annot_reason (RCustom "abstract predicate function") loc) in
    let static_reason = Reason.(mk_reason (RCustom "abstract predicate static") loc) in
    let out_reason = Reason.(mk_reason (RCustom "open predicate") loc) in
    let key_strs = ListUtils.range 0 n |> List.map (fun i -> Some ("x_" ^ Base.Int.to_string i)) in
    let emp = Key_map.empty in
    let tins = Unsoundness.at FunctionPrototype loc |> ListUtils.repeat n in
    let tout =
      OpenPredT { reason = out_reason; base_t = MixedT.at loc trust; m_pos = emp; m_neg = emp }
    in
    let statics = dummy_static static_reason in
    let proto = Unsoundness.function_proto_any Reason.(mk_reason RPrototype loc) in
    let functiontype =
      mk_functiontype
        fun_reason
        tins
        tout
        ~rest_param:None
        ~def_reason:fun_reason
        ~params_names:key_strs
        ~is_predicate:true
    in
    DefT (fun_reason, trust, FunT (statics, proto, functiontype))
  | Refine { loc; base; fn_pred; index } ->
    let reason = Reason.(mk_reason (RCustom "refined type") loc) in
    let base = merge file base in
    let fn_pred = merge file fn_pred in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(EvalT (base, LatentPredT (reason, Type.LatentP (fn_pred, index)), id))
  | Trusted (loc, t) ->
    begin
      match merge file t with
      | Type.DefT (r, trust, def_t) ->
        let reason = Reason.(mk_annot_reason (RTrusted (desc_of_reason r)) loc) in
        Type.(DefT (reason, trust, def_t))
      | _ -> Type.(AnyT.at (AnyError None) loc)
    end
  | Private (loc, t) ->
    begin
      match merge file t with
      | Type.DefT (r, trust, def_t) ->
        let reason = Reason.(mk_annot_reason (RPrivate (desc_of_reason r)) loc) in
        Type.(DefT (reason, trust, def_t))
      | _ -> Type.(AnyT.at (AnyError None) loc)
    end
  | FunAnnot (loc, def) ->
    let reason = Reason.(mk_annot_reason RFunctionType loc) in
    let statics = merge_fun_statics file reason SMap.empty in
    merge_fun file reason def statics
  | ObjAnnot { loc; props; proto; obj_kind } ->
    let reason = Reason.(mk_annot_reason RObjectType loc) in
    let obj_kind =
      match obj_kind with
      | ExactObj -> Type.Exact
      | InexactObj -> Type.Inexact
      | IndexedObj dict -> Type.Indexed ((merge_dict file) dict)
    in
    let props = SMap.map (merge_obj_annot_prop file) props |> NameUtils.namemap_of_smap in
    let mk_object call proto =
      let t = Obj_type.mk_with_proto file.cx reason proto ?call ~props ~obj_kind ~loc in
      if obj_kind = Type.Exact then
        let exact_reason = Reason.(mk_annot_reason (RExactType RObjectType) loc) in
        Type.ExactT (exact_reason, t)
      else
        t
    in
    begin
      match proto with
      | ObjAnnotImplicitProto -> mk_object None (Type.ObjProtoT reason)
      | ObjAnnotExplicitProto (loc, t) ->
        let reason = Reason.(mk_reason RPrototype loc) in
        let proto = Tvar.mk file.cx reason in
        Flow_js.flow file.cx (merge file t, Type.ObjTestProtoT (reason, proto));
        let proto = Flow_js.mk_typeof_annotation file.cx reason proto in
        mk_object None proto
      | ObjAnnotCallable { ts_rev } ->
        let proto = Type.FunProtoT reason in
        let ts =
          Nel.rev_map
            (fun t ->
              let t = merge file t in
              mk_object (Some t) proto)
            ts_rev
        in
        (match ts with
        | (t, []) -> t
        | (t0, t1 :: ts) ->
          let reason = Reason.(mk_annot_reason (RCustom "callable object type") loc) in
          Type.(IntersectionT (reason, InterRep.make t0 t1 ts)))
    end
  | ObjSpreadAnnot { loc; exact; elems_rev } ->
    let reason = Reason.(mk_annot_reason RObjectType loc) in
    let target = Type.Object.Spread.Annot { make_exact = exact } in
    let merge_slice dict props =
      let dict = Option.map ~f:(merge_dict file) dict in
      let prop_map = SMap.map (merge_obj_annot_prop file) props |> NameUtils.namemap_of_smap in
      { Type.Object.Spread.reason; prop_map; dict; generics = Generic.spread_empty }
    in
    let merge_elem = function
      | ObjSpreadAnnotElem t -> Type.Object.Spread.Type (merge file t)
      | ObjSpreadAnnotSlice { dict; props } -> Type.Object.Spread.Slice (merge_slice dict props)
    in
    let (t, todo_rev, head_slice) =
      let open Type.Object.Spread in
      match Nel.map merge_elem elems_rev with
      | (Type t, elems) -> (t, elems, None)
      | (Slice slice, Type t :: elems) -> (t, elems, Some slice)
      | (Slice _, Slice _ :: _) -> failwith "unexpected adjacent slices"
      | (Slice _, []) -> failwith "unexpected solo slice"
    in
    let id = Type.Eval.id_of_aloc_id (Context.make_aloc_id file.cx loc) in
    Type.(
      EvalT (t, TypeDestructorT (unknown_use, reason, SpreadType (target, todo_rev, head_slice)), id))
  | InlineInterface (loc, def) ->
    let reason = Reason.(mk_annot_reason RInterfaceType loc) in
    let id = ALoc.id_none in
    merge_interface ~inline:true file reason id def []

and merge_value file = function
  | ClassExpr (loc, def) ->
    let name = "<<anonymous class>>" in
    let reason = Type.DescFormat.instance_reason (Reason.OrdinaryName name) loc in
    let id = Context.make_aloc_id file.cx loc in
    merge_class file reason id def
  | FunExpr { loc; async = _; generator = _; def; statics } ->
    (* RFunctionType should be Reason.func_reason instead, but this matches the
     * behavior of types-first where function bindings are converted to declared
     * functions, which are given a RFunctionType reason.
     *
     * TODO Fix once T71257430 is closed. *)
    let reason = Reason.(mk_reason RFunctionType loc) in
    let statics = merge_fun_statics file reason statics in
    merge_fun file reason def statics
  | StringVal loc ->
    let reason = Reason.(mk_reason RString loc) in
    Type.(DefT (reason, trust, StrT AnyLiteral))
  | StringLit (loc, lit) ->
    let reason = Reason.(mk_reason RString loc) in
    Type.(DefT (reason, trust, StrT (Literal (None, Reason.OrdinaryName lit))))
  | LongStringLit loc ->
    let len = Context.max_literal_length file.cx in
    let reason = Reason.(mk_annot_reason (RLongStringLit len) loc) in
    Type.(DefT (reason, trust, StrT AnyLiteral))
  | NumberVal loc ->
    let reason = Reason.(mk_reason RNumber loc) in
    Type.(DefT (reason, trust, NumT AnyLiteral))
  | NumberLit (loc, num, raw) ->
    let reason = Reason.(mk_reason RNumber loc) in
    Type.(DefT (reason, trust, NumT (Literal (None, (num, raw)))))
  | BooleanVal loc ->
    let reason = Reason.(mk_reason RBoolean loc) in
    Type.(DefT (reason, trust, BoolT None))
  | BooleanLit (loc, lit) ->
    let reason = Reason.(mk_reason RBoolean loc) in
    Type.(DefT (reason, trust, BoolT (Some lit)))
  | NullLit loc -> Type.NullT.at loc trust
  | ObjLit { loc; frozen; proto; props } ->
    let reason = obj_lit_reason ~frozen loc in
    let proto =
      match proto with
      | None -> Type.ObjProtoT reason
      | Some (loc, t) ->
        let reason = Reason.(mk_reason RPrototype loc) in
        let proto = Tvar.mk file.cx reason in
        Flow_js.flow file.cx (merge file t, Type.ObjTestProtoT (reason, proto));
        Flow_js.mk_typeof_annotation file.cx reason proto
    in
    let props = SMap.map (merge_obj_value_prop file) props |> NameUtils.namemap_of_smap in
    Obj_type.mk_with_proto file.cx reason proto ~obj_kind:Type.Exact ~props ~frozen
  | ObjSpreadLit { loc; frozen; proto; elems_rev } ->
    let reason = obj_lit_reason ~frozen loc in
    (* TODO: fix spread to use provided __proto__ prop *)
    ignore proto;
    let merge_slice props =
      let prop_map = SMap.map (merge_obj_value_prop file) props |> NameUtils.namemap_of_smap in
      { Type.Object.Spread.reason; prop_map; dict = None; generics = Generic.spread_empty }
    in
    let merge_elem = function
      | ObjValueSpreadElem t -> Type.Object.Spread.Type (merge file t)
      | ObjValueSpreadSlice props -> Type.Object.Spread.Slice (merge_slice props)
    in
    let (t, todo_rev, acc) =
      let open Type.Object.Spread in
      match Nel.map merge_elem elems_rev with
      | (Type t, elems) -> (t, elems, [])
      | (Slice slice, Type t :: elems) -> (t, elems, [InlineSlice slice])
      | _ -> failwith "unexpected spread"
    in
    let target =
      let open Type.Object.Spread in
      let make_seal =
        if frozen then
          Frozen
        else
          Sealed
      in
      Value { make_seal }
    in
    let tool = Type.Object.(Resolve Next) in
    let state =
      {
        Type.Object.Spread.todo_rev;
        acc;
        spread_id = Reason.mk_id ();
        union_reason = None;
        curr_resolve_idx = 0;
      }
    in
    let use_op = Type.(Op (ObjectSpread { op = reason })) in
    Tvar.mk_where file.cx reason (fun tout ->
        Flow_js.flow
          file.cx
          (t, Type.(ObjKitT (use_op, reason, tool, Object.Spread (target, state), tout))))
  | ArrayLit (loc, t, ts) ->
    let reason = Reason.(mk_reason RArrayLit loc) in
    let t = merge file t in
    (* NB: tail-recursive map in case of very large literals *)
    let ts = Base.List.map ~f:(merge file) ts in
    let t =
      match (t, ts) with
      | (t, []) -> t
      | (t0, t1 :: ts) -> Type.(UnionT (reason, UnionRep.make t0 t1 ts))
    in
    Type.(DefT (reason, trust, ArrT (ArrayAT (t, None))))

and merge_accessor file = function
  | Get (loc, t) ->
    let t = merge file t in
    Type.Get (Some loc, t)
  | Set (loc, t) ->
    let t = merge file t in
    Type.Set (Some loc, t)
  | GetSet (gloc, gt, sloc, st) ->
    let gt = merge file gt in
    let st = merge file st in
    Type.GetSet (Some gloc, gt, Some sloc, st)

and merge_obj_value_prop file = function
  | ObjValueField (id_loc, t, polarity) ->
    let t = merge file t in
    Type.Field (Some id_loc, t, polarity)
  | ObjValueAccess x -> merge_accessor file x
  | ObjValueMethod { id_loc; fn_loc; async = _; generator = _; def } ->
    (* RFunctionType should be Reason.func_reason instead, but this matches the
     * behavior of types-first where function bindings are converted to declared
     * functions, which are given a RFunctionType reason.
     *
     * TODO Fix once T71257430 is closed. *)
    let reason = Reason.(mk_reason RFunctionType fn_loc) in
    let statics = merge_fun_statics file reason SMap.empty in
    let t = merge_fun file reason def statics in
    Type.Method (Some id_loc, t)

and merge_class_prop file = function
  | ObjValueField (id_loc, t, polarity) ->
    let t = merge file t in
    Type.Field (Some id_loc, t, polarity)
  | ObjValueAccess x -> merge_accessor file x
  | ObjValueMethod { id_loc; fn_loc; async = _; generator = _; def } ->
    (* RFunctionType should be Reason.func_reason instead, but this matches the
     * behavior of types-first where function bindings are converted to declared
     * functions, which are given a RFunctionType reason.
     *
     * TODO Fix once T71257430 is closed. *)
    let reason = Reason.(mk_reason RFunctionType fn_loc) in
    let statics = Type.dummy_static reason in
    let t = merge_fun file reason def statics in
    Type.Method (Some id_loc, t)

and merge_obj_annot_prop file = function
  | ObjAnnotField (id_loc, t, polarity) ->
    let t = merge file t in
    Type.Field (Some id_loc, t, polarity)
  | ObjAnnotAccess x -> merge_accessor file x
  | ObjAnnotMethod { id_loc; fn_loc; def } ->
    let reason = Reason.(mk_annot_reason RFunctionType fn_loc) in
    let statics = merge_fun_statics file reason SMap.empty in
    let t = merge_fun file reason def statics in
    Type.Method (Some id_loc, t)

and merge_interface_prop file = function
  | InterfaceField (id_loc, t, polarity) ->
    let t = merge file t in
    Type.Field (id_loc, t, polarity)
  | InterfaceAccess x -> merge_accessor file x
  | InterfaceMethod ms ->
    let merge_method fn_loc def =
      let reason = Reason.(mk_reason RFunctionType fn_loc) in
      let statics = Type.dummy_static reason in
      merge_fun file reason def statics
    in
    let finish = function
      | (t, []) -> t
      | (t0, t1 :: ts) ->
        let reason = TypeUtil.reason_of_t t0 in
        Type.(IntersectionT (reason, InterRep.make t0 t1 ts))
    in
    let rec loop acc id_loc = function
      | [] -> Type.Method (Some id_loc, finish acc)
      | (id_loc, fn_loc, def) :: ms ->
        let acc = Nel.cons (merge_method fn_loc def) acc in
        loop acc id_loc ms
    in
    let ((id_loc, fn_loc, def), ms) = ms in
    let acc = Nel.one (merge_method fn_loc def) in
    loop acc id_loc ms

and merge_dict file (ObjDict { name; polarity; key; value }) =
  let key = merge file key in
  let value = merge file value in
  { Type.dict_name = name; dict_polarity = polarity; key; value }

and merge_tparams file reason t tparams = merge_tparams_helper file reason (Fn.const t) tparams

and merge_tparams_targs file reason t tparams =
  let t tparams =
    let targs =
      List.map
        (fun tp ->
          let { Type.reason; name; polarity; _ } = tp in
          let t = Type.BoundT (reason, name) in
          (name, reason, t, polarity))
        tparams
    in
    t targs
  in
  merge_tparams_helper file reason t tparams

and merge_tparams_helper file reason t = function
  | Mono -> t []
  | Poly (tparams_loc, tp, tps) ->
    let poly_reason = Reason.(update_desc_reason (fun d -> RPolyType d) reason) in
    let tparams = Nel.map (merge_tparam file) (tp, tps) in
    let t_out = t (Nel.to_list tparams) in
    let id = Context.make_source_poly_id file.cx tparams_loc in
    Type.(DefT (poly_reason, trust, PolyT { tparams_loc; tparams; t_out; id }))

and merge_tparam file tp =
  let (TParam { name_loc; name; polarity; bound; default }) = tp in
  let reason = Reason.(mk_reason (RType (OrdinaryName name)) name_loc) in
  let bound =
    match bound with
    | None -> Type.(DefT (reason, trust, MixedT Mixed_everything))
    | Some t -> merge file t
  in
  let default =
    match default with
    | None -> None
    | Some t -> Some (merge file t)
  in
  { Type.reason; name; polarity; bound; default; is_this = false }

and merge_op file op = map_op (merge file) op

and merge_interface ~inline file reason id def =
  let (InterfaceSig { extends; props; calls }) = def in
  let super =
    let super_reason = Reason.(update_desc_reason (fun d -> RSuperOf d) reason) in
    let ts = List.map (merge file) extends in
    let ts =
      if calls = [] then
        ts
      else
        Type.FunProtoT super_reason :: ts
    in
    match ts with
    | [] -> Type.ObjProtoT super_reason
    | [t] -> t
    | t0 :: t1 :: ts -> Type.(IntersectionT (super_reason, InterRep.make t0 t1 ts))
  in
  let static =
    let static_reason = Reason.(update_desc_reason (fun d -> RStatics d) reason) in
    (* TODO: interfaces don't have a name field, or even statics *)
    let props = add_name_field reason SMap.empty |> NameUtils.namemap_of_smap in
    let proto = Type.NullProtoT static_reason in
    Obj_type.mk_with_proto file.cx static_reason proto ~props ~obj_kind:Type.Inexact
  in
  let (own_props, proto_props) =
    let open Reason in
    SMap.fold
      (fun k prop (own, proto) ->
        let t = merge_interface_prop file prop in
        match prop with
        | InterfaceField _ -> (NameUtils.Map.add (OrdinaryName k) t own, proto)
        | InterfaceAccess _
        | InterfaceMethod _ ->
          (own, NameUtils.Map.add (OrdinaryName k) t proto))
      props
      (NameUtils.Map.empty, NameUtils.Map.empty)
  in
  let inst_call_t =
    let ts = List.rev_map (merge file) calls in
    match ts with
    | [] -> None
    | [t] -> Some (Context.make_call_prop file.cx t)
    | t0 :: t1 :: ts ->
      let reason = TypeUtil.reason_of_t t0 in
      let t = Type.(IntersectionT (reason, InterRep.make t0 t1 ts)) in
      Some (Context.make_call_prop file.cx t)
  in
  fun targs ->
    let open Type in
    let insttype =
      {
        class_id = id;
        type_args = targs;
        own_props = Context.generate_property_map file.cx own_props;
        proto_props = Context.generate_property_map file.cx proto_props;
        inst_call_t;
        initialized_fields = SSet.empty;
        initialized_static_fields = SSet.empty;
        has_unknown_react_mixins = false;
        inst_kind = InterfaceKind { inline };
      }
    in
    DefT (reason, trust, InstanceT (static, super, [], insttype))

and merge_class_extends file this reason extends mixins =
  let super_reason = Reason.(update_desc_reason (fun d -> RSuperOf d) reason) in
  let (super, static_proto) =
    match extends with
    | ObjectPrototypeExtendsNull -> (Type.NullProtoT super_reason, Type.FunProtoT super_reason)
    | ClassImplicitExtends -> (Type.ObjProtoT super_reason, Type.FunProtoT super_reason)
    | ClassExplicitExtends { loc; t } ->
      let t = specialize file (merge file t) in
      let t = TypeUtil.this_typeapp ~annot_loc:loc t this None in
      (t, TypeUtil.class_type t)
    | ClassExplicitExtendsApp { loc; t; targs } ->
      let t = merge file t in
      let targs = List.map (merge file) targs in
      let t = TypeUtil.this_typeapp ~annot_loc:loc t this (Some targs) in
      (t, TypeUtil.class_type t)
  in
  let mixins_rev = List.rev_map (merge_class_mixin file this) mixins in
  let super =
    match List.rev_append mixins_rev [super] with
    | [] -> failwith "impossible"
    | [t] -> t
    | t0 :: t1 :: ts -> Type.(IntersectionT (super_reason, InterRep.make t0 t1 ts))
  in
  (super, static_proto)

and merge_class_mixin =
  let rec loop file = function
    | Pack.Eval (loc, t, (GetProp name as op)) ->
      let (t, names_rev) = loop file t in
      let t = eval file loc t op in
      (t, name :: names_rev)
    | Pack.Ref ref ->
      let f t _ name = (t, [name]) in
      merge_ref file f ref
    | _ -> failwith "unexpected class mixin"
  in
  let merge_mixin_ref file loc ref =
    let (t, names_rev) = loop file ref in
    let name = String.concat "." (List.rev names_rev) in
    let reason = Reason.(mk_annot_reason (RType (OrdinaryName name)) loc) in
    Tvar.mk_derivable_where file.cx reason (fun tout ->
        Flow_js.flow file.cx (t, Type.MixinT (reason, tout)))
  in
  fun file this -> function
    | ClassMixin { loc; t } ->
      let t = specialize file (merge_mixin_ref file loc t) in
      TypeUtil.this_typeapp ~annot_loc:loc t this None
    | ClassMixinApp { loc; t; targs } ->
      let t = merge_mixin_ref file loc t in
      let targs = List.map (merge file) targs in
      TypeUtil.this_typeapp ~annot_loc:loc t this (Some targs)

and merge_class file reason id def =
  let (ClassSig { tparams; extends; implements; static_props; own_props; proto_props }) = def in
  let t targs =
    let this =
      let reason = Reason.(replace_desc_reason RThisType reason) in
      Type.BoundT (reason, "this")
    in
    let (super, static_proto) = merge_class_extends file this reason extends [] in
    let implements = List.map (merge file) implements in
    let static =
      let static_reason = Reason.(update_desc_reason (fun d -> RStatics d) reason) in
      let props = SMap.map (merge_class_prop file) static_props in
      let props = add_name_field reason props in
      let props = NameUtils.namemap_of_smap props in
      Obj_type.mk_with_proto file.cx static_reason static_proto ~props ~obj_kind:Type.Inexact
    in
    let own_props =
      SMap.map (merge_class_prop file) own_props
      |> NameUtils.namemap_of_smap
      |> Context.generate_property_map file.cx
    in
    let proto_props =
      SMap.map (merge_class_prop file) proto_props
      |> add_default_constructor reason extends
      |> NameUtils.namemap_of_smap
      |> Context.generate_property_map file.cx
    in
    let open Type in
    let insttype =
      {
        class_id = id;
        type_args = targs;
        own_props;
        proto_props;
        inst_call_t = None;
        initialized_fields = SSet.empty;
        initialized_static_fields = SSet.empty;
        has_unknown_react_mixins = false;
        inst_kind = ClassKind;
      }
    in
    let inst = DefT (reason, trust, InstanceT (static, super, implements, insttype)) in
    TypeUtil.this_class_type inst false
  in
  merge_tparams_targs file reason t tparams

and merge_fun_statics file reason statics =
  let props =
    SMap.map
      (fun (id_loc, t) ->
        let t = merge file t in
        Type.Field (Some id_loc, t, Polarity.Neutral))
      statics
    |> NameUtils.namemap_of_smap
  in
  let reason = Reason.(update_desc_reason (fun d -> RStatics d) reason) in
  Obj_type.mk_with_proto
    file.cx
    reason
    (Type.FunProtoT reason)
    ~obj_kind:Type.Inexact
    ~props
    ?call:None

and merge_predicate file base_t loc p =
  let singleton key pos =
    let key = (Reason.OrdinaryName key, []) in
    (Key_map.singleton key pos, Key_map.singleton key (Type.NotP pos))
  in
  let pred_and = Key_map.union ~combine:(fun _ p1 p2 -> Some (Type.AndP (p1, p2))) in
  let pred_or = Key_map.union ~combine:(fun _ p1 p2 -> Some (Type.OrP (p1, p2))) in
  let rec pred = function
    | AndP (p1, p2) ->
      let (pos1, neg1) = pred p1 in
      let (pos2, neg2) = pred p2 in
      (pred_and pos1 pos2, pred_or neg1 neg2)
    | OrP (p1, p2) ->
      let (pos1, neg1) = pred p1 in
      let (pos2, neg2) = pred p2 in
      (pred_or pos1 pos2, pred_and neg1 neg2)
    | NotP p ->
      let (pos, neg) = pred p in
      (neg, pos)
    | ExistsP (key, loc) -> singleton key (Type.ExistsP loc)
    | InstanceofP (key, t) ->
      let t = merge file t in
      singleton key Type.(LeftP (InstanceofTest, t))
    | ArrP key -> singleton key Type.ArrP
    | NullP key -> singleton key Type.NullP
    | MaybeP key -> singleton key Type.MaybeP
    | SingletonStrP (key, loc, sense, x) -> singleton key (Type.SingletonStrP (loc, sense, x))
    | SingletonNumP (key, loc, sense, x, raw) ->
      singleton key (Type.SingletonNumP (loc, sense, (x, raw)))
    | SingletonBoolP (key, loc, x) -> singleton key (Type.SingletonBoolP (loc, x))
    | BoolP (key, loc) -> singleton key (Type.BoolP loc)
    | FunP key -> singleton key Type.FunP
    | NumP (key, loc) -> singleton key (Type.NumP loc)
    | ObjP key -> singleton key Type.ObjP
    | StrP (key, loc) -> singleton key (Type.StrP loc)
    | SymbolP (key, loc) -> singleton key (Type.SymbolP loc)
    | VoidP key -> singleton key Type.VoidP
    | SentinelStrP (key, prop, loc, x) ->
      let reason = Reason.(mk_reason RString loc) in
      let t = Type.(DefT (reason, trust, StrT (Literal (None, Reason.OrdinaryName x)))) in
      singleton key Type.(LeftP (SentinelProp prop, t))
    | SentinelNumP (key, prop, loc, x, raw) ->
      let reason = Reason.(mk_reason RNumber loc) in
      let t = Type.(DefT (reason, trust, NumT (Literal (None, (x, raw))))) in
      singleton key Type.(LeftP (SentinelProp prop, t))
    | SentinelBoolP (key, prop, loc, x) ->
      let reason = Reason.(mk_reason RBoolean loc) in
      let t = Type.(DefT (reason, trust, BoolT (Some x))) in
      singleton key Type.(LeftP (SentinelProp prop, t))
    | SentinelNullP (key, prop, loc) ->
      let t = Type.NullT.at loc trust in
      singleton key Type.(LeftP (SentinelProp prop, t))
    | SentinelVoidP (key, prop, loc) ->
      let t = Type.VoidT.at loc trust in
      singleton key Type.(LeftP (SentinelProp prop, t))
    | SentinelExprP (key, prop, t) ->
      let t = merge file t in
      singleton key Type.(LeftP (SentinelProp prop, t))
    | LatentP (t, keys) ->
      let t = merge file t in
      Nel.fold_left
        (fun (pos1, neg1) (key, i) ->
          let (pos2, neg2) = singleton key (Type.LatentP (t, i + 1)) in
          (pred_and pos1 pos2, pred_or neg1 neg2))
        (Key_map.empty, Key_map.empty)
        keys
  in
  let reason = Reason.(mk_reason (RPredicateOf (RCustom "return")) loc) in
  let (m_pos, m_neg) =
    match p with
    | None -> (Key_map.empty, Key_map.empty)
    | Some p -> pred p
  in
  Type.OpenPredT { reason; base_t; m_pos; m_neg }

and merge_fun
    file reason (FunSig { tparams; params; rest_param; this_param; return; predicate }) statics =
  let prototype =
    let reason = Reason.(update_desc_reason (Fn.const RPrototype) reason) in
    Type.Unsoundness.function_proto_any reason
  in
  let params =
    List.map
      (fun param ->
        let (FunParam { name; t }) = param in
        let t = merge file t in
        (name, t))
      params
  in
  let rest_param =
    match rest_param with
    | None -> None
    | Some (FunRestParam { name; loc; t }) ->
      let t = merge file t in
      Some (name, loc, t)
  in
  let this_t =
    match this_param with
    | None -> Type.bound_function_dummy_this
    | Some t -> merge file t
  in
  let return = merge file return in
  let return =
    match predicate with
    | None -> return
    | Some (loc, p) -> merge_predicate file return loc p
  in
  let t =
    let open Type in
    let funtype =
      {
        this_t;
        params;
        rest_param;
        return_t = return;
        is_predicate = predicate <> None;
        def_reason = reason;
      }
    in
    DefT (reason, trust, FunT (statics, prototype, funtype))
  in
  merge_tparams file reason t tparams

let merge_type_alias file reason name tparams body =
  let t = merge file body in
  let t =
    let open Reason in
    let open TypeUtil in
    let t_loc = loc_of_t t in
    mod_reason_of_t (update_desc_reason (fun desc -> RTypeAlias (name, Some t_loc, desc))) t
  in
  let t = Type.(DefT (reason, trust, TypeT (TypeAliasKind, t))) in
  merge_tparams file reason t tparams

let merge_opaque_type file reason id name tparams bound body =
  let opaque_reason = Reason.(replace_desc_reason (ROpaqueType name) reason) in
  let bound = Option.map ~f:(merge file) bound in
  let body = Option.map ~f:(merge file) body in
  let t targs =
    let open Type in
    let opaquetype =
      {
        underlying_t = body;
        super_t = bound;
        opaque_id = id;
        opaque_type_args = targs;
        opaque_name = name;
      }
    in
    DefT (reason, trust, TypeT (OpaqueKind, OpaqueT (opaque_reason, opaquetype)))
  in
  merge_tparams_targs file reason t tparams

let merge_declare_class file reason id def =
  let (DeclareClassSig
        {
          tparams;
          extends;
          mixins;
          implements;
          static_props;
          own_props;
          proto_props;
          static_calls;
          calls;
        }) =
    def
  in
  let t targs =
    let this =
      let reason = Reason.(replace_desc_reason RThisType reason) in
      Type.BoundT (reason, "this")
    in
    let (super, static_proto) = merge_class_extends file this reason extends mixins in
    let implements = List.map (merge file) implements in
    let static =
      let static_reason = Reason.(update_desc_reason (fun d -> RStatics d) reason) in
      let props = SMap.map (merge_interface_prop file) static_props in
      let props = add_name_field reason props in
      let props = NameUtils.namemap_of_smap props in
      let call =
        match List.rev_map (merge file) static_calls with
        | [] -> None
        | [t] -> Some t
        | t0 :: t1 :: ts ->
          let reason = TypeUtil.reason_of_t t0 in
          let t = Type.(IntersectionT (reason, InterRep.make t0 t1 ts)) in
          Some t
      in
      Obj_type.mk_with_proto file.cx static_reason static_proto ~props ?call ~obj_kind:Type.Inexact
    in
    let own_props =
      SMap.map (merge_interface_prop file) own_props
      |> NameUtils.namemap_of_smap
      |> Context.generate_property_map file.cx
    in
    let proto_props =
      SMap.map (merge_interface_prop file) proto_props
      |> add_default_constructor reason extends
      |> NameUtils.namemap_of_smap
      |> Context.generate_property_map file.cx
    in
    let inst_call_t =
      match List.rev_map (merge file) calls with
      | [] -> None
      | [t] -> Some (Context.make_call_prop file.cx t)
      | t0 :: t1 :: ts ->
        let reason = TypeUtil.reason_of_t t0 in
        let t = Type.(IntersectionT (reason, InterRep.make t0 t1 ts)) in
        Some (Context.make_call_prop file.cx t)
    in
    let open Type in
    let insttype =
      {
        class_id = id;
        type_args = targs;
        own_props;
        proto_props;
        inst_call_t;
        initialized_fields = SSet.empty;
        initialized_static_fields = SSet.empty;
        has_unknown_react_mixins = false;
        inst_kind = ClassKind;
      }
    in
    let inst = DefT (reason, trust, InstanceT (static, super, implements, insttype)) in
    TypeUtil.this_class_type inst false
  in
  merge_tparams_targs file reason t tparams

let merge_declare_fun file defs =
  let ts =
    Nel.map
      (fun (_, fn_loc, def) ->
        let reason = Reason.(mk_reason RFunctionType fn_loc) in
        let statics = merge_fun_statics file reason SMap.empty in
        merge_fun file reason def statics)
      defs
  in
  match ts with
  | (t, []) -> t
  | (t0, t1 :: ts) ->
    let reason = TypeUtil.reason_of_t t0 |> Reason.(replace_desc_reason RIntersectionType) in
    Type.(IntersectionT (reason, InterRep.make t0 t1 ts))

let merge_def file reason = function
  | TypeAlias { id_loc = _; name; tparams; body } -> merge_type_alias file reason name tparams body
  | OpaqueType { id_loc; name; tparams; body; bound } ->
    let id = Context.make_aloc_id file.cx id_loc in
    merge_opaque_type file reason id name tparams bound body
  | Interface { id_loc; name = _; tparams; def } ->
    let id = Context.make_aloc_id file.cx id_loc in
    let t targs =
      let t = merge_interface ~inline:false file reason id def targs in
      TypeUtil.class_type t
    in
    merge_tparams_targs file reason t tparams
  | ClassBinding { id_loc; name = _; def } ->
    let id = Context.make_aloc_id file.cx id_loc in
    merge_class file reason id def
  | DeclareClassBinding { id_loc; name = _; def } ->
    let id = Context.make_aloc_id file.cx id_loc in
    merge_declare_class file reason id def
  | FunBinding { id_loc = _; name = _; async = _; generator = _; fn_loc = _; def; statics } ->
    let statics = merge_fun_statics file reason statics in
    merge_fun file reason def statics
  | DeclareFun { id_loc; fn_loc; name = _; def; tail } ->
    merge_declare_fun file ((id_loc, fn_loc, def), tail)
  | Variable { id_loc = _; name = _; def } -> merge file def
  | DisabledEnumBinding _ -> Type.AnyT.error reason
  | EnumBinding { id_loc; name; rep; members; has_unknown_members } ->
    merge_enum file reason id_loc name rep members has_unknown_members

let merge_file file =
  let module_t = visit file.exports in
  let module_ref = Files.module_ref file.key in
  Context.add_module file.cx (Reason.OrdinaryName module_ref) module_t
