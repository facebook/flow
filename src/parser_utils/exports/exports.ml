(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type export =
  | DefaultType of string option (* e.g. `export default class Foo {}` *)
  | Default of string option  (** e.g. `export default function() {}` *)
  | Named of string  (** `export const foo: string = "foo"` *)
  | NamedType of string  (** `export type T = string` *)
  | Module of string * export list  (** `declare module "foo" { ... exports ... }` *)
  | ReExportModule of string
  | ReExportModuleTypes of string
[@@deriving show { with_path = false }]

type t = export list [@@deriving show { with_path = false }]

module Export_sig = struct
  type 'loc t = {
    module_kind: 'loc Type_sig_pack.module_kind option;
    module_refs: string Type_sig_collections.Module_refs.t;
    local_defs: 'loc Type_sig_pack.packed_def Type_sig_collections.Local_defs.t;
    remote_refs: 'loc Type_sig_pack.remote_ref Type_sig_collections.Remote_refs.t;
    pattern_defs: 'loc Type_sig_pack.packed Type_sig_collections.Pattern_defs.t;
    patterns: 'loc Type_sig_pack.pattern Type_sig_collections.Patterns.t;
  }
  [@@warning "-69"]

  let of_module
      {
        Packed_type_sig.Module.module_kind;
        module_refs;
        local_defs;
        dirty_local_defs = _;
        remote_refs;
        pattern_defs;
        dirty_pattern_defs = _;
        patterns;
      } =
    { module_kind = Some module_kind; module_refs; local_defs; remote_refs; pattern_defs; patterns }

  let of_builtins ~module_refs ~local_defs ~remote_refs ~pattern_defs ~patterns =
    { module_kind = None; module_refs; local_defs; remote_refs; pattern_defs; patterns }

  let of_builtin_module ~module_refs ~local_defs ~remote_refs ~module_kind ~pattern_defs ~patterns =
    { module_kind = Some module_kind; module_refs; local_defs; remote_refs; pattern_defs; patterns }
end

let local_def_of_index type_sig index =
  Type_sig_collections.Local_defs.get type_sig.Export_sig.local_defs index

let pattern_of_index type_sig index =
  Type_sig_collections.Patterns.get type_sig.Export_sig.patterns index

let pattern_def_of_index type_sig index =
  Type_sig_collections.Pattern_defs.get type_sig.Export_sig.pattern_defs index

let module_ref_of_index type_sig index =
  Type_sig_collections.Module_refs.get type_sig.Export_sig.module_refs index

module Eval = struct
  open Type_sig
  open Type_sig_pack

  type 'loc evaled =
    | Annot of 'loc packed_annot * string option
    | Value of 'loc packed_value * string option
    | ClassDecl of string
    | EnumDecl of string
    | ComponentDecl of string
    | Nothing

  let opt_name_of_evaled = function
    | ClassDecl name
    | EnumDecl name
    | ComponentDecl name ->
      Some name
    | Annot (_, name_opt)
    | Value (_, name_opt) ->
      name_opt
    | Nothing -> None

  let seen_ref seen = function
    | LocalRef { index; _ } ->
      let dupe = Type_sig_collections.Local_defs.IndexSet.mem index seen in
      let seen = Type_sig_collections.Local_defs.IndexSet.add index seen in
      (dupe, seen)
    | _ -> (false, seen)

  (** Looks up an object field by name. Returns [None] if the name doesn't
      exist or isn't a field. *)
  let field_of_obj_props name props =
    let open Base.Option.Let_syntax in
    match%bind SMap.find_opt name props with
    | ObjValueField (_, field, _) -> Some field
    | ObjValueAccess _
    | ObjValueMethod _ ->
      (* Accessors and methods don't have any sub-properties to contribute *)
      None

  let rec pattern type_sig seen : 'a pattern -> 'a evaled = function
    | PDef index -> packed type_sig seen (pattern_def_of_index type_sig index)
    | PropP { name; def; _ } ->
      let evaled = pattern type_sig seen (pattern_of_index type_sig def) in
      get_field type_sig seen name evaled
    | ComputedP _
    | UnsupportedLiteralP _
    | ObjRestP _
    | IndexP _
    | ArrRestP _ ->
      (* TODO? *)
      Nothing

  and tyref type_sig seen ?name (r : 'a tyref) : 'a evaled =
    match (r, name) with
    (* This case is a heuristic to identify React.AbstractComponent exports, which can be used as types *)
    | ( Qualified { name = "AbstractComponent"; qualification = Unqualified (RemoteRef _); _ },
        Some name
      ) ->
      ComponentDecl name
    | (Qualified { name = _name; qualification; _ }, _) ->
      (match tyref type_sig seen qualification with
      | Annot _ ->
        (* TODO: get `_qual._name` *)
        Nothing
      | Value _ ->
        (* TODO: get `_qual._name` *)
        Nothing
      | ClassDecl n -> ClassDecl n
      | EnumDecl n -> EnumDecl n
      | ComponentDecl n -> ComponentDecl n
      | Nothing -> Nothing)
    | (Unqualified r, _) -> ref type_sig seen r

  and ref type_sig seen (r : 'loc packed_ref) : 'loc evaled =
    let (dupe, seen) = seen_ref seen r in
    if dupe then
      Nothing
    else
      match r with
      | LocalRef { index; _ } -> def type_sig seen (local_def_of_index type_sig index)
      | RemoteRef _
      | BuiltinRef _ ->
        (* TODO: remember these cross-module aliases. if the remote thing matches,
           we can also suggest everything that aliases to it. *)
        Nothing

  (** [def type_sig d] steps through variable definitions to keep walking the
      initializer. all other definitions (like classes or functions) do not contribute
      any exported names, so we don't need to walk them.

      so, for [var x = y], returns [Some y]; for all other definitions, returns [None]. *)
  and def type_sig seen : 'loc packed_def -> 'loc evaled = function
    | Variable { def; name; _ } -> packed ~name type_sig seen def
    | TypeAlias { body; name; _ } -> packed ~name type_sig seen body
    | ClassBinding { name; _ } -> ClassDecl name
    | DeclareClassBinding { name; _ } -> ClassDecl name
    | EnumBinding { name; _ } -> EnumDecl name
    | DisabledEnumBinding { name; _ } -> EnumDecl name
    | ComponentBinding { name; _ } -> ComponentDecl name
    | DisabledComponentBinding { name; _ } -> ComponentDecl name
    | Interface _
    | FunBinding _
    | DeclareFun _
    | NamespaceBinding _
    | OpaqueType _ ->
      (* None of these contain anything that can be imported separately. For example,
         you can't `import {someMethod} ...` from an exported class. *)
      Nothing

  and packed type_sig ?name seen : 'loc packed -> 'loc evaled = function
    | Value x -> Value (x, name)
    | Annot (Typeof { qname = [typeof_name]; _ } as x) ->
      Annot (x, Some (Base.Option.value ~default:typeof_name name))
    | Annot (ReactAbstractComponent _ as x) ->
      (match name with
      | Some name -> ComponentDecl name
      | _ -> Annot (x, name))
    | Annot x -> Annot (x, name)
    | Ref r -> ref type_sig seen r
    | TyRef r -> tyref type_sig seen r
    | TyRefApp { name = tyref_name; _ } -> tyref type_sig seen ?name tyref_name
    | Eval (_, x, op) -> eval type_sig seen x op
    | Pattern index -> pattern type_sig seen (pattern_of_index type_sig index)
    | Require _
    | ImportDynamic _ ->
      (* TODO: remember these cross-module aliases. if the remote thing matches,
         we can also suggest everything that aliases to it. *)
      Nothing
    | Err _ -> Nothing
    | ModuleRef _
    | AsyncVoidReturn _ ->
      (* TODO? *)
      Nothing

  and eval type_sig seen (x : 'loc packed) (op : 'loc packed op) : 'loc evaled =
    match op with
    | GetProp name -> packed type_sig seen x |> get_field type_sig seen name
    | _ ->
      (* TODO? *)
      Nothing

  (** [get_field type_sig name evaled] destructures an object pattern like
      [let { name } = evaled], returning [Some evaled.name] if [evaled] is an
      object AND it has a [name] field; [None] otherwise. *)
  and get_field type_sig seen (name : string) (evaled : 'a evaled) : 'a evaled =
    match evaled with
    | Value ((ObjLit { props; _ } | DeclareModuleImplicitlyExportedObject { props; _ }), _) ->
      (match field_of_obj_props name props with
      | Some p -> packed type_sig seen p
      | None -> Nothing)
    | Value (ObjSpreadLit _, _) -> (* TODO *) Nothing
    | Annot _ ->
      (* TODO? *)
      Nothing
    | Value
        ( ( ClassExpr _ | FunExpr _ | StringVal _ | StringLit _ | LongStringLit _ | NumberVal _
          | NumberLit _ | BooleanVal _ | BooleanLit _ | NullLit _ | EmptyConstArrayLit _
          | ArrayLit _ | BigIntVal _ | BigIntLit _ | AsConst _ ),
          _
        ) ->
      Nothing
    | ClassDecl _ -> Nothing
    | EnumDecl _ -> Nothing
    | ComponentDecl _ -> Nothing
    | Nothing -> Nothing
end

(** [is_typeish def] determines if [def] is a class or enum, whose type is also exported because
    classes and enums are both values and types. *)
let is_typeish = function
  | Eval.ClassDecl _
  | Eval.EnumDecl _
  | Eval.ComponentDecl _ ->
    true
  | Eval.Value _
  | Eval.Annot _
  | Eval.Nothing ->
    false

(** [add_named_type acc name def] adds [NamedType name] to [acc] if [def] is a
    class or enum, since its type is also exported because classes and enums are both
    values and types. *)
let add_named_type acc name def =
  if is_typeish def then
    NamedType name :: acc
  else
    acc

(** [add_default_type acc def] adds [DefaultType] to [acc] if [def] is a class or enum,
    since its type is also exported because classes and enums are both values and types. *)
let add_default_type acc name_opt def =
  if is_typeish def then
    DefaultType name_opt :: acc
  else
    acc

let empty_seen = Type_sig_collections.Local_defs.IndexSet.empty

module ESM = struct
  open Type_sig_pack

  let fold_name type_sig acc name value =
    match value with
    | ExportRef ref ->
      let acc = Eval.ref type_sig empty_seen ref |> add_named_type acc name in
      Named name :: acc
    | ExportBinding index ->
      let acc =
        let def = Eval.def type_sig empty_seen (local_def_of_index type_sig index) in
        add_named_type acc name def
      in
      Named name :: acc
    | ExportDefault { def; _ } ->
      let def = Eval.packed type_sig empty_seen def in
      let opt_name = Eval.opt_name_of_evaled def in
      let acc = add_default_type acc opt_name def in
      Default opt_name :: acc
    | ExportDefaultBinding { index; _ } ->
      let def = Eval.def type_sig empty_seen (local_def_of_index type_sig index) in
      let opt_name = Eval.opt_name_of_evaled def in
      let acc = add_default_type acc opt_name def in
      Default opt_name :: acc
    | ExportFrom _ ->
      (* ExportFrom defines aliases. We decide to depend on autocomplete_ranked_by_usage to pick
       * the right one in most cases. *)
      (* TODO: We cannot resolve to the underlying definition here from another module, so we don't
       * know whether the binding can also be used as a type. *)
      Named name :: acc

  let fold_type acc name value =
    match value with
    | ExportTypeRef _
    | ExportTypeBinding _
    | ExportTypeFrom _ ->
      NamedType name :: acc

  let exports type_sig type_exports exports info =
    let (ESModuleInfo
          {
            type_export_keys;
            export_keys;
            type_stars;
            stars;
            strict = _;
            platform_availability_set = _;
          }
          ) =
      info
    in
    let acc =
      Base.List.fold stars ~init:[] ~f:(fun acc (_, module_index) ->
          ReExportModule (module_ref_of_index type_sig module_index) :: acc
      )
    in
    let acc =
      Base.List.fold type_stars ~init:acc ~f:(fun acc (_, module_index) ->
          ReExportModule (module_ref_of_index type_sig module_index) :: acc
      )
    in
    let acc = Base.Array.fold2_exn ~init:acc ~f:(fold_name type_sig) export_keys exports in
    Base.Array.fold2_exn ~init:acc ~f:fold_type type_export_keys type_exports
end

module CJS = struct
  open Type_sig
  open Type_sig_pack

  (** only objects can be destructured on import *)
  let rec exports_of_value acc type_sig = function
    | ObjLit { props; _ }
    | DeclareModuleImplicitlyExportedObject { props; _ } ->
      SMap.fold
        (fun name value acc ->
          (* only property names that are valid identifier names can currently be
             imported: `module.exports = { "Foo Bar": true }` cannot be imported
             as `import { "Foo Bar" as Foo_bar } ...` yet. This will be allowed by
             https://github.com/tc39/ecma262/pull/2154; until then, we only bother
             indexing names that can actually be imported. *)
          if Parser_flow.string_is_valid_identifier_name name then
            let acc =
              match value with
              | ObjValueField (_, Value (ClassExpr _), _) -> NamedType name :: acc
              | ObjValueField (_, Ref ref, _) ->
                Eval.ref type_sig empty_seen ref |> add_named_type acc name
              | _ -> acc
            in
            Named name :: acc
          else
            acc)
        props
        acc
    | AsConst v -> exports_of_value acc type_sig v
    | EmptyConstArrayLit _
    | ArrayLit _
    | BooleanLit _
    | BooleanVal _
    | ClassExpr _
    | FunExpr _
    | LongStringLit _
    | NullLit _
    | NumberLit _
    | NumberVal _
    | ObjSpreadLit _
    | StringLit _
    | StringVal _
    | BigIntVal _
    | BigIntLit _ ->
      acc

  let exports_of_annot acc = function
    | ObjAnnot { props; _ } -> SMap.fold (fun name _value acc -> Named name :: acc) props acc
    | _ ->
      (* TODO: handle TEMPORARY_Object, ReadOnly, Exact, if they wrap objects? *)
      acc

  let add_named_exports acc type_sig packed =
    match Eval.packed type_sig empty_seen packed with
    | Eval.Annot (annot, _) -> exports_of_annot acc annot
    | Eval.Value (value, _) -> exports_of_value acc type_sig value
    | Eval.ClassDecl _
    | Eval.EnumDecl _
    | Eval.ComponentDecl _
    | Eval.Nothing ->
      acc

  let add_default_exports type_sig acc = function
    | Some module_exports -> Default None :: add_named_exports acc type_sig module_exports
    | None -> acc

  let fold_type acc name value =
    let open Type_sig_pack in
    match value with
    | ExportTypeRef _
    | ExportTypeBinding _
    | ExportTypeFrom _ ->
      NamedType name :: acc

  let exports type_sig type_exports exports info =
    let (CJSModuleInfo { type_export_keys; type_stars; strict = _; platform_availability_set = _ })
        =
      info
    in
    let acc =
      Base.List.fold type_stars ~init:[] ~f:(fun acc (_, module_index) ->
          ReExportModule (module_ref_of_index type_sig module_index) :: acc
      )
    in
    let acc = add_default_exports type_sig acc exports in
    Base.Array.fold2_exn ~init:acc ~f:fold_type type_export_keys type_exports
end

let add_global =
  let add_named name acc =
    if String.contains name '$' then
      acc
    else
      Named name :: acc
  in
  fun global_sig name index acc ->
    let def = local_def_of_index global_sig index in
    match def with
    | Type_sig.Variable _ ->
      add_named_type acc name (Eval.def global_sig empty_seen def) |> add_named name
    | Type_sig.FunBinding _
    | Type_sig.DeclareFun _
    | Type_sig.ComponentBinding _
    | Type_sig.DisabledComponentBinding _
    | Type_sig.NamespaceBinding _ ->
      add_named name acc
    | Type_sig.TypeAlias _
    | Type_sig.Interface _
    | Type_sig.OpaqueType _ ->
      NamedType name :: acc
    | Type_sig.ClassBinding _
    | Type_sig.DeclareClassBinding _
    | Type_sig.EnumBinding _
    | Type_sig.DisabledEnumBinding _ ->
      add_named name (NamedType name :: acc)

let of_sig export_sig : t =
  match export_sig.Export_sig.module_kind with
  | Some (Type_sig_pack.ESModule { type_exports; exports; info }) ->
    ESM.exports export_sig type_exports exports info
  | Some (Type_sig_pack.CJSModule { type_exports; exports; info }) ->
    CJS.exports export_sig type_exports exports info
  | None -> failwith "unexpected exports in global scope"

let of_module type_sig : t = type_sig |> Export_sig.of_module |> of_sig

let of_builtins
    {
      Packed_type_sig.Builtins.global_modules;
      module_refs;
      local_defs;
      remote_refs;
      pattern_defs;
      patterns;
      global_values;
      global_types;
    } =
  let global_sig =
    Export_sig.of_builtins ~module_refs ~local_defs ~remote_refs ~pattern_defs ~patterns
  in
  []
  |> SMap.fold (add_global global_sig) global_values
  |> SMap.fold (add_global global_sig) global_types
  |> SMap.fold
       (fun name { Packed_type_sig.Builtins.loc = _; module_kind } acc ->
         let export_sig =
           Export_sig.of_builtin_module
             ~module_refs
             ~local_defs
             ~remote_refs
             ~module_kind
             ~pattern_defs
             ~patterns
         in
         Module (name, of_sig export_sig) :: acc)
       global_modules

let empty = []
