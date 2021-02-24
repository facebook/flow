(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type export =
  (* TODO: DefaultType, e.g. `export default class Foo {}` *)
  | Default  (** e.g. `export default function() {}` *)
  | Named of string  (** `export const foo: string = "foo"` *)
  | NamedType of string  (** `export type T = string` *)
  | Module of string * export list  (** `declare module "foo" { ... exports ... }` *)
[@@deriving show { with_path = false }]

type t = export list [@@deriving show { with_path = false }]

module Export_sig = struct
  type 'loc t = {
    exports: 'loc Type_sig_pack.exports option;
    export_def: 'loc Type_sig_pack.packed option;
    module_refs: string Type_sig_collections.Module_refs.t;
    local_defs: 'loc Type_sig_pack.packed_def Type_sig_collections.Local_defs.t;
    remote_refs: 'loc Type_sig_pack.remote_ref Type_sig_collections.Remote_refs.t;
    pattern_defs: 'loc Type_sig_pack.packed Type_sig_collections.Pattern_defs.t option;
    patterns: 'loc Type_sig_pack.pattern Type_sig_collections.Patterns.t option;
  }

  let of_module
      {
        Packed_type_sig.Module.exports;
        export_def;
        module_refs;
        local_defs;
        remote_refs;
        pattern_defs;
        patterns;
      } =
    {
      exports = Some exports;
      export_def;
      module_refs;
      local_defs;
      remote_refs;
      pattern_defs = Some pattern_defs;
      patterns = Some patterns;
    }

  let of_builtins ~module_refs ~local_defs ~remote_refs =
    {
      exports = None;
      export_def = None;
      module_refs;
      local_defs;
      remote_refs;
      pattern_defs = None;
      patterns = None;
    }

  let of_builtin_module ~module_refs ~local_defs ~remote_refs ~exports ~export_def =
    {
      exports = Some exports;
      export_def;
      module_refs;
      local_defs;
      remote_refs;
      pattern_defs = None;
      patterns = None;
    }
end

let local_def_of_index type_sig index =
  Type_sig_collections.Local_defs.get type_sig.Export_sig.local_defs index

let pattern_of_index type_sig index =
  match type_sig.Export_sig.patterns with
  | Some patterns -> Type_sig_collections.Patterns.get patterns index
  | None -> failwith "unexpected pattern in builtin module"

let pattern_def_of_index type_sig index =
  match type_sig.Export_sig.pattern_defs with
  | Some pattern_defs -> Type_sig_collections.Pattern_defs.get pattern_defs index
  | None -> failwith "unexpected pattern_def in builtin module"

module Eval = struct
  open Type_sig
  open Type_sig_pack

  type 'loc evaled =
    | Annot of 'loc packed_annot
    | Value of 'loc packed_value
    | ClassDecl
    | EnumDecl
    | Nothing

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

  and tyref type_sig seen (r : 'a tyref) : 'a evaled =
    match r with
    | Qualified { name = _name; qualification; _ } ->
      (match tyref type_sig seen qualification with
      | Annot _ ->
        (* TODO: get `_qual._name` *)
        Nothing
      | Value _ ->
        (* TODO: get `_qual._name` *)
        Nothing
      | ClassDecl -> ClassDecl
      | EnumDecl -> EnumDecl
      | Nothing -> Nothing)
    | Unqualified r -> ref type_sig seen r

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
    | Variable { def; _ } -> packed type_sig seen def
    | TypeAlias { body; _ } -> packed type_sig seen body
    | ClassBinding _ -> ClassDecl
    | DeclareClassBinding _ -> ClassDecl
    | EnumBinding _ -> EnumDecl
    | DisabledEnumBinding _ -> EnumDecl
    | Interface _
    | FunBinding _
    | DeclareFun _
    | OpaqueType _ ->
      (* None of these contain anything that can be imported separately. For example,
         you can't `import {someMethod} ...` from an exported class. *)
      Nothing

  and packed type_sig seen : 'loc packed -> 'loc evaled = function
    | Value x -> Value x
    | Annot x -> Annot x
    | Ref r -> ref type_sig seen r
    | TyRef r -> tyref type_sig seen r
    | TyRefApp { name; _ } -> tyref type_sig seen name
    | Eval (_, x, op) -> eval type_sig seen x op
    | Pattern index -> pattern type_sig seen (pattern_of_index type_sig index)
    | Require _ ->
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
    | Value (ObjLit { props; _ }) ->
      (match field_of_obj_props name props with
      | Some p -> packed type_sig seen p
      | None -> Nothing)
    | Value (ObjSpreadLit _) -> (* TODO *) Nothing
    | Annot _ ->
      (* TODO? *)
      Nothing
    | Value
        ( ClassExpr _ | FunExpr _ | StringVal _ | StringLit _ | LongStringLit _ | NumberVal _
        | NumberLit _ | BooleanVal _ | BooleanLit _ | NullLit _ | ArrayLit _ ) ->
      Nothing
    | ClassDecl -> Nothing
    | EnumDecl -> Nothing
    | Nothing -> Nothing
end

(** [add_named_type acc name def] adds [NamedType name] to [acc] if [def] is a
    class or enum, since its type is also exported because classes and enums are both
    values and types. *)
let add_named_type acc name = function
  | Eval.ClassDecl
  | Eval.EnumDecl ->
    NamedType name :: acc
  | Eval.Value _
  | Eval.Annot _
  | Eval.Nothing ->
    acc

let empty_seen = Type_sig_collections.Local_defs.IndexSet.empty

module ESM = struct
  open Type_sig_pack

  let fold_name type_sig name value acc =
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
    | ExportDefault _
    | ExportDefaultBinding _ ->
      Default :: acc
    | ExportFrom _ ->
      (* TODO: ExportFrom defines aliases, which we don't handle yet. TS
         keeps track of them and only suggests them if the re-exported thing
         can't be imported. *)
      acc

  let fold_type name value acc =
    match value with
    | ExportTypeRef _
    | ExportTypeBinding _ ->
      NamedType name :: acc
    | ExportTypeFrom _ ->
      (* TODO: ExportTypeFrom defines aliases, which we don't handle yet. TS
         keeps track of them and only suggests them if the re-exported thing
         can't be imported. *)
      acc

  let exports type_sig names types stars type_stars =
    (* TODO: re-exports *)
    ignore stars;
    ignore type_stars;
    [] |> SMap.fold (fold_name type_sig) names |> SMap.fold fold_type types
end

module CJS = struct
  open Type_sig
  open Type_sig_pack

  (** only objects can be destructured on import *)
  let exports_of_value acc type_sig = function
    | ObjLit { props; _ } ->
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
    | StringVal _ ->
      acc

  let exports_of_annot acc = function
    | ObjAnnot { props; _ } -> SMap.fold (fun name _value acc -> Named name :: acc) props acc
    | _ ->
      (* TODO: handle TEMPORARY_Object, ReadOnly, Exact, if they wrap objects? *)
      acc

  let add_named_exports acc type_sig packed =
    match Eval.packed type_sig empty_seen packed with
    | Eval.Annot annot -> exports_of_annot acc annot
    | Eval.Value value -> exports_of_value acc type_sig value
    | Eval.ClassDecl
    | Eval.EnumDecl
    | Eval.Nothing ->
      acc

  let add_default_exports type_sig acc =
    match type_sig.Export_sig.export_def with
    | Some module_exports -> Default :: add_named_exports acc type_sig module_exports
    | None -> acc

  let add_type_exports types acc =
    SMap.fold
      (fun name value acc ->
        let open Type_sig_pack in
        match value with
        | ExportTypeRef _
        | ExportTypeBinding _ ->
          NamedType name :: acc
        | ExportTypeFrom _ ->
          (* TODO: ExportTypeFrom defines aliases, which we don't handle yet. TS
             keeps track of them and only suggests them if the re-exported thing
             can't be imported. *)
          acc)
      types
      acc

  let exports type_sig types type_stars =
    (* TODO: re-exports *)
    ignore type_stars;
    [] |> add_default_exports type_sig |> add_type_exports types
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
    | Type_sig.DeclareFun _ ->
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
  match export_sig.Export_sig.exports with
  | Some (Type_sig_pack.ESExports { names; types; stars; type_stars; strict = _ }) ->
    ESM.exports export_sig names types stars type_stars
  | Some (Type_sig_pack.CJSExports { types; type_stars; strict = _ }) ->
    CJS.exports export_sig types type_stars
  | None -> failwith "unexpected exports in global scope"

let of_module type_sig : t = type_sig |> Export_sig.of_module |> of_sig

let of_builtins { Packed_type_sig.Builtins.modules; module_refs; local_defs; remote_refs; globals }
    =
  let global_sig = Export_sig.of_builtins ~module_refs ~local_defs ~remote_refs in
  []
  |> SMap.fold (add_global global_sig) globals
  |> SMap.fold
       (fun name { Packed_type_sig.Builtins.loc = _; exports; export_def } acc ->
         let export_sig =
           Export_sig.of_builtin_module ~module_refs ~local_defs ~remote_refs ~exports ~export_def
         in
         Module (name, of_sig export_sig) :: acc)
       modules

let empty = []
