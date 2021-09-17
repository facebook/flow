(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty
open Utils_js

let cut_off ?(limit = 1000) str =
  let len = String.length str in
  if len > limit then
    String.sub str 0 (len - 1) ^ " ..."
  else
    str

let dump_bot_upper_bound_kind = function
  | NoUpper -> "NoUpper"
  | SomeKnownUpper _ -> "SomeKnownUpper"
  | SomeUnknownUpper u -> spf "SomeUnknownUpper (%s)" u

let dump_bot_kind = function
  | EmptyType -> "EmptyType"
  | EmptyMatchingPropT -> "EmptyMatchingPropT"
  | EmptyTypeDestructorTriggerT _ -> "EmptyTypeDestructorTriggerT"
  | NoLowerWithUpper u -> spf "NoLowerWithUpper (%s)" (dump_bot_upper_bound_kind u)

let dump_import_mode = function
  | ValueMode -> "value"
  | TypeMode -> "type"
  | TypeofMode -> "typeof"

let dump_import_ident (_, id, mode) = spf "`%s` (%s)" id (dump_import_mode mode)

let ctor_of_provenance = function
  | Local -> "Local"
  | Remote { imported_as = Some ii } -> spf "Imported as %s" (dump_import_ident ii)
  | Remote { imported_as = None } -> "Remote"
  | Library { imported_as = Some _ } -> "Library (Imported)"
  | Library { imported_as = None } -> "Library (Remote)"
  | Builtin -> "Builtin"

let dump_symbol { sym_provenance; sym_def_loc; sym_name; _ } =
  Utils_js.spf
    "%s (%s:%s)"
    (Reason.display_string_of_name sym_name)
    (ctor_of_provenance sym_provenance)
    (Reason.string_of_aloc sym_def_loc)

let builtin_value = function
  | FunProto -> "Function.prototype"
  | ObjProto -> "Object.prototype"
  | FunProtoApply -> "Function.prototype.apply"
  | FunProtoBind -> "Function.prototype.bind"
  | FunProtoCall -> "Function.prototype.call"
  | TSymbol s -> dump_symbol s

let rec dump_opt (f : 'a -> string) (o : 'a option) =
  match o with
  | Some t -> f t
  | None -> ""

and dump_any_kind = function
  | Annotated _ -> "Annotated"
  | AnyError kind -> spf "AnyError (%s)" (dump_any_error_kind kind)
  | Unsound kind -> spf "Unsound (%s)" (dump_any_unsoundness_kind kind)
  | Untyped -> "Untyped"

and dump_any_error_kind = function
  | Some UnresolvedName -> "UnresolvedName"
  | None -> "<None>"

and dump_any_unsoundness_kind = function
  | BoundFunctionThis -> "BoundFunctionThis"
  | ComputedNonLiteralKey -> "ComputedNonLiteralKey"
  | Constructor -> "Constructor"
  | DummyStatic -> "DummyStatic"
  | Existential -> "Existential"
  | Exports -> "Exports"
  | FunctionPrototype -> "FunctionPrototype"
  | InferenceHooks -> "InferenceHooks"
  | InstanceOfRefinement -> "InstanceOfRefinement"
  | Merged -> "Merged"
  | ResolveSpread -> "ResolveSpread"
  | Unchecked -> "Unchecked"
  | Unimplemented -> "Unimplemented"
  | UnresolvedType -> "UnresolvedType"
  | WeakContext -> "WeakContext"

and dump_list : 'a. ('a -> string) -> ?sep:string -> 'a list -> string =
 (fun f ?(sep = ", ") ls -> Base.List.map ~f ls |> String.concat sep)

and dump_param_opt = function
  | { prm_optional = true } -> "?"
  | _ -> ""

and dump_param ~depth (p : string option * t * fun_param) =
  match p with
  | (Some s, t, o) -> spf "%s%s: %s" s (dump_param_opt o) (dump_t ~depth t)
  | (_, t, o) -> spf "%s%s" (dump_param_opt o) (dump_t ~depth t)

and dump_rest_params ~depth = function
  | Some (o, t) -> spf "...%s" (dump_param ~depth (o, t, { prm_optional = false }))
  | _ -> ""

and dump_bound ~depth = function
  | Some t -> spf " <: %s" (dump_t ~depth t)
  | _ -> ""

and dump_polarity = function
  | Positive -> "+"
  | Negative -> "-"
  | Neutral -> ""

and dump_type_param ~depth { tp_name; tp_bound; tp_polarity; _ } =
  spf "TParam(%s, %s, %s)" (dump_polarity tp_polarity) tp_name (dump_bound ~depth tp_bound)

and dump_type_params ~depth = function
  | Some [] -> ""
  | Some ps -> spf "TypeParams(%s)" (dump_list (dump_type_param ~depth) ps)
  | _ -> ""

and dump_fun_t ~depth { fun_params; fun_rest_param; fun_return; fun_type_params; fun_static } =
  spf
    "Fun(%s, %s, %s, %s, out: %s)"
    (dump_type_params ~depth fun_type_params)
    (dump_list (dump_param ~depth) fun_params)
    (dump_rest_params ~depth fun_rest_param)
    (dump_t ~depth fun_static)
    (dump_t ~depth fun_return)

and dump_field ~depth x t polarity optional =
  spf
    "%s%s%s: %s"
    (dump_polarity polarity)
    x
    (if optional then
      "?"
    else
      "")
    (dump_t ~depth t)

and dump_prop ~depth = function
  | NamedProp { name; prop; _ } -> dump_named_prop ~depth name prop
  | CallProp f -> dump_fun_t ~depth f
  | SpreadProp t -> dump_spread ~depth t

and dump_named_prop ~depth x = function
  | Field { t; polarity; optional } ->
    dump_field ~depth (Reason.display_string_of_name x) t polarity optional
  | Method t -> dump_fun_t ~depth t
  | Get t -> spf "get %s" (dump_t ~depth t)
  | Set t -> spf "get %s" (dump_t ~depth t)

and dump_dict ~depth { dict_polarity; dict_name; dict_key; dict_value } =
  spf
    "%s[%s%s]: %s"
    (dump_polarity dict_polarity)
    (match dict_name with
    | Some n -> spf "%s: " n
    | _ -> "")
    (dump_t ~depth dict_key)
    (dump_t ~depth dict_value)

and dump_spread ~depth t = spf "...%s" (dump_t ~depth t)

and dump_obj ~depth { obj_kind; obj_props; _ } =
  match obj_kind with
  | ExactObj -> spf "{|%s|}" (dump_list (dump_prop ~depth) obj_props)
  | InexactObj -> spf "{%s, ...}" (dump_list (dump_prop ~depth) obj_props)
  | IndexedObj d ->
    let dict = dump_dict ~depth d in
    spf "{%s, %s}" (dump_list (dump_prop ~depth) obj_props) dict

and dump_arr ~depth { arr_readonly; arr_elt_t; _ } =
  let ctor =
    if arr_readonly then
      "$ReadOnlyArray"
    else
      "Array"
  in
  spf "%s<%s>" ctor (dump_t ~depth arr_elt_t)

and dump_generic ~depth (s, kind, ts) =
  spf
    "Generic (%s, kind= %s, params=%s)"
    (dump_symbol s)
    (Ty.debug_string_of_generic_kind kind)
    (dump_generics ~depth ts)

and dump_generics ~depth = function
  | Some ts -> "<" ^ dump_list (dump_t ~depth) ts ^ ">"
  | _ -> ""

and dump_tvar (RVar i) = spf "T_%d" i

and dump_utility ~depth u =
  let ctor = Ty.string_of_utility_ctor u in
  match Ty.types_of_utility u with
  | Some ts -> Base.List.map ~f:(dump_t ~depth) ts |> String.concat ", " |> spf "%s (%s)" ctor
  | None -> ctor

and dump_t ?(depth = 10) t =
  if depth < 0 then
    "..."
  else
    let depth = depth - 1 in
    match t with
    | TVar (v, ts) -> spf "TVAR(%s, params=%s)" (dump_tvar v) (dump_generics ~depth ts)
    | Bound (_, s) -> spf "Bound(%s)" s
    | Generic g -> dump_generic ~depth g
    | Any kind -> spf "Any (%s)" (dump_any_kind kind)
    | Top -> "Top"
    | Bot k -> spf "Bot (%s)" (dump_bot_kind k)
    | Void -> "Void"
    | Null -> "Null"
    | Symbol -> "Symbol"
    | Num (Some x) -> spf "Num (%s)" x
    | Num None -> "Num"
    | NumLit s -> spf "\"%s\"" s
    | Str (Some x) -> spf "Str (%s)" (Reason.display_string_of_name x)
    | Str None -> "Str"
    | StrLit s -> spf "\"%s\"" (Reason.display_string_of_name s)
    | Bool (Some x) -> spf "Bool (%b)" x
    | Bool None -> "Bool"
    | BoolLit b -> spf "\"%b\"" b
    | Fun f -> dump_fun_t ~depth f
    | Obj o -> dump_obj ~depth o
    | Arr a -> dump_arr ~depth a
    | Tup ts -> spf "Tup (%s)" (dump_list (dump_t ~depth) ~sep:"," ts)
    | Union (t1, t2, ts) ->
      spf "Union (%s)" (dump_list (dump_t ~depth) ~sep:", " (Base.List.take (t1 :: t2 :: ts) 10))
    | Inter (t1, t2, ts) -> spf "Inter (%s)" (dump_list (dump_t ~depth) ~sep:", " (t1 :: t2 :: ts))
    | InlineInterface { if_extends; if_props; if_dict } ->
      let dict =
        match if_dict with
        | None -> ""
        | Some d -> dump_dict ~depth d
      in
      spf
        "InlineInterface (%s, %s)"
        (dump_list (dump_generic ~depth) if_extends)
        (spf "{ %s %s }" (dump_list (dump_prop ~depth) if_props) dict)
    | TypeOf v -> spf "Typeof (%s)" (builtin_value v)
    | Utility u -> dump_utility ~depth u
    | IndexedAccess { _object; index; optional } ->
      spf
        "IndexedAccess (%s) (%s) (optional=%b)"
        (dump_t ~depth _object)
        (dump_t ~depth index)
        optional
    | Mu (i, t) -> spf "Mu (%d, %s)" i (dump_t ~depth t)
    | CharSet s -> spf "CharSet (%s)" s

and dump_class_decl ~depth (name, ps) =
  spf "Class (name=%s, params= %s)" (dump_symbol name) (dump_type_params ~depth ps)

and dump_interface_decl ~depth (name, ps) =
  spf "Interface (name=%s, params= %s)" (dump_symbol name) (dump_type_params ~depth ps)

and dump_module ~depth name exports _default =
  let name =
    match name with
    | Some name -> dump_symbol name
    | None -> "<no name>"
  in
  let exports = dump_list (dump_decl ~depth) ~sep:", " exports in
  spf "Module(%s, %s)" name exports

and dump_decl ~depth = function
  | VariableDecl (name, t) ->
    spf "VariableDecl (%s, %s)" (Reason.display_string_of_name name) (dump_t ~depth t)
  | TypeAliasDecl { import; name; tparams; type_ } ->
    spf
      "TypeAlias (import=%b, %s, %s, %s)"
      import
      (dump_symbol name)
      (dump_type_params ~depth tparams)
      (Base.Option.value_map type_ ~default:"" ~f:(fun t -> cut_off (dump_t ~depth t)))
  | ClassDecl (s, ps) -> spf "ClassDecl (%s) (%s)" (dump_symbol s) (dump_type_params ~depth ps)
  | InterfaceDecl (s, ps) ->
    spf "InterfaceDecl (%s) (%s)" (dump_symbol s) (dump_type_params ~depth ps)
  | EnumDecl name -> spf "Enum(%s)" (dump_symbol name)
  | ModuleDecl { name; exports; default } -> dump_module ~depth name exports default

and dump_elt ~depth = function
  | Type t -> spf "Type (%s)" (dump_t ~depth t)
  | Decl d -> spf "Decl (%s)" (dump_decl ~depth d)

let dump_binding (v, ty) = Utils_js.spf "type %s = %s" (dump_tvar v) (dump_t ty)

let dump_env_t s = Base.List.map ~f:dump_binding s |> String.concat "\n"

let string_of_polarity = function
  | Negative -> "Negative"
  | Neutral -> "Neutral"
  | Positive -> "Positive"

let string_of_ctor_t = function
  | TVar (RVar _, _) -> "RecVar"
  | Bound _ -> "Bound"
  | Generic _ -> "Generic"
  | Any (Annotated _) -> "Explicit Any"
  | Any _ -> "Implicit Any"
  | Top -> "Top"
  | Bot _ -> "Bot"
  | Void -> "Void"
  | Null -> "Null"
  | Symbol -> "Symbol"
  | Num _ -> "Num"
  | Str _ -> "Str"
  | Bool _ -> "Bool"
  | NumLit _ -> "NumLit"
  | StrLit _ -> "StrLit"
  | BoolLit _ -> "BoolLit"
  | Fun _ -> "Fun"
  | Obj _ -> "Obj"
  | Arr _ -> "Arr"
  | Tup _ -> "Tup"
  | Union _ -> "Union"
  | Inter _ -> "Inter"
  | InlineInterface _ -> "InlineInterface"
  | TypeOf _ -> "Typeof"
  | Utility _ -> "Utility"
  | IndexedAccess _ -> "IndexedAccess"
  | Mu _ -> "Mu"
  | CharSet _ -> "CharSet"

let string_of_ctor_decl = function
  | TypeAliasDecl _ -> "TypeAlias"
  | ClassDecl _ -> "ClassDecl"
  | InterfaceDecl _ -> "InterfaceDecl"
  | ModuleDecl _ -> "Module"
  | VariableDecl _ -> "VariableDecl"
  | EnumDecl _ -> "EnumDecl"

let json_of_elt ~strip_root =
  let json_of_provenance loc p =
    Hh_json.(
      JSON_Object
        [
          ("kind", JSON_String (ctor_of_provenance p));
          ("loc", JSON_String (Reason.string_of_aloc ~strip_root loc));
        ])
  in
  let json_of_symbol { sym_provenance; sym_def_loc; sym_name; _ } =
    Hh_json.(
      JSON_Object
        [
          ("provenance", json_of_provenance sym_def_loc sym_provenance);
          ("name", JSON_String (Reason.display_string_of_name sym_name));
        ])
  in
  let json_of_builtin_value =
    Hh_json.(
      function
      | FunProto -> JSON_String "Function.prototype"
      | ObjProto -> JSON_String "Object.prototype"
      | FunProtoApply -> JSON_String "Function.prototype.apply"
      | FunProtoBind -> JSON_String "Function.prototype.bind"
      | FunProtoCall -> JSON_String "Function.prototype.call"
      | TSymbol s -> json_of_symbol s)
  in
  let rec json_of_t t =
    Hh_json.(JSON_Object (("kind", JSON_String (string_of_ctor_t t)) :: json_of_t_list t))
  and json_of_t_list t =
    Hh_json.(
      match t with
      | TVar (v, ts) -> json_of_tvar v @ json_of_targs ts
      | Bound (_, name) -> [("bound", JSON_String name)]
      | Generic g -> json_of_generic g
      | Any (Annotated _) -> [("any", JSON_String "explicit")]
      | Any _ -> [("any", JSON_String "implicit")]
      | Top
      | Bot _
      | Void
      | Null
      | Symbol
      | Num _
      | Str _
      | Bool _ ->
        []
      | NumLit s -> [("literal", JSON_String s)]
      | StrLit s -> [("literal", JSON_String (Reason.display_string_of_name s))]
      | BoolLit b -> [("literal", JSON_Bool b)]
      | Fun f -> json_of_fun_t f
      | Obj o -> json_of_obj_t o
      | Arr { arr_readonly; arr_literal; arr_elt_t } ->
        [
          ("readonly", JSON_Bool arr_readonly);
          ("literal", Base.Option.value_map arr_literal ~f:(fun t -> JSON_Bool t) ~default:JSON_Null);
          ("type", json_of_t arr_elt_t);
        ]
      | Tup ts -> [("types", JSON_Array (Base.List.map ~f:json_of_t ts))]
      | Union (t0, t1, ts) -> [("types", JSON_Array (Base.List.map ~f:json_of_t (t0 :: t1 :: ts)))]
      | Inter (t0, t1, ts) -> [("types", JSON_Array (Base.List.map ~f:json_of_t (t0 :: t1 :: ts)))]
      | InlineInterface { if_extends; if_props; if_dict } ->
        Hh_json.(
          let extends = Base.List.map ~f:(fun g -> JSON_Object (json_of_generic g)) if_extends in
          [
            ("extends", JSON_Array extends);
            ("body", JSON_Array (Base.List.map ~f:json_of_prop if_props));
            ("dict", Base.Option.value_map if_dict ~f:(fun d -> json_of_dict d) ~default:JSON_Null);
          ])
      | TypeOf b -> [("name", json_of_builtin_value b)]
      | Utility u -> json_of_utility u
      | IndexedAccess { _object; index; optional } ->
        [
          ("object", json_of_t _object); ("index", json_of_t index); ("optional", JSON_Bool optional);
        ]
      | Mu (i, t) -> [("mu_var", int_ i); ("type", json_of_t t)]
      | CharSet s -> [("literal", JSON_String s)])
  and json_of_tvar (RVar i) = Hh_json.[("id", int_ i)]
  and json_of_generic (s, k, targs_opt) =
    json_of_targs targs_opt
    @ [
        ("type", json_of_symbol s);
        ("generic_kind", Hh_json.JSON_String (Ty.debug_string_of_generic_kind k));
      ]
  and json_of_fun_t { fun_params; fun_rest_param; fun_return; fun_type_params; fun_static } =
    let open Hh_json in
    [
      ("typeParams", json_of_type_params fun_type_params);
      ("paramTypes", JSON_Array (Base.List.map ~f:(fun (_, t, _) -> json_of_t t) fun_params));
      ( "paramNames",
        JSON_Array
          (List.rev_map
             (function
               | (Some n, _, _) -> JSON_String n
               | (None, _, _) -> JSON_String "_")
             fun_params) );
      ( "restParam",
        match fun_rest_param with
        | None -> JSON_Null
        | Some (name, t) ->
          JSON_Object
            ([("restParamType", json_of_t t)]
            @
            match name with
            | None -> []
            | Some name -> [("restParamName", JSON_String name)]) );
      ("returnType", json_of_t fun_return);
      ("staticType", json_of_t fun_static);
    ]
  and json_of_obj_t o =
    Hh_json.(
      let { obj_kind; obj_props; obj_literal; obj_frozen } = o in
      let obj_kind =
        match obj_kind with
        | ExactObj -> JSON_String "Exact"
        | InexactObj -> JSON_String "Inexact"
        | IndexedObj d -> json_of_dict d
      in
      [
        ("obj_kind", obj_kind);
        ("frozen", JSON_Bool obj_frozen);
        ("literal", Base.Option.value_map obj_literal ~f:(fun t -> JSON_Bool t) ~default:JSON_Null);
        ("props", JSON_Array (Base.List.map ~f:json_of_prop obj_props));
      ])
  and json_of_type_params ps =
    Hh_json.(
      match ps with
      | None -> JSON_Null
      | Some tparams -> JSON_Array (Base.List.map ~f:json_of_typeparam tparams))
  and json_of_targs targs_opt =
    Hh_json.(
      match targs_opt with
      | Some targs -> [("typeArgs", JSON_Array (Base.List.map ~f:json_of_t targs))]
      | None -> [])
  and json_of_typeparam
      { tp_name : string; tp_bound : t option; tp_polarity : polarity; tp_default : t option } =
    Hh_json.(
      JSON_Object
        ([
           ("name", JSON_String tp_name);
           ("bound", Base.Option.value_map tp_bound ~f:json_of_t ~default:JSON_Null);
           ("polarity", json_of_polarity tp_polarity);
         ]
        @ Base.Option.value_map tp_default ~default:[] ~f:(fun t -> [("default", json_of_t t)])))
  and json_of_polarity polarity = Hh_json.JSON_String (string_of_polarity polarity)
  and json_of_prop prop =
    Hh_json.(
      JSON_Object
        (match prop with
        | NamedProp { name; prop; from_proto; def_loc } ->
          [
            ("kind", JSON_String "NamedProp");
            ( "prop",
              JSON_Object
                [
                  ("name", JSON_String (Reason.display_string_of_name name));
                  ("prop", json_of_named_prop prop);
                  ("from_proto", JSON_Bool from_proto);
                  ( "def_loc",
                    match def_loc with
                    | None -> JSON_Null
                    | Some loc -> JSON_String (Reason.string_of_aloc loc) );
                ] );
          ]
        | CallProp ft ->
          [("kind", JSON_String "CallProp"); ("prop", JSON_Object (json_of_fun_t ft))]
        | SpreadProp t -> [("kind", JSON_String "SpreadProp"); ("prop", json_of_t t)]))
  and json_of_dict { dict_polarity; dict_name; dict_key; dict_value } =
    Hh_json.(
      JSON_Object
        [
          ("polarity", json_of_polarity dict_polarity);
          ("name", JSON_String (Base.Option.value dict_name ~default:"_"));
          ("key", json_of_t dict_key);
          ("value", json_of_t dict_value);
        ])
  and json_of_named_prop p =
    Hh_json.(
      JSON_Object
        (match p with
        | Field { t; polarity; optional } ->
          [
            ("kind", JSON_String "Field");
            ("type", json_of_t t);
            ("polarity", json_of_polarity polarity);
            ("optional", JSON_Bool optional);
          ]
        | Method t -> [("kind", JSON_String "Method"); ("funtype", JSON_Object (json_of_fun_t t))]
        | Get t -> [("kind", JSON_String "Get"); ("type", json_of_t t)]
        | Set t -> [("kind", JSON_String "Set"); ("type", json_of_t t)]))
  and json_of_utility u =
    Hh_json.(
      let ctor = Ty.string_of_utility_ctor u in
      let ts = json_of_targs (Ty.types_of_utility u) in
      ("kind", JSON_String ctor) :: ts)
  in
  let json_of_class_decl (name, tparams) =
    [("name", json_of_symbol name); ("typeParams", json_of_type_params tparams)]
  in
  let json_of_interface_decl (name, tparams) =
    [("name", json_of_symbol name); ("typeParams", json_of_type_params tparams)]
  in
  let json_of_module name _ _ =
    Hh_json.[("name", Base.Option.value_map ~f:json_of_symbol ~default:JSON_Null name)]
  in
  let json_of_decl =
    let open Hh_json in
    function
    | VariableDecl (name, t) ->
      [("name", JSON_String (Reason.display_string_of_name name)); ("type_", json_of_t t)]
    | TypeAliasDecl { name; tparams; type_; _ } ->
      [
        ("name", json_of_symbol name);
        ("typeParams", json_of_type_params tparams);
        ("body", Base.Option.value_map ~f:json_of_t ~default:JSON_Null type_);
      ]
    | ClassDecl (s, ps) -> json_of_class_decl (s, ps)
    | InterfaceDecl (s, ps) -> json_of_interface_decl (s, ps)
    | EnumDecl name -> [("name", json_of_symbol name)]
    | ModuleDecl { name; exports; default } -> json_of_module name exports default
  in
  fun elt ->
    let payload =
      match elt with
      | Type t -> json_of_t_list t
      | Decl d -> json_of_decl d
    in
    let kind =
      match elt with
      | Type t -> string_of_ctor_t t
      | Decl d -> string_of_ctor_decl d
    in
    Hh_json.(JSON_Object (("kind", JSON_String kind) :: payload))
