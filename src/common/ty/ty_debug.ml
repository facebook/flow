(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty
open Utils_js

let string_of_polarity = function
  | Negative -> "Negative"
  | Neutral -> "Neutral"
  | Positive -> "Positive"

let string_of_ctor_t = function
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
  | BigInt _ -> "BigInt"
  | NumLit _ -> "NumLit"
  | StrLit _ -> "StrLit"
  | BoolLit _ -> "BoolLit"
  | BigIntLit _ -> "BigIntLit"
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
  | Conditional _ -> "Conditional"
  | Infer _ -> "Infer"
  | Renders _ -> "Renders"

let string_of_ctor_decl = function
  | TypeAliasDecl _ -> "TypeAlias"
  | ClassDecl _ -> "ClassDecl"
  | InterfaceDecl _ -> "InterfaceDecl"
  | ModuleDecl _ -> "Module"
  | NamespaceDecl _ -> "NamespaceDecl"
  | VariableDecl _ -> "VariableDecl"
  | NominalComponentDecl _ -> "NominalComponentDecl"
  | EnumDecl _ -> "EnumDecl"

let dump_any_error_kind = function
  | Some UnresolvedName -> "UnresolvedName"
  | Some MissingAnnotation -> "MissingAnnotation"
  | None -> "<None>"

let dump_any_unsoundness_kind = function
  | BoundFunctionThis -> "BoundFunctionThis"
  | ComputedNonLiteralKey -> "ComputedNonLiteralKey"
  | Constructor -> "Constructor"
  | DummyStatic -> "DummyStatic"
  | Exports -> "Exports"
  | FunctionPrototype -> "FunctionPrototype"
  | InferenceHooks -> "InferenceHooks"
  | InstanceOfRefinement -> "InstanceOfRefinement"
  | Merged -> "Merged"
  | ResolveSpread -> "ResolveSpread"
  | Unchecked -> "Unchecked"
  | Unimplemented -> "Unimplemented"
  | UnresolvedType -> "UnresolvedType"
  | NonBindingPattern -> "NonBindingPattern"

let dump_any_kind = function
  | Annotated _ -> "Annotated"
  | AnyError kind -> spf "AnyError (%s)" (dump_any_error_kind kind)
  | Recursive -> "Recursive"
  | Unsound kind -> spf "Unsound (%s)" (dump_any_unsoundness_kind kind)
  | Untyped -> "Untyped"
  | Placeholder -> "Placeholder"

module Make (C : sig
  val aloc_to_loc : (ALoc.t -> Loc.t) option
end) =
struct
  let string_of_aloc =
    match C.aloc_to_loc with
    | Some aloc_to_loc ->
      (fun ?(strip_root = None) loc -> aloc_to_loc loc |> Reason.string_of_loc ~strip_root)
    | None -> (fun ?(strip_root = None) loc -> Reason.string_of_aloc ~strip_root loc)

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
      (string_of_aloc sym_def_loc)

  let builtin_value = function
    | FunProto -> "Function.prototype"
    | ObjProto -> "Object.prototype"
    | FunProtoBind -> "Function.prototype.bind"
    | TSymbol s -> dump_symbol s

  let rec dump_opt (f : 'a -> string) (o : 'a option) =
    match o with
    | Some t -> f t
    | None -> ""

  and dump_list : 'a. ('a -> string) -> ?sep:string -> 'a list -> string =
   (fun f ?(sep = ", ") ls -> Base.List.map ~f ls |> String.concat sep)

  and dump_listi : 'a. (int -> 'a -> string) -> ?sep:string -> 'a list -> string =
   (fun f ?(sep = ", ") ls -> Base.List.mapi ~f ls |> String.concat sep)

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

  and dump_fun_t
      ~depth { fun_params; fun_rest_param; fun_return; fun_type_params; fun_static; fun_effect } =
    spf
      "Fun(%s, %s, %s, %s, out: %s, hook: %b)"
      (dump_type_params ~depth fun_type_params)
      (dump_list (dump_param ~depth) fun_params)
      (dump_rest_params ~depth fun_rest_param)
      (dump_t ~depth fun_static)
      (dump_return_t ~depth fun_return)
      (fun_effect = Ty.Hook)

  and dump_return_t ~depth t =
    match t with
    | ReturnType t -> dump_t ~depth t
    | TypeGuard (impl, x, t) ->
      let impl =
        if impl then
          "implies "
        else
          ""
      in
      spf "%s%s is %s" impl x (dump_t ~depth t)

  and dump_tuple_element ~depth i name t polarity optional =
    if Base.Option.is_none name && polarity = Neutral && not optional then
      dump_t ~depth t
    else
      spf
        "%s%s%s: %s"
        (dump_polarity polarity)
        (Base.Option.value ~default:(spf "element_%d" i) name)
        ( if optional then
          "?"
        else
          ""
        )
        (dump_t ~depth t)

  and dump_tuple_spread ~depth name t =
    let name = Base.Option.value_map name ~default:"" ~f:(fun name -> spf "%s: " name) in
    spf "...%s%s" name (dump_t ~depth t)

  and dump_field ~depth x t polarity optional =
    spf
      "%s%s%s: %s"
      (dump_polarity polarity)
      x
      ( if optional then
        "?"
      else
        ""
      )
      (dump_t ~depth t)

  and dump_prop ~depth = function
    | NamedProp { name; prop; _ } -> dump_named_prop ~depth name prop
    | CallProp f -> dump_fun_t ~depth f
    | SpreadProp t -> dump_spread ~depth t
    | MappedTypeProp { key_tparam; source; prop; flags; homomorphic } ->
      dump_mapped_type ~depth key_tparam source prop flags homomorphic

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

  and dump_mapped_type ~depth { tp_name; _ } source prop { optional; polarity } homomorphic =
    spf
      "%s[%s in %s%s]%s: %s"
      (dump_polarity polarity)
      tp_name
      (match homomorphic with
      | Homomorphic -> "keyof "
      | _ -> "")
      (dump_t ~depth source)
      (match optional with
      | KeepOptionality -> ""
      | MakeOptional -> "?"
      | RemoveOptional -> "-?")
      (dump_t ~depth prop)

  and dump_obj ~depth { obj_kind; obj_props; _ } =
    match obj_kind with
    | ExactObj -> spf "{|%s|}" (dump_list (dump_prop ~depth) obj_props)
    | InexactObj -> spf "{%s, ...}" (dump_list (dump_prop ~depth) obj_props)
    | IndexedObj d ->
      let dict = dump_dict ~depth d in
      spf "{%s, %s}" (dump_list (dump_prop ~depth) obj_props) dict
    | MappedTypeObj -> spf "{%s}" (dump_list (dump_prop ~depth) obj_props)

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
      | BigInt (Some x) -> spf "BigInt (%s)" x
      | BigInt None -> "BigInt"
      | BigIntLit s -> spf "\"%s\"" s
      | Fun f -> dump_fun_t ~depth f
      | Obj o -> dump_obj ~depth o
      | Arr a -> dump_arr ~depth a
      | Tup { elements; inexact } ->
        spf
          "Tup (%s%s)"
          (dump_listi
             (fun i -> function
               | TupleElement { name; t; polarity; optional } ->
                 dump_tuple_element ~depth i name t polarity optional
               | TupleSpread { t; name } -> dump_tuple_spread ~depth name t)
             ~sep:","
             elements
          )
          ( if inexact then
            ", ..."
          else
            ""
          )
      | Union (_, t1, t2, ts) ->
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
      | TypeOf (v, ts) -> spf "Typeof (%s, %s)" (builtin_value v) (dump_generics ~depth ts)
      | Utility u -> dump_utility ~depth u
      | IndexedAccess { _object; index; optional } ->
        spf
          "IndexedAccess (%s) (%s) (optional=%b)"
          (dump_t ~depth _object)
          (dump_t ~depth index)
          optional
      | Conditional { check_type; extends_type; true_type; false_type } ->
        spf
          "Conditional (%s, %s, %s, %s)"
          (dump_t ~depth check_type)
          (dump_t ~depth extends_type)
          (dump_t ~depth true_type)
          (dump_t ~depth false_type)
      | Infer (s, b) ->
        spf
          "Infer (%s, %s)"
          (dump_symbol s)
          (Base.Option.value_map ~default:"None" ~f:(dump_t ~depth) b)
      | Renders (t, _) -> spf "Renders (%s)" (dump_t ~depth t)

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

  and dump_namespace ~depth name exports =
    let name =
      match name with
      | Some name -> dump_symbol name
      | None -> "<no name>"
    in
    let exports = dump_list (dump_decl ~depth) ~sep:", " exports in
    spf "Namespace(%s, %s)" name exports

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
    | NominalComponentDecl { name; tparams; targs = _; is_type } ->
      spf
        "NominalComponentDecl (%s, %s, %b)"
        (dump_symbol name)
        (dump_type_params ~depth tparams)
        is_type
    | NamespaceDecl { name; exports } -> dump_namespace ~depth name exports
    | ModuleDecl { name; exports; default } -> dump_module ~depth name exports default

  and dump_elt ~depth = function
    | Type t -> spf "Type (%s)" (dump_t ~depth t)
    | Decl d -> spf "Decl (%s)" (dump_decl ~depth d)

  let json_of_elt ~strip_root =
    let json_of_provenance loc p =
      Hh_json.(
        JSON_Object
          [
            ("kind", JSON_String (ctor_of_provenance p));
            ("loc", JSON_String (string_of_aloc ~strip_root loc));
          ]
      )
    in
    let json_of_symbol { sym_provenance; sym_def_loc; sym_name; _ } =
      Hh_json.(
        JSON_Object
          [
            ("provenance", json_of_provenance sym_def_loc sym_provenance);
            ("name", JSON_String (Reason.display_string_of_name sym_name));
          ]
      )
    in
    let json_of_builtin_value =
      Hh_json.(
        function
        | FunProto -> JSON_String "Function.prototype"
        | ObjProto -> JSON_String "Object.prototype"
        | FunProtoBind -> JSON_String "Function.prototype.bind"
        | TSymbol s -> json_of_symbol s
      )
    in
    let rec json_of_t t =
      Hh_json.(JSON_Object (("kind", JSON_String (string_of_ctor_t t)) :: json_of_t_list t))
    and json_of_t_list t =
      Hh_json.(
        match t with
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
        | Bool _
        | BigInt _ ->
          []
        | NumLit s -> [("literal", JSON_String s)]
        | StrLit s -> [("literal", JSON_String (Reason.display_string_of_name s))]
        | BoolLit b -> [("literal", JSON_Bool b)]
        | BigIntLit s -> [("literal", JSON_String s)]
        | Fun f -> json_of_fun_t f
        | Obj o -> json_of_obj_t o
        | Arr { arr_readonly; arr_literal; arr_elt_t } ->
          [
            ("readonly", JSON_Bool arr_readonly);
            ( "literal",
              Base.Option.value_map arr_literal ~f:(fun t -> JSON_Bool t) ~default:JSON_Null
            );
            ("type", json_of_t arr_elt_t);
          ]
        | Tup { elements; inexact } ->
          [
            ( "elements",
              JSON_Array
                (Base.List.map
                   ~f:(function
                     | TupleElement { t; name; polarity; optional } ->
                       JSON_Object
                         [
                           ("kind", JSON_String "TupleElement");
                           ("name", JSON_String (Base.Option.value name ~default:""));
                           ("t", json_of_t t);
                           ("optional", JSON_Bool optional);
                           ("polarity", json_of_polarity polarity);
                         ]
                     | TupleSpread { t; name } ->
                       JSON_Object
                         [
                           ("kind", JSON_String "TupleSpread");
                           ("name", JSON_String (Base.Option.value name ~default:""));
                           ("t", json_of_t t);
                         ])
                   elements
                )
            );
            ("inexact", JSON_Bool inexact);
          ]
        | Union (_, t0, t1, ts) ->
          [("types", JSON_Array (Base.List.map ~f:json_of_t (t0 :: t1 :: ts)))]
        | Inter (t0, t1, ts) ->
          [("types", JSON_Array (Base.List.map ~f:json_of_t (t0 :: t1 :: ts)))]
        | InlineInterface { if_extends; if_props; if_dict } ->
          Hh_json.(
            let extends = Base.List.map ~f:(fun g -> JSON_Object (json_of_generic g)) if_extends in
            [
              ("extends", JSON_Array extends);
              ("body", JSON_Array (Base.List.map ~f:json_of_prop if_props));
              ("dict", Base.Option.value_map if_dict ~f:(fun d -> json_of_dict d) ~default:JSON_Null);
            ]
          )
        | TypeOf (b, targs) -> ("name", json_of_builtin_value b) :: json_of_targs targs
        | Utility u -> json_of_utility u
        | IndexedAccess { _object; index; optional } ->
          [
            ("object", json_of_t _object);
            ("index", json_of_t index);
            ("optional", JSON_Bool optional);
          ]
        | Conditional { check_type; extends_type; true_type; false_type } ->
          [
            ("check", json_of_t check_type);
            ("extends", json_of_t extends_type);
            ("true", json_of_t true_type);
            ("false", json_of_t false_type);
          ]
        | Infer (s, b) ->
          [
            ("name", json_of_symbol s);
            ("bound", Base.Option.value_map ~default:JSON_Null ~f:json_of_t b);
          ]
        | Renders (t, variant) ->
          [
            ("argument", json_of_t t);
            ( "variant",
              match variant with
              | RendersNormal -> Hh_json.JSON_String "normal"
              | RendersMaybe -> Hh_json.JSON_String "maybe"
              | RendersStar -> Hh_json.JSON_String "star"
            );
          ]
      )
    and json_of_generic (s, k, targs_opt) =
      json_of_targs targs_opt
      @ [
          ("type", json_of_symbol s);
          ("generic_kind", Hh_json.JSON_String (Ty.debug_string_of_generic_kind k));
        ]
    and json_of_fun_t
        { fun_params; fun_rest_param; fun_return; fun_type_params; fun_static; fun_effect } =
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
               fun_params
            )
        );
        ( "restParam",
          match fun_rest_param with
          | None -> JSON_Null
          | Some (name, t) ->
            JSON_Object
              ([("restParamType", json_of_t t)]
              @
              match name with
              | None -> []
              | Some name -> [("restParamName", JSON_String name)]
              )
        );
        ("returnType", json_of_return_t fun_return);
        ("staticType", json_of_t fun_static);
        ("functionHook", JSON_Bool (fun_effect = Ty.Hook));
      ]
    and json_of_return_t = function
      | ReturnType t -> Hh_json.(JSON_Object [("type_", json_of_t t)])
      | TypeGuard (impl, x, t) ->
        Hh_json.(
          JSON_Object
            [
              ( "type_guard",
                JSON_Object
                  [
                    ("implies", JSON_Bool impl);
                    ("type_parameter", JSON_String x);
                    ("type_", json_of_t t);
                  ]
              );
            ]
        )
    and json_of_obj_t o =
      Hh_json.(
        let { obj_def_loc; obj_kind; obj_props; obj_literal } = o in
        let obj_kind =
          match obj_kind with
          | ExactObj -> JSON_String "Exact"
          | InexactObj -> JSON_String "Inexact"
          | IndexedObj d -> json_of_dict d
          | MappedTypeObj -> JSON_String "MappedType"
        in
        [
          ( "def_loc",
            match obj_def_loc with
            | None -> JSON_Null
            | Some loc -> JSON_String (string_of_aloc loc)
          );
          ("obj_kind", obj_kind);
          ("literal", Base.Option.value_map obj_literal ~f:(fun t -> JSON_Bool t) ~default:JSON_Null);
          ("props", JSON_Array (Base.List.map ~f:json_of_prop obj_props));
        ]
      )
    and json_of_type_params ps =
      Hh_json.(
        match ps with
        | None -> JSON_Null
        | Some tparams -> JSON_Array (Base.List.map ~f:json_of_typeparam tparams)
      )
    and json_of_targs targs_opt =
      Hh_json.(
        match targs_opt with
        | Some targs -> [("typeArgs", JSON_Array (Base.List.map ~f:json_of_t targs))]
        | None -> []
      )
    and json_of_typeparam
        { tp_name : string; tp_bound : t option; tp_polarity : polarity; tp_default : t option } =
      Hh_json.(
        JSON_Object
          ([
             ("name", JSON_String tp_name);
             ("bound", Base.Option.value_map tp_bound ~f:json_of_t ~default:JSON_Null);
             ("polarity", json_of_polarity tp_polarity);
           ]
          @ Base.Option.value_map tp_default ~default:[] ~f:(fun t -> [("default", json_of_t t)])
          )
      )
    and json_of_polarity polarity = Hh_json.JSON_String (string_of_polarity polarity)
    and json_of_prop prop =
      Hh_json.(
        JSON_Object
          (match prop with
          | NamedProp { name; prop; inherited; source; def_locs } ->
            [
              ("kind", JSON_String "NamedProp");
              ( "prop",
                JSON_Object
                  [
                    ("name", JSON_String (Reason.display_string_of_name name));
                    ("prop", json_of_named_prop prop);
                    ("inherited", JSON_Bool inherited);
                    ("source", JSON_String (string_of_prop_source source));
                    ( "def_locs",
                      JSON_Array
                        (Base.List.map def_locs ~f:(fun loc -> JSON_String (string_of_aloc loc)))
                    );
                  ]
              );
            ]
          | CallProp ft ->
            [("kind", JSON_String "CallProp"); ("prop", JSON_Object (json_of_fun_t ft))]
          | SpreadProp t -> [("kind", JSON_String "SpreadProp"); ("prop", json_of_t t)]
          | MappedTypeProp
              {
                key_tparam = { tp_name; _ };
                source;
                prop;
                flags = { optional; polarity };
                homomorphic;
              } ->
            let optional_str =
              match optional with
              | KeepOptionality -> "KeepOptionality"
              | RemoveOptional -> "RemoveOptional"
              | MakeOptional -> "MakeOptional"
            in
            let homomorphic_str =
              match homomorphic with
              | Homomorphic -> "Homomorphic"
              | Unspecialized -> "Unspecialized"
              | SemiHomomorphic _ -> "SemiHomomorphic"
            in
            [
              ("kind", JSON_String "MappedTypeProp");
              ( "prop",
                JSON_Object
                  [
                    ("key_tparam", JSON_String tp_name);
                    ("source", json_of_t source);
                    ("homomorphic", JSON_String homomorphic_str);
                    ("prop", json_of_t prop);
                    ( "flags",
                      JSON_Object
                        [
                          ("polarity", json_of_polarity polarity);
                          ("optional", JSON_String optional_str);
                        ]
                    );
                  ]
              );
            ])
      )
    and json_of_dict { dict_polarity; dict_name; dict_key; dict_value } =
      Hh_json.(
        JSON_Object
          [
            ("polarity", json_of_polarity dict_polarity);
            ("name", JSON_String (Base.Option.value dict_name ~default:"_"));
            ("key", json_of_t dict_key);
            ("value", json_of_t dict_value);
          ]
      )
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
          | Set t -> [("kind", JSON_String "Set"); ("type", json_of_t t)])
      )
    and json_of_utility u =
      Hh_json.(
        let ctor = Ty.string_of_utility_ctor u in
        let ts = json_of_targs (Ty.types_of_utility u) in
        ("kind", JSON_String ctor) :: ts
      )
    in
    let json_of_class_decl (name, tparams) =
      [("name", json_of_symbol name); ("typeParams", json_of_type_params tparams)]
    in
    let json_of_interface_decl (name, tparams) =
      [("name", json_of_symbol name); ("typeParams", json_of_type_params tparams)]
    in
    let json_of_nominal_component_decl (name, tparams, is_type) =
      [
        ("name", json_of_symbol name);
        ("typeParams", json_of_type_params tparams);
        ("isType", Hh_json.JSON_Bool is_type);
      ]
    in
    let json_of_namespace name _ =
      Hh_json.[("name", Base.Option.value_map ~f:json_of_symbol ~default:JSON_Null name)]
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
      | NominalComponentDecl { name; tparams; targs = _; is_type } ->
        json_of_nominal_component_decl (name, tparams, is_type)
      | NamespaceDecl { name; exports } -> json_of_namespace name exports
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
end

module M = Make (struct
  let aloc_to_loc = None
end)

let dump_t_EXPOSES_ABSTRACT_LOCS = M.dump_t
