(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ty
open Utils_js

let cut_off ?(limit=1000) str =
  let len = String.length str in
  if len > limit
    then String.sub str 0 (len - 1) ^ " ..."
    else str

let dump_bot_upper_bound_kind = function
  | NoUpper -> "NoUpper"
  | SomeKnownUpper _ -> "SomeKnownUpper"
  | SomeUnknownUpper u -> spf "SomeUnknownUpper (%s)" u

let dump_bot_kind = function
  | EmptyType -> "EmptyType"
  | EmptyMatchingPropT -> "EmptyMatchingPropT"
  | EmptyTypeDestructorTriggerT _ -> "EmptyTypeDestructorTriggerT"
  | NoLowerWithUpper u -> spf "NoLowerWithUpper (%s)" (dump_bot_upper_bound_kind u)

let rec dump_opt (f: 'a -> string) (o: 'a option) = match o with
  | Some t -> f t
  | None -> ""

and dump_list : 'a . ('a -> string) -> ?sep:string -> 'a list -> string =
  fun f ?(sep=", ") ls ->
    Core_list.map ~f:f ls |> String.concat sep

and dump_param_opt = function
  | { prm_optional = true } -> "?"
  | _ -> ""

and dump_param ~depth (p: string option * t * fun_param) = match p with
  | Some s, t, o ->
    spf "%s%s: %s" s (dump_param_opt o) (dump_t ~depth t)
  | _, t, o -> spf "%s%s" (dump_param_opt o) (dump_t ~depth t)

and dump_rest_params ~depth = function
  | Some (o, t) ->
    spf "...%s" (dump_param ~depth (o, t, { prm_optional = false }))
  | _ -> ""

and dump_bound ~depth = function
  | Some t -> spf " <: %s" (dump_t ~depth t)
  | _ -> ""

and dump_polarity = function
  | Positive -> "+"
  | Negative -> "-"
  | Neutral -> ""

and dump_type_param ~depth { tp_name; tp_bound; tp_polarity; _ } =
  spf "TParam(%s, %s, %s)" (dump_polarity tp_polarity) tp_name
    (dump_bound ~depth tp_bound)

and dump_type_params ~depth = function
  | Some [] -> ""
  | Some ps -> spf "TypeParams(%s)" (dump_list (dump_type_param ~depth) ps)
  | _ -> ""

and dump_fun_t ~depth { fun_params; fun_rest_param; fun_return; fun_type_params } =
  spf "Fun(%s, %s, %s, out: %s)"
    (dump_type_params ~depth fun_type_params)
    (dump_list (dump_param ~depth) fun_params)
    (dump_rest_params ~depth  fun_rest_param)
    (dump_t ~depth fun_return)

and dump_field ~depth name { fld_polarity; fld_optional } t =
  spf "%s%s%s: %s"
    (dump_polarity fld_polarity)
    name
    (if fld_optional then "?" else "")
    (dump_t ~depth t)

and dump_prop ~depth = function
  | NamedProp (s, p) -> dump_named_prop ~depth s p
  | IndexProp d -> dump_dict ~depth d
  | CallProp f -> dump_fun_t ~depth f
  | SpreadProp t -> dump_spread ~depth t

and dump_named_prop ~depth x = function
  | Field (t, field) -> dump_field ~depth x field t
  | Method t -> dump_fun_t ~depth t
  | Get t -> spf "get %s" (dump_t ~depth t)
  | Set t -> spf "get %s" (dump_t ~depth t)

and dump_dict ~depth { dict_polarity; dict_name; dict_key; dict_value } =
  spf "%s[%s%s]: %s"
    (dump_polarity dict_polarity)
    (match dict_name with
      | Some n -> spf "%s: " n
      | _ -> "")
    (dump_t ~depth dict_key)
    (dump_t ~depth dict_value)

and dump_spread ~depth t =
  spf "...%s" (dump_t ~depth t)

and dump_obj ~depth { obj_exact; obj_props; _ } =
  if obj_exact
    then spf "{|%s|}" (dump_list (dump_prop ~depth) obj_props)
    else spf "{%s}"   (dump_list (dump_prop ~depth) obj_props)

and dump_arr ~depth { arr_readonly; arr_elt_t; _ } =
  let ctor = if arr_readonly then "$ReadOnlyArray" else "Array" in
  spf "%s<%s>" ctor (dump_t ~depth arr_elt_t)

and dump_generics ~depth = function
  | Some ts -> "<" ^ dump_list (dump_t ~depth) ts ^ ">"
  | _ -> ""

and dump_tvar (RVar i) = spf "T_%d" i

and dump_symbol { provenance; def_loc; name; _ } =
  spf "(%s, %s) %s" (Ty.debug_string_of_provenance_ctor provenance)
    (Reason.string_of_aloc def_loc) name

and dump_utility ~depth u =
  let ctor = Ty.string_of_utility_ctor u in
  match Ty.types_of_utility u with
  | Some ts ->
    Core_list.map ~f:(dump_t ~depth) ts |>
    String.concat ", " |>
    spf "%s (%s)" ctor
  | None -> ctor

and dump_t ?(depth = 10) t =
  if depth < 0 then "..." else
  let depth = depth - 1 in
  match t with
  | TVar (v, ts) ->
    spf "TVAR(%s, params=%s)" (dump_tvar v)
      (dump_generics ~depth ts)
  | Bound (_, s) -> spf "Bound(%s)" s
  | Generic (s, kind, ts) ->
    spf "Generic (%s, kind= %s, params=%s)"
      (dump_symbol s)
      (Ty.debug_string_of_generic_kind kind)
      (dump_generics ~depth ts)
  | Any Implicit -> "Implicit Any"
  | Any Explicit -> "Explicit Any"
  | Top -> "Top"
  | Bot k -> spf "Bot (%s)" (dump_bot_kind k)
  | Void -> "Void"
  | Null -> "Null"
  | Num (Some x) -> spf "Num (%s)" x
  | Num None -> "Num"
  | NumLit s -> spf "\"%s\"" s
  | Str (Some x) -> spf "Str (%s)" x
  | Str None -> "Str"
  | StrLit s -> spf "\"%s\"" s
  | Bool (Some x) -> spf "Bool (%b)" x
  | Bool None -> "Bool"
  | BoolLit b ->  spf "\"%b\"" b
  | Fun f -> dump_fun_t ~depth f
  | Obj o -> dump_obj ~depth o
  | Arr a -> dump_arr ~depth a
  | Tup ts ->
    spf "Tup (%s)" (dump_list (dump_t ~depth) ~sep:"," ts)
  | Union (t1,t2,ts) ->
    spf "Union (%s)" (dump_list (dump_t ~depth) ~sep:", " (ListUtils.first_n 10 (t1::t2::ts)))
  | Inter (t1,t2,ts) ->
    spf "Inter (%s)" (dump_list (dump_t ~depth) ~sep:", " (t1::t2::ts))
  | TypeAlias { ta_name; ta_tparams; ta_type } ->
    spf "TypeAlias (%s, %s, %s)"
      (dump_symbol ta_name)
      (dump_type_params ~depth ta_tparams)
      (Option.value_map ta_type ~default:"" ~f:(fun t -> cut_off (dump_t ~depth t)))
  | TypeOf (path, n) ->
    spf "Typeof(%s)" (String.concat "." (path@[n]))
  | Module n ->
    spf "Module(%s)" (dump_symbol n)
  | ClassDecl (name, ps) ->
    spf "Class (name=%s, params= %s)" (dump_symbol name) (dump_type_params ~depth ps)
  | InterfaceDecl (name, ps) ->
    spf "Interface (name=%s, params= %s)" (dump_symbol name) (dump_type_params ~depth ps)
  | Utility u -> dump_utility ~depth u
  | Mu (i, t) -> spf "Mu (%d, %s)" i (dump_t ~depth t)

let dump_binding (v, ty) =
  Utils_js.spf "type %s = %s" (dump_tvar v) (dump_t ty)

let dump_env_t s = Core_list.map ~f:dump_binding s |> String.concat "\n"

let string_of_polarity = function
  | Negative -> "Negative"
  | Neutral -> "Neutral"
  | Positive -> "Positive"

let string_of_ctor = function
  | TVar (RVar _, _) -> "RecVar"
  | Bound _ -> "Bound"
  | Generic _ -> "Generic"
  | Any Implicit -> "Implicit Any"
  | Any Explicit -> "Explicit Any"
  | Top -> "Top"
  | Bot _ -> "Bot"
  | Void -> "Void"
  | Null -> "Null"
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
  | TypeAlias _ -> "TypeAlias"
  | TypeOf _ -> "Typeof"
  | ClassDecl _ -> "ClassDecl"
  | InterfaceDecl _ -> "InterfaceDecl"
  | Utility _ -> "Utility"
  | Module _ -> "Module"
  | Mu _ -> "Mu"


let json_of_t ~strip_root =

  let json_of_provenance loc p = Hh_json.(JSON_Object [
      "kind", JSON_String (Ty.debug_string_of_provenance_ctor p);
      "loc", JSON_String (Reason.string_of_aloc ~strip_root loc);
    ])
  in

  let json_of_symbol { provenance; def_loc; name; _ } = Hh_json.(
    JSON_Object [
      "provenance", json_of_provenance def_loc provenance;
      "name", JSON_String name;
    ])
  in

  let rec json_of_t t = Hh_json.(
    JSON_Object ([
      "kind", JSON_String (string_of_ctor t)
    ] @
    match t with
    | TVar (v, ts) -> json_of_tvar v @ json_of_targs ts
    | Bound (_, name) -> [
        "bound", JSON_String name
      ]
    | Generic (s, k, targs_opt) ->
      json_of_targs targs_opt @ [
        "type", json_of_symbol s;
        "kind", JSON_String (Ty.debug_string_of_generic_kind k);
      ]
    | Any Implicit -> [ "any", JSON_String "implicit" ]
    | Any Explicit -> [ "any", JSON_String "explicit" ]
    | Top | Bot _
    | Void | Null
    | Num _ | Str _ | Bool _ -> []
    | NumLit s
    | StrLit s -> [
        "literal", JSON_String s
      ]
    | BoolLit b -> [
        "literal", JSON_Bool b
      ]
    | Fun f -> json_of_fun_t f
    | Obj { obj_exact; obj_props; obj_literal; obj_frozen } -> [
        "exact", JSON_Bool obj_exact;
        "frozen", JSON_Bool obj_frozen;
        "literal", JSON_Bool obj_literal;
        "props", JSON_Array (Core_list.map ~f:json_of_prop obj_props);
      ]
    | Arr { arr_readonly; arr_literal; arr_elt_t; } -> [
        "readonly", JSON_Bool arr_readonly;
        "literal", JSON_Bool arr_literal;
        "type", json_of_t arr_elt_t;
      ]
    | Tup ts -> [
        "types", JSON_Array (Core_list.map ~f:json_of_t ts);
      ]
    | Union (t0,t1,ts) -> [
        "types", JSON_Array (Core_list.map ~f:json_of_t (t0::t1::ts));
      ]
    | Inter (t0,t1,ts) -> [
        "types", JSON_Array (Core_list.map ~f:json_of_t (t0::t1::ts));
      ]
    | TypeAlias { ta_name; ta_tparams; ta_type } -> [
        "name", json_of_symbol ta_name;
        "typeParams", json_of_type_params ta_tparams;
        "body", Option.value_map ~f:json_of_t ~default:JSON_Null ta_type
      ]
    | TypeOf (path, name) -> [
        "path", JSON_Array (Core_list.map ~f:(fun x -> JSON_String x) path);
        "name", JSON_String name;
      ]
    | Module name -> [
        "name", json_of_symbol name;
      ]
    | ClassDecl (name, tparams) -> [
        "name", json_of_symbol name;
        "typeParams", json_of_type_params tparams;
      ]
    | InterfaceDecl (name, tparams) -> [
        "name", json_of_symbol name;
        "typeParams", json_of_type_params tparams;
      ]
    | Utility u -> json_of_utility u
    | Mu (i, t) -> [
        "mu_var", int_ i;
        "type", json_of_t t;
      ]
    )
  )

  and json_of_tvar (RVar i) = Hh_json.(["id", int_ i])

  and json_of_fun_t { fun_params; fun_rest_param; fun_return; fun_type_params } =
    Hh_json.(
      [
        "typeParams", json_of_type_params fun_type_params;
      ] @ [
        "paramTypes",
        JSON_Array (Core_list.map ~f:(fun (_, t, _) -> json_of_t t) fun_params)
      ] @ [
        "paramNames", JSON_Array (List.rev_map (function
          | (Some n, _, _) -> JSON_String n
          | (None, _, _) -> JSON_String "_"
          ) fun_params);
      ] @ [
        "restParam", (match fun_rest_param with
        | None -> JSON_Null
        | Some (name, t) -> JSON_Object (
          [
            "restParamType", json_of_t t;
          ] @ (match name with
            | None -> []
            | Some name -> ["restParamName", JSON_String name])));
        "returnType", json_of_t fun_return;
      ]
    )

  and json_of_type_params ps = Hh_json.(
    match ps with
    | None -> JSON_Null
    | Some tparams -> JSON_Array (Core_list.map ~f:json_of_typeparam tparams)
  )

  and json_of_targs targs_opt = Hh_json.(
    match targs_opt with
    | Some targs -> [ "typeArgs", JSON_Array (Core_list.map ~f:json_of_t targs) ]
    | None -> []
  )

  and json_of_typeparam  {
    tp_name: string;
    tp_bound: t option;
    tp_polarity: polarity;
    tp_default: t option;
  } = Hh_json.(
    JSON_Object ([
      "name", JSON_String tp_name;
      "bound", Option.value_map tp_bound ~f:json_of_t ~default:JSON_Null;
      "polarity", json_of_polarity tp_polarity;
    ] @
    Option.value_map tp_default ~default:[] ~f:(fun t -> ["default", json_of_t t])
    )
  )

  and json_of_polarity polarity =
    Hh_json.JSON_String (string_of_polarity polarity)

  and json_of_prop prop = Hh_json.(
    JSON_Object (match prop with
    | NamedProp (name, p) -> [
        "kind", JSON_String "NamedProp";
        "prop", JSON_Object [
            "name", JSON_String name;
            "prop", json_of_named_prop p;
          ];
      ]
    | IndexProp d -> [
        "kind", JSON_String "IndexProp";
        "prop", json_of_dict d;
      ]
    | CallProp ft -> [
        "kind", JSON_String "NamedProp";
        "prop", JSON_Object (json_of_fun_t ft);
      ]
    | SpreadProp t -> [
        "kind", JSON_String "SpreadProp";
        "prop", json_of_t t;
      ]
    )
  )

  and json_of_dict { dict_polarity; dict_name; dict_key; dict_value } = Hh_json.(
    JSON_Object [
      "polarity", json_of_polarity dict_polarity;
      "name", JSON_String (Option.value dict_name ~default:"_");
      "key", json_of_t dict_key;
      "value", json_of_t dict_value;
    ]
  )

  and json_of_named_prop p = Hh_json.(JSON_Object (
    match p with
    | Field (t, { fld_polarity; fld_optional }) -> [
        "kind", JSON_String "field";
        "type", json_of_t t;
        "polarity", json_of_polarity fld_polarity;
        "optional", JSON_Bool fld_optional;
      ]
    | Method t -> [
        "kind", JSON_String "Method";
        "funtype", JSON_Object (json_of_fun_t t);
      ]
    | Get t -> [
        "kind", JSON_String "Get";
        "type", json_of_t t;
      ]
    | Set t -> [
        "kind", JSON_String "Set";
        "type", json_of_t t;
      ]
  ))

  and json_of_utility u = Hh_json.(
    let ctor = Ty.string_of_utility_ctor u in
    let ts = json_of_targs (Ty.types_of_utility u) in
    ("kind", JSON_String ctor) :: ts
  )

  in fun t -> json_of_t t
