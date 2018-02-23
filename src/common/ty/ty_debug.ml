(**
 * Copyright (c) 2013-present, Facebook, Inc.
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

let rec dump_opt (f: 'a -> string) (o: 'a option) = match o with
  | Some t -> f t
  | None -> ""

and dump_list : 'a . ('a -> string) -> ?sep:string -> 'a list -> string =
  fun f ?(sep=", ") ls ->
    List.map f ls |> String.concat sep

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
  spf "%s%s%s" (dump_polarity tp_polarity) tp_name
    (dump_bound ~depth tp_bound)

and dump_type_params ~depth = function
  | Some [] -> ""
  | Some ps -> spf "<%s>" (dump_list (dump_type_param ~depth) ps)
  | _ -> ""

and dump_fun_t ~depth { fun_params; fun_rest_param; fun_return; fun_type_params } =
  spf "Fun(%s(%s%s) => %s)"
    (dump_type_params ~depth fun_type_params)
    (dump_list (dump_param ~depth) fun_params)
    (dump_rest_params ~depth  fun_rest_param)
    (dump_t ~depth fun_return)

and dump_field { fld_polarity; fld_optional } =
  spf "%s%s" (dump_polarity fld_polarity) (if fld_optional then "?" else "")

and dump_prop ~depth = function
  | NamedProp (s, p) -> spf "%s: %s" s (dump_named_prop ~depth p)
  | IndexProp d -> dump_dict ~depth d
  | CallProp f -> dump_fun_t ~depth f

and dump_named_prop ~depth = function
  | Field (t, field) -> spf "%s: %s" (dump_t ~depth t) (dump_field field)
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

and dump_obj ~depth { obj_exact; obj_props } =
  if obj_exact
    then spf "{|%s|}" (dump_list (dump_prop ~depth) obj_props)
    else spf "{%s}"   (dump_list (dump_prop ~depth) obj_props)

and dump_generics ~depth = function
  | Some ts -> "<" ^ dump_list (dump_t ~depth) ts ^ ">"
  | _ -> ""

and dump_tvar (TVar i) = spf "T_%d" i

and dump_t ?(depth = 10) t =
  if depth < 0 then "..." else
  let depth = depth - 1 in
  match t with
  | ID v -> dump_tvar v
  | Generic (s, st, ts) ->
    spf "Generic(%s, struct= %b, params=%s)" s st (dump_generics ~depth ts)
  | Any -> "Any"
  | AnyObj -> "AnyObj"
  | AnyFun -> "AnyFun"
  | Top -> "Top"
  | Bot -> "Bot"
  | Void -> "Void"
  | Null -> "Null"
  | Num -> "Num"
  | NumLit s -> spf "\"%s\"" s
  | Str -> "Str"
  | StrLit s -> spf "\"%s\"" s
  | Bool -> "Bool"
  | BoolLit b ->  spf "\"%b\"" b
  | Fun f -> dump_fun_t ~depth f
  | Obj o -> dump_obj ~depth o
  | Arr t -> spf "Array<%s>" (dump_t ~depth t)
  | This -> "this"
  | Tup ts ->
    spf "Tup(%s)" (dump_list (dump_t ~depth) ~sep:"," ts)
  | Union (t1,t2,ts) ->
    spf "Union(%s)" (dump_list (dump_t ~depth) ~sep:", " (ListUtils.first_n 10 (t1::t2::ts)))
  | Inter (t1,t2,ts) ->
    spf "Inter(%s)" (dump_list (dump_t ~depth) ~sep:", " (t1::t2::ts))
  | TypeAlias { ta_name; ta_imported=None; ta_tparams; ta_type } ->
    spf "TypeAlias(%s, params=[%s], body=%s)"
      ta_name
      (dump_type_params ~depth ta_tparams)
      (Option.value_map ta_type ~default:"" ~f:(fun t -> cut_off (dump_t ~depth t)))
  | TypeAlias { ta_name; ta_imported=Some file; _ } ->
    spf "Import(%s, %s)" ta_name file
  | TypeOf n ->
    spf "Typeof(%s)" n
  | Exists -> "*"
  | Class (n, s, ps) ->
    spf "%s(%s, params= %s)" (if s then "Interface" else "Class") n
      (dump_type_params ~depth ps)
  | Mu (v, t) -> spf "Mu(%s, %s)" (dump_tvar v) (dump_t ~depth t)

let dump_binding (v, ty) =
  Utils_js.spf "type %s = %s" (dump_tvar v) (dump_t ty)

let dump_env_t s = List.map dump_binding s |> String.concat "\n"

let string_of_polarity = function
  | Negative -> "Negative"
  | Neutral -> "Neutral"
  | Positive -> "Positive"

let string_of_ctor = function
  | ID _ -> "ID"
  | Generic _ -> "Generic"
  | Any -> "Any"
  | AnyObj -> "AnyObj"
  | AnyFun -> "AnyFun"
  | Top -> "Top"
  | Bot -> "Bot"
  | Void -> "Void"
  | Null -> "Null"
  | Num -> "Num"
  | Str -> "Str"
  | Bool -> "Bool"
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
  | Class _ -> "Class"
  | This -> "This"
  | Exists -> "Exists"
  | Mu _ -> "Mu"

let rec json_of_t t = Hh_json.(
  JSON_Object ([
    "kind", JSON_String (string_of_ctor t)
  ] @
  match t with
  | ID v -> json_of_tvar v
  | Generic (s, str, targs_opt) -> (
    match targs_opt with
      | Some targs -> [ "typeArgs", JSON_Array (List.map json_of_t targs) ]
      | None -> []
    ) @ [
      "type", JSON_String s;
      "structural", JSON_Bool str;
    ]
  | Any | AnyObj | AnyFun
  | Top | Bot
  | Void | Null
  | Num | Str | Bool -> []
  | NumLit s
  | StrLit s -> [
      "literal", JSON_String s
    ]
  | BoolLit b -> [
      "literal", JSON_Bool b
    ]
  | Fun f -> json_of_fun_t f
  | Obj { obj_exact; obj_props } -> [
      "exact", JSON_Bool obj_exact;
      "props", JSON_Array (List.map json_of_prop obj_props);
    ]
  | Arr t -> [
      "type", json_of_t t;
    ]
  | Tup ts -> [
      "types", JSON_Array (List.map json_of_t ts);
    ]
  | Union (t0,t1,ts) -> [
      "types", JSON_Array (List.map json_of_t (t0::t1::ts));
    ]
  | Inter (t0,t1,ts) -> [
      "types", JSON_Array (List.map json_of_t (t0::t1::ts));
    ]
  | TypeAlias { ta_name; ta_imported; ta_tparams; ta_type } -> [
      "name", JSON_String ta_name;
      "imported", (
        match ta_imported with
        | Some name -> JSON_String name
        | _ -> JSON_Null
      );
      "typeParams", json_of_type_params ta_tparams;
      "body", Option.value_map ~f:json_of_t ~default:JSON_Null ta_type
    ]
  | TypeOf name -> [
      "name", JSON_String name;
    ]
  | Class (name, structural, tparams) -> [
      "name", JSON_String name;
      "structural", JSON_Bool structural;
      "typeParams", json_of_type_params tparams;
    ]
  | This
  | Exists -> []
  | Mu (v, t) ->
    json_of_tvar v @ [
      "type", json_of_t t;
    ]
  )
)

and json_of_tvar (TVar id) = Hh_json.([
  "id", int_ id
])

and json_of_fun_t { fun_params; fun_rest_param; fun_return; fun_type_params } =
  Hh_json.(
    [
      "typeParams", json_of_type_params fun_type_params;
    ] @ [
      "paramTypes",
      JSON_Array (List.map (fun (_, t, _) -> json_of_t t) fun_params)
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
  | Some tparams -> JSON_Array (List.map json_of_typeparam tparams)
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
